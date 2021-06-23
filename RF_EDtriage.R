install.packages("disk.frame", repo="https://cran.rstudio.com/")

library(forcats)
install.packages('dplyr')
library(MASS)
library(randomForest)
library(caret)
library(reticulate)
library(dplyr)
sagemaker <- import('sagemaker')

session <- sagemaker$Session()
bucket <- session$default_bucket()
#inputData = read.csv(file = 'chunk4-vehicles.csv', header = TRUE, sep = ',')
#summary(inputData)

load(file = "processedData4.RData")
#triageData <- data
#sapply(triageData, class)
str(data)
summary(data)

ED_data <- data[, -c(1, 4, 7, 9,10, 64)]
str(ED_data)

ED_data$GENDER <- as.numeric(ED_data$GENDER)
ED_data$MODEL_OF_CARE <- as.numeric(ED_data$MODEL_OF_CARE)


# create Age Group variable for mean imputation
ED_data$AGE_GROUP <- data$AGE %/% 10
ED_data$RR_grouped <- (data$RR_1 < 10)
ED_data$BP_grouped <- (data$SBP_1 < 80)
ED_data$HR_grouped <- (data$HEART_RATE_1 < 50 | data$HEART_RATE_1 > 150)
ED_data$GCS_critical <- (data$GCS_1 < 9 )
ED_data$GCS_unresponsive <- (data$GCS_1 > 9 & data$GCS_1 < 13)
ED_data[,c(1, 93,94, 95, 96, 97)] <- lapply(ED_data[,c(1, 93,94, 95, 96, 97)], as.numeric)
str(ED_data)

#triageData <- triageData[-c(1,2, 4,7)]#remove dependent and descriptor variables
# function for mean imputation
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# function for replacing with zero
replace.zero <- function(x) replace(x, is.na(x), 0)

# subset first observations
Numeric_cols <- colnames(ED_data %>% select_if(is.numeric))
First_obs_cols <- Numeric_cols[endsWith(Numeric_cols, '_1')]
Second_obs_cols <- Numeric_cols[endsWith(Numeric_cols, '_2')]
Body_measure_cols <- Numeric_cols[Numeric_cols %in% c("PATIENT_HEIGHT","PATIENT_WEIGHT","MODEL_OF_CARE")]
#Binary_cols <- Numeric_cols[Numeric_cols %in% c("PATIENT_HEIGHT","PATIENT_WEIGHT")]

# group data by GENDER and AGE GROUP then impute the group mean for missing values
ED_data = ED_data %>% group_by(AGE_GROUP , GENDER) %>%
  mutate_at(First_obs_cols, impute.mean) %>%  # initial observations
  #mutate_at(Second_obs_cols, impute.mean) %>%  # second observations
  mutate_at(Body_measure_cols, impute.mean) # body measurements
#mutate_at(Binary_cols, replace.zero)  # binary variables replace with mode (0 for all)
str(ED_data)


#### create reusable subsets of data ####
# create lists of variable types to use for indexing
Numeric_cols <- colnames(data %>% select_if(is.numeric)) 
# create drop list
Time_cols <- Numeric_cols[endsWith(Numeric_cols, 'DT_TM')] 
Second_obs_cols <- Numeric_cols[endsWith(Numeric_cols, '_2')]
drop_from_keras <- c(Time_cols, Second_obs_cols)
#drop_from_keras <- c(Time_cols) 
# create a subset of numeric values
Numeric_data <- data[,Numeric_cols]
# remove second observations and time observations
Numeric_data <- Numeric_data[, !colnames(Numeric_data) %in% drop_from_keras]
#response <- as.data.frame(data$TRIAGE_CATEGORY)
#Numeric_plus_response <- cbind(response, Numeric_data) 
Numeric_data[,1] <- lapply(Numeric_data[,1], as.factor)
str(Numeric_data)
summary(Numeric_data)


set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(Numeric_data), 0.7*nrow(Numeric_data)) # indices for 60% training data
trainingData <- Numeric_data[trainingIndex, ] # training data
testData <- Numeric_data[-trainingIndex, ] # test data

trainingData <- trainingData[, -c(32)]
names(trainingData)[names(trainingData) == "PRESENTING_GROUPMental Health"] <- "PRESENTING_GROUP_MentalHealth"
names(trainingData)[names(trainingData) == "PRESENTING_GROUPObstetrics and Gynaecology"] <- "PRESENTING_GROUP_Obstetrics_Gynaecology"
names(testData)[names(testData) == "PRESENTING_GROUPMental Health"] <- "PRESENTING_GROUP_MentalHealth"
names(testData)[names(testData) == "PRESENTING_GROUPObstetrics and Gynaecology"] <- "PRESENTING_GROUP_Obstetrics_Gynaecology"
names(trainingData)[names(trainingData) == "PRESENTING_GROUPENT,face"] <- "PRESENTING_GROUP_ENTface"
names(testData)[names(testData) == "PRESENTING_GROUPENT,face"] <- "PRESENTING_GROUP_ENTface"
summary(trainingData)
#trainingData_RF$zone_name <- levels(droplevels(trainingData_RF$zone_name))
summary(trainingData)
str(trainingData)
str(testData)
#summary(trainingData_RF)
#str(trainingData_RF)

set.seed(1234)# can be any number 
#trainingData_RF <- droplevels(trainingData_RF)
#trainingData_RF$zone_name <- droplevels(trainingData_RF$zone_name)
#rf1 <- randomForest(is_double_parked ~ ., trainingData[1:883459,], importance=TRUE)
#rf2 <- randomForest(zone_name ~ ., trainingData[883460:1766917,], importance=TRUE)
#rf.combined <- combine(rf1,rf2)
#rf.combined

rf.model1 <- randomForest(TRIAGE_CATEGORY ~., data = trainingData, na.action = na.omit, proximity = FALSE, importance = TRUE)
rf.model1

# Fine tuning parameters of Random Forest model
rf.model2 <- randomForest(TRIAGE_CATEGORY ~ ., data = trainingData, ntree = 500, mtry = 6, proximity = FALSE, importance = TRUE, na.action = na.omit)
rf.model2

trainset = trainingData
testset <- testData
predtrain = predict(rf.model2, trainset, type = "class")
table(predtrain, trainset$is_double_parked)

str(trainset)

trainset = trainingData
testset <- testData
predvalid <- predict(rf.model2, testset, type = "class")
mean(predvalid == testset$TRIAGE_CATEGORY)
table(predvalid, testset$TRIAGE_CATEGORY)

importance(rf.model2, descending)        
varImpPlot(rf.model2)  

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(is_double_parked ~ ., data = trainset, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, testset, type = "class")
  a[i-2] = mean(predValid == testset$is_double_parked)
}
 
a
 
plot(3:8,a)


