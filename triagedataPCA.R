install.packages('aod')
install.packages('ggplot2')
install.packages('ISLR')
install.packages('dplyr')
install.packages('car')
triageData <- read.csv("G:/David/JCU/MA5854/ED_Patients_Data.csv", header = T, sep = ",")

sapply(triageData, class)
str(triageData)
summary(triageData)

#convert to numeric
triageData[] <- lapply(triageData, as.numeric)
#convert NA to 0
triageData[is.na(triageData)] = 0
str(triageData)

#ED_data <- triageData[-c(1,5,69,71,73,75,77)]
#install.packages("openxlsx")
#library(openxlsx)
#write.xlsx(ED_data,"G:/David/JCU/MA5854/ED_data.xlsx") 

set.seed(100) # set seed to replicate results
trainingIndex <- sample(1:nrow(triageData), 0.8*nrow(triageData)) # indices for 80% training data
train_triage <- triageData[trainingIndex, ] # training data
test_triage <- triageData[-trainingIndex, ] # test data

#remove the dependent and identifier variables
ED_triage <- triageData[-c(1,5,69,71,73,75,77)]

#divide the new data
pca.train <- ED_triage[1:nrow(train_triage),]
pca.test <- ED_triage[-(1:nrow(train_triage)),]

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)
summary(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation
prin_comp$rotation[,1]

dim(prin_comp$x)

biplot(prin_comp, scale = 0)

prin_comp$rotation[1:74,1:10]

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:20]  


#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:40]

sum(prop_varex[1:40])

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.ED_data <- data.frame(TRIAGE_CATEGORY = train_triage$TRIAGE_CATEGORY, prin_comp$x)

#we are interested in first 30 PCAs
train.ED_data <- train.ED_data[,1:40]

#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model <- rpart(TRIAGE_CATEGORY ~ .,data = train.ED_data, method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 45 components
test.data <- test.data[,1:40]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
#rpart.prediction


