library(readxl)
library(tidyverse)
library(lubridate)
library(reshape2)

#### load data ####
#temp <- tempfile(fileext = ".xlsx")
#download.file("https://jcudata.s3-ap-southeast-2.amazonaws.com/ED_Patients_Data.xlsx",temp)
# could not open zip file of xlsx
temp = "ED_patients_Data.xlsx"
data <- as.data.frame(read_excel(temp))

# review missing values
str(data)
missing <- as.data.frame(map(data, ~sum(is.na(.))))
# write.csv(missing, "missing.csv")

#### remove variables with no data ####
data$MEDICATION_USED_STEROIDS_DT_TM <- NULL
data$MEDICATION_USED_IMMUNO_DT_TM <- NULL 
data$MEDICATION_USED_ANTIB_DT_TM <- NULL
data$MEDICATION_USED_ANTINF_DT_TM <- NULL 
data$MEDICATION_USED_OTHER_DT_TM <- NULL 

#### convert datatypes ####

# factors
data$PATIENT_ID <- as.factor(data$PATIENT_ID)
data$GENDER <- as.factor(data$GENDER)
data$HOSPITAL <- as.factor(data$HOSPITAL)
data$TRIAGE_CATEGORY <- as.factor(data$TRIAGE_CATEGORY)
data$MODEL_OF_CARE <- as.factor(data$MODEL_OF_CARE)
data$AVPU_1 <- as.factor(data$AVPU_1)
data$AVPU_2 <- as.factor(data$AVPU_2)

# datetimes
data$TRIAGE_DT_TM <- as_datetime(data$TRIAGE_DT_TM, tz = "UTC", format = NULL)
data$AVPU_1_DT_TM <- as_datetime(data$AVPU_1_DT_TM, tz = "UTC", format = NULL)
data$AVPU_2_DT_TM <- as_datetime(data$AVPU_2_DT_TM, tz = "UTC", format = NULL)
data$GCS_1_DT_TM <- as_datetime(data$GCS_1_DT_TM, tz = "UTC", format = NULL)
data$GCS_2_DT_TM <- as_datetime(data$GCS_2_DT_TM, tz = "UTC", format = NULL)
data$RR_1_DT_TM <- as_datetime(data$RR_1_DT_TM, tz = "UTC", format = NULL)
data$RR_2_DT_TM <- as_datetime(data$RR_2_DT_TM, tz = "UTC", format = NULL)
data$O2SATS_1_DT_TM <- as_datetime(data$O2SATS_1_DT_TM, tz = "UTC", format = NULL)
data$O2SATS_2_DT_TM <- as_datetime(data$O2SATS_2_DT_TM, tz = "UTC", format = NULL)
data$FIO2_1_DT_TM <- as_datetime(data$FIO2_1_DT_TM, tz = "UTC", format = NULL)
data$FIO2_2_DT_TM <- as_datetime(data$FIO2_2_DT_TM, tz = "UTC", format = NULL)
data$PULSE_1_DT_TM <- as_datetime(data$PULSE_1_DT_TM, tz = "UTC", format = NULL)
data$PULSE_2_DT_TM <- as_datetime(data$PULSE_2_DT_TM, tz = "UTC", format = NULL)
data$HEART_RATE_1_DT_TM <- as_datetime(data$HEART_RATE_1_DT_TM, tz = "UTC", format = NULL)
data$HEART_RATE_2_DT_TM <- as_datetime(data$HEART_RATE_2_DT_TM, tz = "UTC", format = NULL)
data$SBP_1_DT_TM <- as_datetime(data$SBP_1_DT_TM, tz = "UTC", format = NULL)
data$SBP_2_DT_TM <- as_datetime(data$SBP_2_DT_TM, tz = "UTC", format = NULL)
data$DBP_1_DT_TM <- as_datetime(data$DBP_1_DT_TM, tz = "UTC", format = NULL)
data$DBP_2_DT_TM <- as_datetime(data$DBP_2_DT_TM, tz = "UTC", format = NULL)
data$MAP_1_DT_TM <- as_datetime(data$MAP_1_DT_TM, tz = "UTC", format = NULL)
data$MAP_2_DT_TM <- as_datetime(data$MAP_2_DT_TM, tz = "UTC", format = NULL)
data$TEMP_TYMP_1_DT_TM <- as_datetime(data$TEMP_TYMP_1_DT_TM, tz = "UTC", format = NULL)
data$TEMP_TYMP_2_DT_TM <- as_datetime(data$TEMP_TYMP_2_DT_TM, tz = "UTC", format = NULL)
data$TEMP_ORAL_1_DT_TM <- as_datetime(data$TEMP_ORAL_1_DT_TM, tz = "UTC", format = NULL)
data$TEMP_ORAL_2_DT_TM <- as_datetime(data$TEMP_ORAL_2_DT_TM, tz = "UTC", format = NULL)

# binary
data$SMOKING_STATUS <- as.logical(data$SMOKING_STATUS)

# Recode Pregnancy Status when it is = to 2
data$PREGNANCY_STATUS <- recode(data$PREGNANCY_STATUS, `2` = 1)
# convert Pregnancy Status to binary
data$PREGNANCY_STATUS <- as.logical(data$PREGNANCY_STATUS)


# create uniqueID
data <- data %>% mutate(ID = paste(PATIENT_ID, TRIAGE_DT_TM, sep = '_'))
# check distinct
n_distinct(data$ID, na.rm = FALSE)

# write processed data to new file
data.table::fwrite(data, "processedData.csv", row.names = F, sep = ",")


# split SNOMED codes
#data$SNOMED <- str_split(data$PATIENT_PROBLEM_HISTORY, "\\|")


#### pre-processing tasks todo ####

# one hot encoding of SNOMED codes





