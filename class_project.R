library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)
library(skimr)
library(RANN)
library(ISLR)
library(pastecs)
library(lattice)
library(RWeka)

# Read in the csv into a dataframe
df <- data.frame(read.csv('data/raw/teaching_training_data.csv'))
df_cft <- data.frame(read.csv('data/raw/teaching_training_data_cft.csv'))
df_com <- data.frame(read.csv('data/raw/teaching_training_data_com.csv'))
df_grit <- data.frame(read.csv('data/raw/teaching_training_data_grit.csv'))
df_num <- data.frame(read.csv('data/raw/teaching_training_data_num.csv'))
df_opt <- data.frame(read.csv('data/raw/teaching_training_data_opt.csv'))

# Find all NA values per column
# colSums(is.na(df))

# CLEAN------------------------------------------------------------------------
# Filter out the first survey data
fsd <- filter(df, survey_num==1)

colSums(is.na(fsd))
# Filter out the "irrelevant" columns
fsd <- select(fsd, -job_start_date, -job_leave_date, -company_size, -monthly_pay, -survey_num, -province)
# Filter out error rows (repeated IDs)
fsd <- distinct(fsd, unid, .keep_all = TRUE)

colSums(is.na(df))
colSums(is.na(fsd))

# Remove rows with NA
# fsd <- fsd[!is.na(fsd$volunteer),]
fsd <- fsd[!is.na(fsd$gender),]
fsd <- fsd[!is.na(fsd$dob),]


glimpse(fsd)

# Create "age" column based on date of birth
fsd <- fsd %>%
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey))

# Drop columns
fsd <- select(fsd, -age_at_survey, -dob, -survey_date_month, -financial_situation_now, -financial_situation_5years)

# Cleaning function
helper_function <- function(file_name) {
  file_name %>%
    select(2:3) %>%
    distinct(unid, .keep_all=TRUE)
}
df_cft <- helper_function(df_cft)
df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)

# JOIN all the dataframe
fsd <- left_join(fsd, df_cft, by="unid")
fsd <- left_join(fsd, df_com, by="unid")
fsd <- left_join(fsd, df_grit, by="unid")
fsd <- left_join(fsd, df_num, by="unid")
fsd <- left_join(fsd, df_opt, by="unid")

colSums(is.na(fsd))

fsd <- mutate(fsd,
              # Male=1, Female=0
              gender = factor(ifelse(gender=="Male", 1, 0)),
              
              # Yes=1, No=0
              volunteer = factor(ifelse(volunteer=="Yes", 1, 0)),
              leadershiprole = factor(ifelse(leadershiprole=="Yes", 1, 0)),
              
              working = factor(ifelse(working==TRUE, 1, 0)),
              
              # Clean data to numeric
              peoplelive = parse_number(as.character(peoplelive)),
              peoplelive_15plus= parse_number(as.character(peoplelive_15plus)),
              numchildren = parse_number(as.character(numchildren)),
              numearnincome = parse_number(as.character(numearnincome))
)

# Change NA to be a factor
fsd$volunteer <- addNA(fsd$volunteer)
levels(fsd$volunteer) <- c(levels(fsd$volunteer), 2)
fsd$leadershiprole <- addNA(fsd$leadershiprole)
levels(fsd$leadershiprole) <- c(levels(fsd$leadershiprole), 2)

fsd <- replace_na(fsd, list(volunteer = 2, leadershiprole = 2))

glimpse(fsd)
glimpse(df)

# # Median imputing NAs
# # fsd$working <- as.factor(make.names(fsd$working))
# preProcValues <- preProcess(fsd, method = c("medianImpute"))
# fsd_processed <- predict(preProcValues, fsd)
# 
# colSums(is.na(fsd_processed))
# 
# glimpse(fsd)
# glimpse(fsd_processed)

# # Filter out responses with NAs in these columns
# fsd <- filter(fsd, !(is.na(volunteer) & 
#                        is.na(leadershiprole) & 
#                        is.na(peoplelive) & 
#                        is.na(peoplelive_15plus) & 
#                        is.na(numchildren) &
#                        is.na(numearnincome) &
#                        is.na(financial_situation_now) & 
#                        is.na(financial_situation_5years) & 
#                        is.na(givemoney_yes)))
# 
# # Create new column "Responsive" based on NAs
# fsd <- mutate(fsd,
#               responsive = factor(ifelse(!(is.na(volunteer) & 
#                                              is.na(leadershiprole) & 
#                                              is.na(peoplelive) & 
#                                              is.na(peoplelive_15plus) & 
#                                              is.na(numchildren) &
#                                              is.na(numearnincome) &
#                                              is.na(financial_situation_now) & 
#                                              is.na(financial_situation_5years) & 
#                                              is.na(givemoney_yes)), 1, 0))
# )
# 
# # Convert "0: I live alone" to just "0"
# fsd[ ((fsd$peoplelive=="0: I live alone") & (fsd$numearnincome=="0: I live alone")), ]$peoplelive <- 0
# fsd[ ((fsd$peoplelive==0) & (fsd$numearnincome=="0: I live alone")), ]$numearnincome <- 0
# fsd[ ((fsd$peoplelive=="0: I live alone") & (fsd$numearnincome==0)), ]$peoplelive <- 0
# fsd <- filter(fsd, !(peoplelive=="0: I live alone" | numearnincome=="0: I live alone"))
# 
# fsd[ (fsd$peoplelive=="More than 15"), ]$peoplelive <- 16
# fsd[ (fsd$peoplelive_15plus=="More than 15"), ]$peoplelive_15plus <- 16
# fsd[ (fsd$peoplelive=="15 or more"), ]$peoplelive <- 16
# fsd[ (fsd$peoplelive_15plus=="15 or more"), ]$peoplelive_15plus <- 16
# 
# fsd[ (!(fsd$peoplelive < fsd$peoplelive_15plus)), ]
# 
# # Replace all NAs accordingly
# fsd[is.na(fsd$anygrant),]$anygrant <- FALSE
# fsd[is.na(fsd$anyhhincome),]$anyhhincome <- FALSE
# fsd[is.na(fsd$volunteer),]$volunteer <- "No"
# fsd[is.na(fsd$peoplelive_15plus),]$peoplelive_15plus <- 0
# fsd[is.na(fsd$peoplelive),]$peoplelive <- 0
# fsd[is.na(fsd$leadershiprole),]$leadershiprole <- "No"
# fsd[is.na(fsd$numchildren),]$numchildren <- 0
# fsd[is.na(fsd$numearnincome),]$numearnincome <- 0
# 
# # Remove "15 or More"
# fsd <- filter(fsd, !(peoplelive=="More than 15" | peoplelive_15plus=="More than 15" | numearnincome=="More than 15"))
# fsd <- filter(fsd, !(peoplelive=="15 or more" | peoplelive_15plus=="15 or more" | numearnincome=="15 or more"))
# 
# # Remove all remaining rows containing NAs
# colSums(is.na(fsd))
# fsd <- fsd[!is.na(fsd$gender),]
# fsd <- fsd[!is.na(fsd$dob),]
# # fsd <- na.omit(fsd, cols="gender")
# colSums(is.na(fsd))
# 
# 
# glimpse(fsd)
#------------------------------------------------------------------------------



# VISUALIZATION----------------------------------------------------------------
# Histogram of female working vs male working
ggplot(fsd, aes(x=working, fill=gender)) + 
  geom_histogram(stat="count")

# Histogram of age with working divided by gender
ggplot(fsd, aes(x=age, fill=working)) +
  geom_histogram(binwidth = 1) +
  facet_grid(gender ~ .)
#------------------------------------------------------------------------------



# MODELING---------------------------------------------------------------------
# fsd_processed = select(fsd_processed, gender, working, volunteer, leadershiprole, age, peoplelive, peoplelive_15plus)
# 
# fsd_processed <- mutate(fsd_processed,
#                 # Male=1, Female=0
#                 gender = factor(ifelse(gender=="Male", 1, 0)),
# 
#                 # Yes=1, No=0
#                 volunteer = factor(ifelse(volunteer=="Yes", 1, 0)),
#                 leadershiprole = factor(ifelse(leadershiprole=="Yes", 1, 0)),
# 
#                 working = factor(ifelse(working, 1, 0)),
# 
#                 peoplelive = as.numeric(levels(peoplelive))[peoplelive],
#                 peoplelive_15plus = as.numeric(levels(peoplelive_15plus))[peoplelive_15plus]
# )
# 
# 
# 
# glimpse(fsd)
# 
# colSums(is.na(fsd))
# 
# dummies_model <- dummyVars(working ~ . , data=fsd)
# 
# fsd_working <- fsd$working
# fsd_mat <- predict(dummies_model, newdata = fsd)
# 
# fsd <- data.frame(fsd_mat)
# 
# head(fsd)
# 
# preProcess_scale_model <- preProcess(fsd[c('age', 'peoplelive', 'peoplelive_15plus')], method=c('center', 'scale'))
# 
# fsd[c('age', 'peoplelive', 'peoplelive_15plus')] <- predict(preProcess_scale_model, newdata = fsd[c('age', 'peoplelive', 'peoplelive_15plus')])
# 
# fsd$working <- fsd_working
# 
# 
# set.seed(100)

# fsd_processed <- select(fsd_processed, -volunteer, -leadershiprole, -anygrant, -anyhhincome, -givemoney_yes)
# fsd_processed <- select(fsd_processed, -anygrant, -anyhhincome, -givemoney_yes)

# fsd <- select(fsd, -volunteer, -leadershiprole, -anygrant, -anyhhincome, -givemoney_yes)
fsd <- select(fsd, -anygrant, -anyhhincome, -givemoney_yes)

colSums(is.na(fsd))
glimpse(fsd)

# CATEGORICAL VARIABLES/ONE-HOT ENCODED----------------------------------------
dummies_model <- dummyVars(working ~ . , data=fsd)
fsd_working <- fsd$working
fsd_mat <- predict(dummies_model, newdata = fsd)
fsd <- data.frame(fsd_mat)
head(fsd)
preProcess_scale_model <- preProcess(fsd[c('age', 'peoplelive', 'peoplelive_15plus')], method=c('center', 'scale'))
fsd[c('age', 'peoplelive', 'peoplelive_15plus')] <- predict(preProcess_scale_model, newdata = fsd[c('age', 'peoplelive', 'peoplelive_15plus')])
fsd$working <- fsd_working

colSums(is.na(fsd))

# Dropping columns with over 50% NAs
fsd <- select(fsd, -num_score, -com_score, -peoplelive, -peoplelive_15plus, -numearnincome)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(fsd$working, p=0.8, list=FALSE)
# Step 2: Create the training  dataset
trainData <- fsd[trainRowNumbers,]
# Step 3: Create the test dataset
testData <- fsd[-trainRowNumbers,]

# Median imputing NAs separately
# fsd$working <- as.factor(make.names(fsd$working))
preProcValues <- preProcess(trainData, method = c("medianImpute"))
trainData <- predict(preProcValues, trainData)
colSums(is.na(trainData))

preProcValues <- preProcess(testData, method = c("medianImpute"))
testData <- predict(preProcValues, testData)
colSums(is.na(testData))

## Defining the training controls for multiple models
trControl <- trainControl(method = "cv",
                          number = 5,
                          verboseIter = TRUE)

# Model weights
model_weights <- ifelse(trainData$working == 0,
                        (1/table(trainData$working)[1]) * 0.5,
                        (1/table(trainData$working)[2]) * 0.5)

# Model RANGER with weights
model_ranger_w <- train(working ~ ., trainData,
                        method='ranger',
                        trControl=trControl,
                        weights = model_weights,
                        metric="Accuracy")
testData$pred_ranger_w <- predict(object=model_ranger_w, testData)
confusionMatrix(testData$pred_ranger_w, testData$working)

## MODELS THAT ARE ALRIGHT
# # Model XGBTREE with weights
# model_xgbTree <- train(working ~ ., trainData,
#                       method='xgbTree',
#                       trControl=trControl,
#                       weights = model_weights,
#                       metric="Accuracy")
# testData$pred_xgbTree <- predict(object=model_xgbTree, testData)
# confusionMatrix(testData$pred_xgbTree, testData$working)
# 
# # Model GBM with weights
# model_gbm <- train(working ~ ., trainData,
#                        method='gbm',
#                        trControl=trControl,
#                        weights = model_weights,
#                        metric="Accuracy")
# testData$pred_gbm <- predict(object=model_gbm, testData)
# confusionMatrix(testData$pred_gbm, testData$working)
# 
# # Model BLACKBOOST with weights
# model_blackboost <- train(working ~ ., trainData,
#                    method='blackboost',
#                    trControl=trControl,
#                    weights = model_weights,
#                    metric="Accuracy")
# testData$pred_blackboost <- predict(object=model_blackboost, testData)
# confusionMatrix(testData$pred_blackboost, testData$working)
# 
# # Model KNN with weights
# model_knn <- train(working ~ ., trainData,
#                           method='knn',
#                           trControl=trControl,
#                           weights = model_weights,
#                           metric="Accuracy")
# testData$pred_knn <- predict(object=model_knn, testData)
# confusionMatrix(testData$pred_knn, testData$working)
# 
# # Model hdda with weights
# model_hdda <- train(working ~ ., trainData,
#                    method='hdda',
#                    trControl=trControl,
#                    weights = model_weights,
#                    metric="Accuracy")
# testData$pred_hdda <- predict(object=model_hdda, testData)
# confusionMatrix(testData$pred_hdda, testData$working)

# MODELS THAT DIDN'T REALLY WORK-----------------------------------------------
# # Model RANGER
# model_ranger <- train(working ~ ., trainData,
#                       method='ranger',
#                       trControl=trControl,
#                       metric="Accuracy")
# 
# testData$pred_ranger <- predict(object=model_ranger, testData)
# confusionMatrix(testData$pred_ranger, testData$working)
# 
# # Model XGBTREE
# model_xgbTree <- train(working ~ ., trainData,
#                       method='xgbTree',
#                       trControl=trControl,
#                       metric="Accuracy")
# 
# testData$pred_xgbTree <- predict(object=model_xgbTree, testData)
# confusionMatrix(testData$pred_xgbTree, testData$working)



