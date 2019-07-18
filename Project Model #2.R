?read_csv()
dataframe <- data.frame(read.csv('https://raw.githubusercontent.com/neilrankinza/ix_cape_town_2019_project/master/data/raw/teaching_training_data.csv'))
summary(dataframe)

head(dataframe)
View(dataframe)

#new <- na.approx.default(dataframe)
#lapply(trainer_data, function(working) any(is.na(working)))
train_1 = sample(c(T, F), nrow(dataframe), prob = c(0.8, 0.2), replace = TRUE)




trainer_data <- dataframe[train_1,]
tester_data <- dataframe[-train_1,]
trainer_data
tester_data

install.packages('caret')
library(caret)
install.packages('tidyverse')
library(tidyverse)
#install.packages("randomForest")
#library(randomForest)
trainer_data$working <- as.factor(trainer_data$working)

filtereddata <- filter(trainer_data, survey_num == 1)
install.packages('lubridate')
library(lubridate)
df <- filtereddata %>%
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey))
#new_model <- train(working~ ., data=trainer_data, method='glm', trControl = trControl, na.action=na.roughfix)
new_model <- train(working~ age + gender + province + volunteer + anygrant+givemoney_yes, data=df,method = 'glm', family=binomial(), na.action=na.omit)
summary(new_model)
