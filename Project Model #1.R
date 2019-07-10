?read_csv()
dataframe <- data.frame(read_csv('https://raw.githubusercontent.com/neilrankinza/ix_cape_town_2019_project/master/data/raw/teaching_training_data.csv'))
summary(dataframe)
head(dataframe)

#new <- na.approx.default(dataframe)
#lapply(trainer_data, function(working) any(is.na(working)))
train_1 = sample(c(T, F), nrow(dataframe), prob = c(0.8, 0.2), replace = TRUE)




trainer_data <- dataframe[train_1,]
tester_data <- dataframe[-train_1,]
trainer_data
tester_data


install.packages("randomForest")
library(randomForest)
trainer_data$working <- as.factor(trainer_data$working)
#new_model <- train(working~ ., data=trainer_data, method='glm', trControl = trControl, na.action=na.roughfix)
new_model <- train(working~ ., data=trainer_data, method='glm', trControl = trControl, na.action=na.omit)
rmse(cars_test$dist, predict(fit, cars_test))
