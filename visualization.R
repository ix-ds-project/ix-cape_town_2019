library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read in the csv into a dataframe
df <- data.frame(read.csv('data/raw/teaching_training_data.csv'))

# Filter out the "relevant" columns
clean_df <- select(df, X, survey_num, survey_date_month, gender, dob, working)

# Filter out the first survey data
first_survey_data <- filter(clean_df, survey_num==1)
# Clean up rows containing NA values
first_survey_data <- na.omit(first_survey_data)

# Check number of NAs per column
colSums(is.na(first_survey_data))

# Create "age" column based on date of birth
first_survey_data <- first_survey_data %>%
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey))

head(first_survey_data, n=20)

# Histogram of female working vs male working
ggplot(first_survey_data, aes(x=working, fill=gender)) + 
  geom_histogram(stat="count")

# Histogram of age with working divided by gender
ggplot(first_survey_data, aes(x=age, fill=working)) +
  geom_histogram(binwidth = 1) +
  facet_grid(gender ~ .)