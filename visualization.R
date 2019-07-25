library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read in the csv into a dataframe
df <- data.frame(read.csv('data/raw/teaching_training_data.csv'))

# Find all NA values per column
# colSums(is.na(df))

# CLEAN------------------------------------------------------------------------
# Filter out the first survey data
first_survey_data <- filter(df, survey_num==1)
# Filter out the "relevant" columns
first_survey_data <- select(first_survey_data, X, survey_date_month, gender, dob, working, volunteer, leadershiprole, peoplelive, peoplelive_15plus, numchildren, numearnincome, anygrant, anyhhincome)

# Replace all NAs accordingly
first_survey_data[is.na(first_survey_data$anygrant),]$anygrant <- FALSE
first_survey_data[is.na(first_survey_data$anyhhincome),]$anyhhincome <- FALSE
first_survey_data[is.na(first_survey_data$volunteer),]$volunteer <- "No"
first_survey_data[is.na(first_survey_data$peoplelive_15plus),]$peoplelive_15plus <- 0
first_survey_data[is.na(first_survey_data$peoplelive),]$peoplelive <- 0
first_survey_data[is.na(first_survey_data$leadershiprole),]$leadershiprole <- "No"
first_survey_data[is.na(first_survey_data$numchildren),]$numchildren <- 0
first_survey_data[is.na(first_survey_data$numearnincome),]$numearnincome <- 0

# Remove "0: I live alone"
first_survey_data[ ((first_survey_data$peoplelive=="0: I live alone") & (first_survey_data$numearnincome=="0: I live alone")), ]$peoplelive <- 0
first_survey_data[ ((first_survey_data$peoplelive==0) & (first_survey_data$numearnincome=="0: I live alone")), ]$numearnincome <- 0
first_survey_data[ ((first_survey_data$peoplelive=="0: I live alone") & (first_survey_data$numearnincome==0)), ]$peoplelive <- 0
first_survey_data <- filter(first_survey_data, !(peoplelive=="0: I live alone" | numearnincome=="0: I live alone"))

# Remove "15 or More"
first_survey_data <- filter(first_survey_data, !(peoplelive=="More than 15" | peoplelive_15plus=="More than 15" | numearnincome=="More than 15"))
first_survey_data <- filter(first_survey_data, !(peoplelive=="15 or more" | peoplelive_15plus=="15 or more" | numearnincome=="15 or more"))

# Remove all remaining rows containing NAs
colSums(is.na(first_survey_data))
first_survey_data <- first_survey_data[!is.na(first_survey_data$gender),]
first_survey_data <- first_survey_data[!is.na(first_survey_data$dob),]
# first_survey_data <- na.omit(first_survey_data, cols="gender")
colSums(is.na(first_survey_data))

# Create "age" column based on date of birth
first_survey_data <- first_survey_data %>%
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey))

# Drop column
first_survey_data <- select(first_survey_data, -age_at_survey)
#------------------------------------------------------------------------------

# VISUALIZATION----------------------------------------------------------------
# Histogram of female working vs male working
ggplot(first_survey_data, aes(x=working, fill=gender)) + 
  geom_histogram(stat="count")

# Histogram of age with working divided by gender
ggplot(first_survey_data, aes(x=peoplelive, fill=working)) +
  geom_histogram(binwidth = 1) +
  facet_grid(gender ~ .)





