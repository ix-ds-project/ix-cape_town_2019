# iX Data Science Class Project

### Names:
Alden Ferguson, Alyse Barlow, Yeon Kim

The code presented in this repository represents the steps taken to clean and train data pertaining to the employment status of indivduals.

## SETUP
Have the csv files in the directory './data/raw/'
All the code is in the class_project.R file

## CLEANING
In order to use our data to create different models, we used the data from the first surve and filtered out irrelevant columns like job_start_date, job_leave_date, company_size, monthly_pay, survey_num, and province.
We went on to imput values for the vast number of NA's in our dataset and later used the date of birth data to create a new column that computed each individual's age. 
We cleaned and added the five other datasets to the original baseline data, creating one large dataset of the most useful information.
Lastly, we converted the strings to numeric values.

## VISUALIZATION
We created a quick visuals to see the relationship between females and males working 
Then viewed the working status of different ages divided by gender 

## MODELING
We then weighted different varibles in the dataset and ran models to predict who would be working.
