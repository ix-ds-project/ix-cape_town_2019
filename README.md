# iX Data Science Class Project

#### Names: Alden Ferguson, Alyse Barlow, Yeon Kim
The code presented in this repository represents the steps taken to clean and train data pertaining to the employment status of indivduals.

## SETUP
Have the csv files in the directory './data/raw/'. The files are not on this repo due to their large sizes.

All the code is in the "class_project.R" file.

All the commented code just exists to document progress.

## CLEANING
In order to use our data to create different models, we used the data from the first surve and filtered out irrelevant columns.

We joined in all the score data given to the main dataframe.

We then preprocessed the data by one-hot encoding categorical variables and imputing NA variables in columns.

## VISUALIZATION
We created a quick visuals to see the relationship between females and males working, then viewed the working status of different ages divided by gender 

## MODELING
We then weighted different varibles in the dataset and ran models to predict who would be working. After running the "ranger" method, we predicted who will be working with 71.43% accuracy.
