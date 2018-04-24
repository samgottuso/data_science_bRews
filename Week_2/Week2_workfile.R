#Week 2 Working File

#This week we are going to focus on Data Transformation (Chapter 3 of R for Data Science)

#First let's look at the data that we have on Brew Reviews

setwd('C:/Users/583185/Desktop/data_science/data_science_bRews/Week_2')

beer_reviews<-read.csv('beer_reviews.csv')

#Holy Crap batman it's huge, there's 1.5 Million rows of data there, clearly we can't work with it all at once, it'll take forever.

#Change it into a tibble (fancy dataframe) so we can work with it a little easier.

library(dplyr)

beer_reviews<-as_tibble(beer_reviews)
