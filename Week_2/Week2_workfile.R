#Week 2 Working File

#This week we are going to focus on Data Transformation (Chapter 3 of R for Data Science)

#First let's look at the data that we have on Brew Reviews

#Should only have to run this once, or just do it from the file explorer

unzip('beer_reviews.zip')


beer_reviews<-read.csv('beer_reviews.csv')

#Holy Crap batman it's huge, there's 1.5 Million rows of data there, clearly we can't work with it all at once, it'll take forever.

#Change it into a tibble (fancy dataframe) so we can work with it a little easier.

library(dplyr)

beer_reviews<-as_tibble(beer_reviews)

IPA_reviews<-filter(beer_reviews,beer_style=="American IPA")

#This is better, but we still have a lot of reviews for the same beers. That could be cool to work with, but maybe we can just look at aggregates for now

#Remember, the %>% allows us to do multiple 'tidy' commands at the same time

IPA_reviews_agg<- IPA_reviews %>%
  #Allows us to do operations on different groups, so in this case for each beer
  group_by(beer_beerid)%>%
  mutate(avg_review=mean(review_overall))%>%
  mutate(num_review=length(review_overall)) %>%
  ungroup() 
  #so now we've added our average and number of reviews for each beer, but we still have every single beer review

IPA_reviews_agg<-IPA_reviews_agg[unique(IPA_reviews_agg$beer_beerid),]


##^^This isn't working, might need to ensure all of the beer id values are consistent


#Woohoo, now we have just 1 row for each of the unique beers in our data

#drop columns we don't care about we could create a list of all the ones we want to keep, or just those we want to drop

IPA_reviews_agg<-select(IPA_reviews_agg,-(review_time:review_profilename))

#and just a couple more

IPA_reviews_agg<-select(IPA_reviews_agg,-(review_palate:review_taste))



