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
  ungroup()%>%
  mutate(beer_beerid=as.integer(beer_beerid))
  #so now we've added our average and number of reviews for each beer, but we still have every single beer review

IPA_reviews_agg_unique<-distinct(IPA_reviews_agg,beer_beerid,.keep_all=TRUE)


#Woohoo, now we have just 1 row for each of the unique beers in our data

#drop columns we don't care about we could create a list of all the ones we want to keep, or just those we want to drop

IPA_reviews_agg_unique<-select(IPA_reviews_agg_unique,-(review_time:review_profilename))

#and just a couple more

IPA_reviews_agg_unique<-select(IPA_reviews_agg_unique,-(review_palate:review_taste))

#Let's do some exploratory data analysis on our remaining data

summary(IPA_reviews_agg_unique)

#What # of beers have the name of "IPA" (real creative guys)

#What brewery had the most American IPAs on the list

#Another visualization practice
library(ggplot2)

#let's look at abv and reviews, but only for beers that have more than 2 reviews

abv_viz_data<-filter(IPA_reviews_agg_unique,num_review>2)

ratings_abv<-ggplot(abv_viz_data,aes(x=beer_abv,y=avg_review))+geom_point()

ratings_abv

#Is there any kind of linear relationship between them?

ratings_abv+geom_smooth(method = 'lm')

#Eh kinda. What do the coefficients look like?

ratings_abv_model<-lm(avg_review~beer_abv,data = abv_viz_data)

ratings_abv_model$coefficients



#Nice so let's do the same thing with a different type of beer

#what are the types of beers we can choose from?

b_types<-unique(beer_reviews$beer_style)

#from the first 50

b_types[1:51]


