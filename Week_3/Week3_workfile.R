#Week 3 Working File

# This week we are going to continue to work on data transformation with our beer data 
# but we're gonna focus on some processes that will make it faster and easier to do repeated processes... mostly through loops

#Not always a best practice (can be slower and a little more cumbersome) but easy to understand

#We'll start with a for loop.

##Basic example of a for loop

#Create a list of numbers

for_list<-as.list(seq(1,10,1))

for(x in for_list){
  print(x+1)
}

#So basic structure is that we take something that we want to do multiple times (printing x+1) and then iterate (repeat) over some values

#The other basic type of loop is the if/else loop

y<-1

if(y>6){
  print("Big")
}else if(y<=2){
  print("Small")
}else{
  print("Just right")
}

#Test something with your IF--- then do an action, or go to another action

#Now let's combine them.

for(elm in for_list){
  if((elm%% 2 == 0)){
    print(paste(elm, " is even"))
  }else{
    print(paste(elm, " is odd"))
  }
}

##Okay now with our beer data

beer_reviews<-read.csv('beer_reviews.csv')

library(dplyr)

beer_reviews<-as_tibble(beer_reviews)

#So let's say instead of selecting 1 type of beer, like we did last time, that we want to select multiple types.

German_beer_types<-c('Hefeweizen','Doppelbock','Schwarzbier','Witbier','Dunkelweizen','Gose')

average_reviews<-vector("double",length = length(German_beer_types))

for(i in seq(1,length(German_beer_types),1)){
  current_reviews<- beer_reviews %>%
    filter(beer_style==German_beer_types[i])
  average_reviews[i]=mean(current_reviews$review_overall)
}

#Mapping/Purr or save that for later?
