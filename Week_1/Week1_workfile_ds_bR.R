library(dplyr)
library(ggplot2)
library(stringr)
library(rvest)
#Load in our data (can be done from preloaded data packages like here, .csv, databases, web pages etc)
sw_data<-starwars


sw_data<-sw_data%>%
  #mutate just allows us to create new columns in our data--- here I'm creating a seperate first and last name columns, along with their URL on Wookiepedia (yes that's star wars wikipedia)
  mutate(first.name=word(name))%>%
  mutate(last.name=ifelse(is.na(word(name,start=2)),"",word(name,start=2)))%>%
  mutate(url=paste0("starwars.wikia.com/wiki/",first.name,"_",last.name))



#Use for loops as a last resort, this is pretty bad practice in R
sw_data$apperances=0
for (row in seq(1,nrow(sw_data))){
  #the apply class of functions allows us to use other functions (in this case length) over multiple items
  sw_data$apperances[row]=sum(sapply(sw_data$films[row],length))
}
  

#Star Wars Appearances by gender--- do male characters show up any more frequently than female characters?
#let's filter our data a little bit
#Taking out those who only appeared in 1 movie
sw_data_gender<-subset(sw_data[sw_data$apperances>1,])
#Taking out droids
sw_data_gender<-sw_data_gender[!is.na(sw_data_gender$gender),]

sw_gender<-ggplot(data=sw_data_gender,aes(name,sw_data_gender$apperances))+geom_point(aes(colour=sw_data_gender$gender),size=8)+labs(title="STAR WARS",subtitle = "Appearances by Gender",y="Appearances",fill="Gender")
sw_gender+theme(axis.text.x=element_text(angle=45,hjust=1),panel.background = element_rect(fill='black'),plot.title=element_text(size=20,face='bold',colour = 'yellow',hjust=0.5),plot.subtitle = element_text(size=18,face='bold',colour = 'yellow',hjust=0.5))
#^ since we are going to reuse this theme, we can save it as a variable to apply to our other graphs
sw_theme<-theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill='black'),plot.title=element_text(size=20,face='bold',colour = 'yellow',hjust=0.5),plot.subtitle = element_text(size=18,face='bold',colour = 'yellow',hjust=0.5))

#One more graph-- let's look at height in the star wars universe, it's listed in CM. I want to know if force users are shorter than the normal population (so I can hold onto hope)

#Createa a list of those who are forse senstive
force_powers<-c('Y','N','N','Y','Y','N','N','N','N','Y','Y','N','N','N','N','N','N','N','Y','Y','N','N','N','N','N','N','N','N','N','N','Y','N','N','Y','N','N','N','N','N','N','N','Y','N','Y','N','N','N','Y','Y','Y','Y','Y','Y','Y','N','N','N','N','N','Y','Y','N','Y','N','N','N','N','N','N','N','Y','N','N','N','N','Y','Y','N','N','N','N','Y','Y','N','N','N','N')
sw_data$force_powers<-'Y'
sw_data$force_powers<-force_powers

sw_height<-ggplot(sw_data,aes(sw_data$height,fill=sw_data$force_powers))+geom_histogram(bins=30,stat='count')+labs(title="Height in CM")


#add a mean line for first and non
mean_no_force<-mean(sw_data$height[sw_data$force_powers=='N'],na.rm=TRUE)
mean_force<-mean(sw_data$height[sw_data$force_powers=='Y'],na.rm=TRUE)


sw_height<-sw_height+geom_vline(aes(xintercept=mean_no_force),colour='red',linetype='dashed')
sw_height<-sw_height+geom_vline(aes(xintercept=mean_force),colour='blue',linetype='dashed')
sw_height+sw_theme




#Okay try one your self---- homeworlds and forcepowers, appearances and force powers

#Step 1-- do a basic transformation


#Step 2-- set up graph

#Step 3-- apply our themes

#Step 4-- finalize


#start of Week 1 Material

#What if we want to graph multiple ting at the same time? 
#Use the fact_wrap formula and facet_grid

#Let's say we are a slightly xenophobic physician on Corrusant and we have a diverse population of patients coming into the office. Can we look at BMI by human or non-human species?

#add some missing values

sw_data[37,'species']='Human'
sw_data[40,'species']='Human'
sw_data[73,'species']='Droid'
sw_data[80,'species']='Umbaran'
sw_data[86,'species']='Human'


#remember we use mutate to add columns to our data
sw_data<-sw_data%>%
  mutate(race=ifelse(species=='Human','Human',ifelse(species=='Droid','Droid','Alien')))%>%
  #add blanks for those we don't need to seperately label
  mutate(tag=ifelse(race=='Alien',species,''))



ggplot(data=sw_data,aes(x=mass,y=height,colour=gender))+geom_point()+geom_text(aes(label=tag),hjust=-.25,vjust=.4)+
  sw_theme+
  facet_grid(~race,scales="free_x")


#Next step--- how to "melt" a dataframe for graphing

#often times you'll have to spread your data out to better graph it, for example if we want to graph the same 

#located in the tidyr library (don't need this line if you just used library(tidyverse))


library(tidyr)

#reading in a different dataset, let's say that you're a marketer who is trying to advertise Jedi Personal Training Classes
new_sw_data_gather<-read.csv('Week_1/gather_SW_data.csv')

#so let's take a look at it, it doesn't make that much sense
View(new_sw_data_gather)

#We would want to before vs after force powers, but we can't do that the way it's set up

new_sw_data_gather<-new_sw_data_gather%>%
  gather(key='Time',value='force_powers','Before.Training','After.Training')

#Gather plot

#Spread example

new_sw_data_spread<-read.csv('Week_1/spread_SW_data.csv')

View(new_sw_data_spread)

new_sw_data_spread<-new_sw_data_spread%>%
  spread(key='Type.Force.Powers',value='Number.Force.Powers')

#Spread Plot