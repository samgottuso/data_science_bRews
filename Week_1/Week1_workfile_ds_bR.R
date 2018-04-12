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

#What if we want to make some comparative graphs that we can look at all at the same time? 
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
  #this is just saying that if they are humans or droids keep their race as human or droid, otherwise assign them to be an alien
  mutate(race=ifelse(species=='Human','Human',ifelse(species=='Droid','Droid','Alien')))%>%
  #add blanks for those we don't need to seperately label
  mutate(tag=ifelse(race=='Alien',species,''))


#load in our data, plot mass and height to x and y axis. Colour the points by gender. Add labels and adjust their position
ggplot(data=sw_data,aes(x=mass,y=height,colour=gender))+geom_point()+geom_text(aes(label=tag),hjust=-.25,vjust=.4)+
  sw_theme+
  #adding the grids based on race--- the scales just makes it a little easier to plot.
  facet_grid(~race,scales="free_x")


#Next step--- how to "melt" a dataframe for graphing

#often times you'll have to gather observations or spread them out to better visualize i.

#Gather is typically used when we have variables that are 

#located in the tidyr library (don't need this line if you just used library(tidyverse))

library(tidyr)

#reading in a different dataset, let's say that you're a marketer who is trying to advertise Jedi Personal Training Classes
new_sw_data_gather<-read.csv('gather_SW_data.csv')

#so let's take a look at it-- it isn't in a very nice format.

#We would want to before vs after force powers, but we can't do that the way it's set up

View(new_sw_data_gather)

#Try to graph it, it's not right at all-- it's just showing a cordinates system instead of the progress

ggplot(new_sw_data_gather,aes(x=Before.Training,y=After.Training))+geom_point()

#so we can gather Before.Training and After.Training into 1 variable of 'time'. The value of each of those observations is put into the force_powers column

new_sw_data_gather<-new_sw_data_gather%>%
  gather(key='Time',value='force_powers','Before.Training','After.Training')

View(new_sw_data_gather)
#That's better

#make Time a factor so we can order it.

new_sw_data_gather$Time<-factor(new_sw_data_gather$Time,levels=c('Before.Training','After.Training'))


#Gather plot -- let's look at the progress our clients made from before and after training

ggplot(new_sw_data_gather,aes(x=Time,y=force_powers,group=Name))+geom_point()+geom_line(colour='yellow')+geom_text(aes(label=new_sw_data_gather$Name,colour= factor(new_sw_data_gather$Side)))+
  scale_color_manual(values=c('Blue','Red'))+
  labs(title="Increase in Force Powers",y='Number of Force Powers')+sw_theme

#Spread example

#Let's load in the data -- it's called spread_SW_data.csv

new_sw_data_spread<-

View(new_sw_data_spread)

#What's wrong with this data if we want to look at light vs dark force powers?

#Because they are in the same column, this data isn't in the format that we want it 

ggplot(new_sw_data_spread,aes(x=Type,y=Number.Force.Powers))+geom_point()


#Let's work together to spread the data. We want to spread out the Type column into 2 new columns of light and dark.We want the values to be the number of powers

new_sw_data_spread<-new_sw_data_spread%>%
  spread()

#Spread Plot-- let's look at a bar graph that shows us each Jedi's force powers

ggplot(aes())+geom_bar(aes())+
  scale_fill_manual(values=c('Red','Blue'))+
  labs(title='Type of Force Powers by Jedi',x='Jedi',y='Number of Force Powers')+sw_theme

