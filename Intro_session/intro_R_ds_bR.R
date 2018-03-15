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
sw_gender+theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill='black'),plot.title=element_text(size=20,face='bold',colour = 'yellow',hjust=0.5),plot.subtitle = element_text(size=18,face='bold',colour = 'yellow',hjust=0.5))
#^ since we are going to reuse this theme, we can save it as a variable to apply to our other graphs
sw_theme<-theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill='black'),plot.title=element_text(size=20,face='bold',colour = 'yellow',hjust=0.5),plot.subtitle = element_text(size=18,face='bold',colour = 'yellow',hjust=0.5))

#One more graph-- let's look at height in the star wars universe, it's listed in CM. I want to know if force users are shorter than the normal population (so I can hold onto hope)

#Createa a list of those who are forse senstive
force_powers<-c('Y','N','N','Y','Y','N','N','N','N','Y','Y','N','N','N','N','N','N','N','Y','Y','N','N','N','N','N','N','N','N','N','N','Y','N','N','Y','N','N','N','N','N','N','N','Y','N','Y','N','N','N','Y','Y','Y','Y','Y','Y','Y','N','N','N','N','N','Y','Y','N','Y','N','N','N','N','N','N','N','Y','N','N','N','N','Y','Y','N','N','N','N','Y','Y','N','N','N','N')
sw_data$force_powers<-'Y'
sw_data$force_powers<-force_powers

sw_height<-ggplot(sw_data,aes(height,fill=force_powers))+geom_histogram(bins=10)+labs(title="Height in CM")


#add a mean line for first and non
mean_no_force<-mean(sw_data$height[sw_data$force_powers=='N'],na.rm=TRUE)
mean_force<-mean(sw_data$height[sw_data$force_powers=='Y'],na.rm=TRUE)


sw_height<-sw_height+geom_vline(aes(xintercept=mean_no_force),colour='red',linetype='dashed')
sw_height<-sw_height+geom_vline(aes(xintercept=mean_force),colour='blue',linetype='dashed')
sw_height+sw_theme




#Okay try one your self

#Step 1-- do a basic transformation

#Step 2-- set up graph

#Step 3-- apply our themes

#Step 4-- finalize

