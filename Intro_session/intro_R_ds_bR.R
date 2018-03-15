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
sw_data_gender<-subset(sw_data[sw_data$apperances>1,])

sw_gender<-ggplot(data=sw_data_gender,aes(name,sw_data_gender$apperances))+geom_point(aes(colour=sw_data_gender$gender),size=8)+labs(title="STAR WARS",subtitle = "Appearances by Gender",y="Appearances",fill="Gender")
sw_gender+theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill='black'),plot.title=element_text(size=20,face='bold',colour = 'yellow',hjust=0.5),plot.subtitle = element_text(size=18,face='bold',colour = 'yellow',hjust=0.5))


