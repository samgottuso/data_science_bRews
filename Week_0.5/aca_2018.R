library(dplyr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(fiftystater)

#Load in our data

aca_enrollment<-read.csv('aca_2018.csv')

#Change data type to data frame

aca_enrollment<-as.data.frame(aca_enrollment)

#Rename our columns

colnames(aca_enrollment)<-c('Location','Marketplace_type','Total_Consumers')

#cut out rows we don't need--- we want to preserve the total data in aca_enrollment, but we don't want to graph it, so created an extra DF

aca_enrollment_graphing<-aca_enrollment[4:54,]

aca_enrollment<-aca_enrollment[3:54,]
#make this blank
aca_enrollment[1,2]=''

#The map aes only reads lower case states
aca_enrollment_graphing$Location<-tolower(aca_enrollment_graphing$Location)
#Reorder by state
aca_enrollment_graphing<-aca_enrollment_graphing[order(aca_enrollment_graphing$Location),]
abbrevs<-state.abb
#add in DC
abbrevs<-append(abbrevs,'DC',after=8)

#Add abbreviations to the dataframe
aca_enrollment_graphing$abbreviation<-abbrevs


data('fifty_states')


#First graph -- nice by static
ggplot(aca_enrollment_graphing,aes(map_id = aca_enrollment_graphing$Location))+geom_map(aes(fill=Marketplace_type),map = fifty_states,color='black')+
  expand_limits(x=fifty_states$long,y=fifty_states$lat)+
  theme_classic()+
  coord_map()+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "'Murica", y = "",title='Marketplace Type by State') + fifty_states_inset_boxes()



#Interactive Graph in Leaflet

#Loading in the shape files for states
state_shapes<-invisible(readOGR("cb_2017_us_state_20m.shp",layer = "cb_2017_us_state_20m", GDAL1_integer64_policy = TRUE))


#Giving state shapes our data because it's not playing nicely with the aca data
state_shapes<-state_shapes[0:51,]

state_shapes$abbrevation<-aca_enrollment_graphing$abbreviation

state_shapes$Marketplace_type<-aca_enrollment_graphing$Marketplace_type

state_shapes$Total_Consumers<-aca_enrollment_graphing$Total_Consumers

#Making a color pallete for our different marketplaces

state_pal<-colorFactor('Set1',state_shapes$Marketplace_type)



#I'm using Leaflet because I've used it before. There is another package (tmap) apparently builds easily over geom_map ... 
#but converts to leaftlet in the end

leaflet(data = state_shapes) %>%
  addTiles()%>%
  setView(-95,37.8,3.25)%>%
  #This is where we are adding the colors and the labels
  addPolygons(color="#4444444",fillColor = ~state_pal(Marketplace_type),weight = 2, opacity =1, fillOpacity = .8,
              #brings states to the front when we highlight them
              highlight= highlightOptions(weight = 5, bringToFront = TRUE),
              #This is the label that pops up
              label=paste(as.character(state_shapes$abbrevation),"enrollment is ",as.character(state_shapes$Total_Consumers))) %>%
  #adding the legend
  addLegend("topright",pal=state_pal,values=state_shapes$Marketplace_type,title='Marketplace Type by State')

