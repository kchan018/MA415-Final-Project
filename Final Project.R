## Packages required.
# install.packages("rgeos")
library(rgeos)
# install.packages("rgdal")
library(rgdal)
library(httr)
library(plyr)
library(dplyr)
library(ggplot2)
library(jsonlite)
## Foursquare API Information
client_id="GWSECN2BPBIDYSBEZD35XYTLVDYYNIHFQ3FBE2YTMO3BFJWD"
client_secret="WAZDXRFGQO12IIS00G2KZS0H34BEVDWYDDGCM4AWOWEXRF3K"
v="20180504"
query="Restaurant"
radius=10000
##==============================================
## Downloaded from https://developers.google.com/adwords/api/docs/appendix/geotargeting?csw=1
## These files contain information necessary to get the latitude and longitude of various cities.
cities<-read.csv("files/cities_google.csv")
states<-read.csv("files/states_google.csv")

states<-states[,-4]
names(states)[1]<-"Parent.ID"
city.state<-merge(cities,states,by="Parent.ID",all=TRUE)
city.state<-city.state[,-c(9,10,11,12)]
names(city.state)<-c("Parent.ID","Criteria.ID","City","Canonical.Name","County.Code","Target", "Status","State")

abb<-read.csv("files/abbreviations.csv")

city_state<-merge(city.state,abb,by="State")
city_state[,10]<-as.character(city_state[,10])
city_state[,4]<-as.character(city_state[,4])
##==============================================
##get lat/lon of city centers
##code from http://stackoverflow.com/questions/27867846/quick-way-to-get-longitude-latitude-from-city-state-input
geo_init <- function() {
  
  try({
    GET("http://www.mapcruzin.com/fcc-wireless-shapefiles/cities-towns.zip",
        write_disk("cities.zip"))
    unzip("cities.zip", exdir="cities") })
  
  shp <- readOGR("cities-towns/citiesx020.shp", "citiesx020")
  
  geo <-
    gCentroid(shp, byid=TRUE) %>%
    data.frame() %>%
    rename(lon=x, lat=y) %>%
    mutate(city=shp@data$NAME, state=shp@data$STATE)
  
}

geocode <- function(geo_db, city, state) {
  do.call(rbind.data.frame, mapply(function(x, y) {
    geo_db %>% filter(city==x, state==y)
  }, city, state, SIMPLIFY=FALSE))
}
geo_db <- geo_init()

## Getting the latitude and longitude of Boston, Chicago, Dallas, and San Francisco.

boston=geo_db %>% geocode(city="Boston",state="MA")
boston_ll=paste0(boston$lat,",",boston$lon)
chicago=geo_db %>% geocode(city="Chicago",state="IL")
chicago_ll=paste0(chicago$lat,",",chicago$lon)
dallas=geo_db %>% geocode(city="Dallas",state="TX")
dallas_ll=paste0(dallas$lat,",",dallas$lon)
sf=geo_db %>% geocode(city="San Francisco",state="CA")
sf_ll=paste0(sf$lat,",",sf$lon)


##==============================================
foursquare<-function(client_id,client_secret,v,query,radius,near.df){
  foursquare.list<-list();
  require(jsonlite);
  for(i in 1:length(near.df)){
    near=near.df[i];
    fqs.query=paste0("https://api.foursquare.com/v2/venues/search?client_id=",client_id,
                     "&client_secret=",client_secret,
                     "&intent=browse",
                     "&v=",v,
                     "&radius=",radius,
                     "&ll=",near,
                     "&query=",query);
    fqs.request=readLines(fqs.query);
    foursquare.list[[i]]<-fromJSON(fqs.request,simplifyDataFrame=TRUE);
    Sys.sleep(20);
  }
  return(foursquare.list);
}
##==============================================

# Calling the Foursquare API to obtain restaurant data for each of the cities.
boston_restaurants=foursquare(client_id,client_secret,v,query,radius,boston_ll)
chicago_restaurants=foursquare(client_id,client_secret,v,query,radius,chicago_ll)
dallas_restaurants=foursquare(client_id,client_secret,v,query,radius,dallas_ll)
sf_restaurants=foursquare(client_id,client_secret,v,query,radius,sf_ll)

##==============================================

## Function to obtain list of restaurants from Foursquare.
get_business<-function(list){
  return(list$response$venues)
}

## Obtaining list of Boston's restaurants from Foursquare.
boston_food<-lapply(boston_restaurants,get_business)
boston_test<-boston_food[sapply(boston_food, length)>=1]
boston_test<-lapply(boston_test,flatten)

boston_foodplaces<-rbind.fill(lapply(boston_test,function(y){as.data.frame((y),stringsAsFactors=F)}))
boston_foodplaces<-boston_foodplaces[!duplicated(boston_foodplaces$id),]

## Cleaning list of Boston's restaurants.
boston_temp<-data.frame(t(sapply(boston_foodplaces$categories,c)))

clean_list_data_frame<-function(data){
  for(i in 1:ncol(data)){
    data[,i]<-as.character(data[,i])
  }
  return(data)
}

boston_temp<-clean_list_data_frame(boston_temp)
boston_foodplaces<-within(boston_foodplaces,rm(categories,venueChains,location.formattedAddress,hereNow.groups,specials.items))
boston_foodplaces<-cbind(boston_foodplaces,boston_temp)
boston_foodplaces <- boston_foodplaces[,!duplicated(colnames(boston_foodplaces))]
write.csv(boston_foodplaces, file = "Boston_Food Places.csv",row.names=FALSE)

# Graph of Boston's Most Popular Cuisines
detach("package:plyr",unload=T)
boston_cuisine <- boston_foodplaces %>% group_by(shortName) %>% summarise(count = n())
ggplot(boston_cuisine, aes(x = shortName, y = count, fill = shortName)) + geom_bar(stat = "identity") + coord_flip() + 
      labs(title = "Most Popular Cuisines in Boston", x = "Cuisine", y="Frequency")

## Obtaining list of Chicago's restaurants from Foursquare.
chicago_food<-lapply(chicago_restaurants,get_business)
chicago_test<-chicago_food[sapply(chicago_food, length)>=1]
chicago_test<-lapply(chicago_test,flatten)

library(plyr)
chicago_foodplaces<-rbind.fill(lapply(chicago_test,function(y){as.data.frame((y),stringsAsFactors=F)}))
chicago_foodplaces<-chicago_foodplaces[!duplicated(chicago_foodplaces$id),]

## Cleaning list of Chicago's restaurants.
chicago_temp<-data.frame(t(sapply(chicago_foodplaces$categories,c)))

chicago_temp<-clean_list_data_frame(chicago_temp)
chicago_foodplaces<-within(chicago_foodplaces,rm(categories,venueChains,location.formattedAddress,hereNow.groups,specials.items))
chicago_foodplaces<-cbind(chicago_foodplaces,chicago_temp)
chicago_foodplaces <- chicago_foodplaces[,!duplicated(colnames(chicago_foodplaces))]
write.csv(chicago_foodplaces, file = "Chicago_Food Places.csv",row.names=FALSE)

# Graph of Chicago's Most Popular Cuisines
detach("package:plyr",unload=T)
chicago_cuisine <- chicago_foodplaces %>% group_by(shortName) %>% summarise(count = n())
ggplot(chicago_cuisine, aes(x = shortName, y = count, fill = shortName)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(title = "Most Popular Cuisines in Chicago", x = "Cuisine", y="Frequency")

## Obtaining list of Dallas' restaurants from Foursquare.
dallas_food<-lapply(dallas_restaurants,get_business)
dallas_test<-dallas_food[sapply(dallas_food, length)>=1]
dallas_test<-lapply(dallas_test,flatten)

library(plyr)
dallas_foodplaces<-rbind.fill(lapply(dallas_test,function(y){as.data.frame((y),stringsAsFactors=F)}))
dallas_foodplaces<-dallas_foodplaces[!duplicated(dallas_foodplaces$id),]
## Cleaning list of Dallas' restaurants.
dallas_temp<-data.frame(t(sapply(dallas_foodplaces$categories,c)))

dallas_temp<-clean_list_data_frame(dallas_temp)
dallas_foodplaces<-within(dallas_foodplaces,rm(categories,venueChains,location.formattedAddress,hereNow.groups,specials.items))
dallas_foodplaces<-cbind(dallas_foodplaces,chicago_temp)
dallas_foodplaces <- dallas_foodplaces[,!duplicated(colnames(dallas_foodplaces))]
write.csv(dallas_foodplaces, file = "Dallas_Food Places.csv",row.names=FALSE)

# Graph of Dallas' Most Popular Cuisines
detach("package:plyr", unload=T)
dallas_cuisine <- dallas_foodplaces %>% group_by(shortName) %>% summarise(count = n())
ggplot(dallas_cuisine, aes(x = shortName, y = count, fill = shortName)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(title = "Most Popular Cuisines in Dallas", x = "Cuisine", y="Frequency")

## Obtaining list of San Francisco's restaurants from Foursquare.
sf_food<-lapply(sf_restaurants,get_business)
sf_test<-sf_food[sapply(sf_food, length)>=1]
sf_test<-lapply(sf_test,flatten)

library(plyr)
sf_foodplaces<-rbind.fill(lapply(sf_test,function(y){as.data.frame((y),stringsAsFactors=F)}))
sf_foodplaces<-sf_foodplaces[!duplicated(sf_foodplaces$id),]

## Cleaning list of San Francisco's restaurants.
sf_temp<-data.frame(t(sapply(sf_foodplaces$categories,c)))

sf_temp<-clean_list_data_frame(sf_temp)
sf_foodplaces<-within(sf_foodplaces,rm(categories,venueChains,location.formattedAddress,hereNow.groups,specials.items))
sf_foodplaces<-cbind(sf_foodplaces,chicago_temp)
sf_foodplaces <- sf_foodplaces[,!duplicated(colnames(sf_foodplaces))]
write.csv(sf_foodplaces, file = "San Francisco_Food Places.csv",row.names=FALSE)

# Graph of San Francisco's Most Popular Cuisines
detach("package:plyr",unload=T)
sf_cuisine <- sf_foodplaces %>% group_by(shortName) %>% summarise(count = n())
ggplot(sf_cuisine, aes(x = shortName, y = count, fill = shortName)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(title = "Most Popular Cuisines in San Francisco", x = "Cuisine", y="Frequency")
