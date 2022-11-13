#install.packages('terra')
library(terra)
library(rnoaa)
library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages('tmap')
library(tmap)
#install.packages('sf')
library(sf)
stationsny <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\station_info.csv")
counties <- st_read("C:\\Users\\foliv\\Documents\\thesis data\\NYS_Civil_Boundaries.shp\\Counties.shp")
plot(counties$geometry)
cny <- counties[counties$NAME =="Oneida"|
                  counties$NAME =="Madison"|
                  counties$NAME == "Onondaga",]
all_stations <- ghcnd_stations()
counties.points <- st_as_sf(all_stations, coords = c("longitude","latitude"), 
                            crs=4326)
counties.p <- st_transform(counties.points,st_crs(cny))
#plots tricounty boundaries
plot(cny$geometry)
plot(counties.p$geometry, add=TRUE, pch=19)
#reads city boundary shape file and sorts out cities within tricounty area
cities <- st_read("C:\\Users\\foliv\\Documents\\thesis data\\NYS_Civil_Boundaries.shp\\Cities_Towns.shp")
cny_cities <- cities[cities$COUNTY =="Oneida"|
                       cities$COUNTY =="Madison"|
                       cities$COUNTY == "Onondaga",]
#sorts out stations in our tricounty area
stations <- st_intersection(counties.p, cny)
#plot with city/town boundaries with stations
plot(cny_cities$geometry)
plot(stations$geometry, add =TRUE, pch=19)
#vector with only station ids
id_station <- stations$id
#pull data from stations using rnoaa
daily_ghcnd <- meteo_pull_monitors(
  id_station,
  keep_flags = FALSE,
  date_min = "1950-01-01",
  date_max = "2021-12-31",
  var = "all")
#removes any rows where prcp is NA
w_prcp_daily <- subset(daily_ghcnd, !is.na(prcp))
hillside <- meteo_distance(
  all_stations,
  43.3570138,-75.3873953,units = "deg",
  radius = 30,
  limit = NULL)
#data frame with the last year of recorded data for cny stations
cny_stations_years <- all_stations[all_stations$id %in% id_station,]
#adds column for active vs nonactive weather stations
cny_stations_years <- cny_stations_years %>%
  mutate(status = if_else(.$last_year < 2022, "Nonactive", "Active"))
#tmap for tricounty area with city boundaries and stations
counties_cities <- tm_shape(cny_cities)+
  tm_borders(lwd=1, #line thickness
             lty=1, col= "grey")+ #line type
tm_shape(cny)+
  tm_borders(lwd=2, lty=1, col= "black")+
tm_shape(stations)+
  tm_dots(size= 0.3, title= "NWS Weather Stations", col= "red")+
  tm_scale_bar(position=c("center", "top"), text.size= 1)+
  tm_compass(position = c("RIGHT", "bottom"), size = 3)+
  tm_layout(title= "NWS Stations in Onondaga, Madison, and Oneida County NY", 
            inner.margins= 0.04, title.fontface = "bold")
  
tmap_save(counties_cities, "C:\\Users\\foliv\\Documents\\thesis data\\cities_counties.png", 
          width= 5, height= 5, units= "in", dpi= 200)
min(stations$first_year)
max(stations$last_year)

year <- seq(1893, 2022)
year_sub <- list()

mapsave <- list()
#subsets stations collecting prcp data
prcp_stations <- stations[stations$element == "PRCP",]
#makes new column with total number of years that station has been active
prcp_stations$total_years <- prcp_stations$last_year - prcp_stations$first_year
#designates whether station is currently operating
prcp_stations$current_year <- ifelse(prcp_stations$last_year == 2022, 1,0)

year_sub_current <- list()
colorv <- character()

for(i in 1:length(year)){
  year_sub[[i]] <- prcp_stations[year[i] >= prcp_stations$first_year & 
                              year[i] <= prcp_stations$last_year,]

  if(nrow(prcp_stations%>%
          filter(year[i] >= first_year & 
                 year[i] <= last_year&
                 current_year == 1&
                 total_years >50))!=0){
  year_sub_current[[i]] <- prcp_stations%>%
      filter(year[i] >= first_year & 
                                   year[i] <= last_year&
                                     current_year == 1&
                                     total_years >50)}else{
                                       year_sub_current[[i]] <- prcp_stations[year[i] >= prcp_stations$first_year & 
                                        year[i] <= prcp_stations$last_year,]}
  if(nrow(prcp_stations%>%
          filter(year[i] >= first_year & 
                 year[i] <= last_year&
                 current_year == 1&
                 total_years >50))!=0){
  colorv[i] <- "black"}else{
    colorv[i] <- "white"
  }
  mapsave <- tm_shape(cny, unit= "mi")+
    tm_borders(lwd=2, lty=1, col= "black")+
    tm_shape(year_sub[[i]])+
    tm_dots(size= 0.3, title= "NWS Weather Stations", col= "red")+
    tm_shape(year_sub_current[[i]])+
    tm_dots(size= 0.3, col= colorv[i])+
    tm_layout(title= year[i])+
    tm_shape(cny_cities)+
    tm_borders(lwd=1, #line thickness
               lty=1, col= "grey")+ #line type
    tm_scale_bar(position=c("center", "top"), text.size= 1)+
    tm_compass(position = c("RIGHT", "bottom"), size = 3)+
    tm_add_legend(title= "Legend", type = "symbol", 
                  labels= c("NWS stations", "NWS station active in 2022 and has been
                            active for at least 50 years"), 
                  col= c("red", "black"))
  
  
  
  tmap_save(mapsave, paste0("C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year",year[i],".png"), 
            width= 5, height= 5, units= "in", dpi= 200)
  
}


#install.packages("magick")
library(magick)
# list file names and read in
imgs <- list.files(path = "C:\\Users\\foliv\\Documents\\thesis data\\
                   time_sequence", full.names = TRUE)
img_list <- lapply(imgs, image_read)

# join the images together
img_joined <- image_join(img_list)

# animate at 4 frames per second
img_animated <- image_animate(img_joined, fps = 4)

# save
image_write(image = img_animated,
            path = "C:\\Users\\foliv\\Documents\\thesis data\\gif_time_seq_2.gif")



#read in csv with storm database for madison, oneida, and onondaga
storm_mad <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\storm\\madison_50_21.csv")
storm_onon <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\storm\\onondaga_50_21.csv")
storm_onei <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\storm\\oneida_50_21.csv")

#subset flash floods that have locations and make a geometry column
loc_storm_mad <- subset(storm_mad, !is.na(BEGIN_LON))
p_storm_mad <- st_as_sf(loc_storm_mad, coords = c("BEGIN_LON","BEGIN_LAT"), 
                            crs=4326)
#onondaga flash floods
loc_storm_onon <- subset(storm_onon, !is.na(BEGIN_LON))
p_storm_onon <- st_as_sf(loc_storm_onon, coords = c("BEGIN_LON","BEGIN_LAT"), 
                        crs=4326)
#oneida flash floods
loc_storm_onei <- subset(storm_onei, !is.na(BEGIN_LON))
p_storm_onei <- st_as_sf(loc_storm_onei, coords = c("BEGIN_LON","BEGIN_LAT"), 
                        crs=4326)
city_label <- cny_cities[cny_cities$POP2020 >30000,]
#map with stations and flash flood
tm_shape(cny_cities, unit= "mi")+
  tm_borders(lwd=1, #line thickness
             lty=1, col= "grey")+ #line type
  tm_shape(stations)+
  tm_dots(size= 0.3, title= "NWS Stations", col="red")+
  tm_shape(p_storm_mad)+
  tm_dots(size= 0.3, title= "Madison Flash Floods", col= "blue")+
  tm_shape(p_storm_onei)+
  tm_dots(size= 0.3, title= "Oneida Flash Floods", col= "blue")+
  tm_shape(p_storm_onon)+
  tm_dots(size= 0.3, title= "Onondaga Flash Floods", col= "blue")+
  tm_scale_bar(position=c("left", "BOTTOM"), text.size= 1)+
  tm_compass(position = c("RIGHT", "bottom"), size = 3)+
  tm_add_legend(title= "Legend", type = "symbol", labels= c("NWS stations", "Flash flood events"), 
                col= c("red", "blue"))+
  tm_layout(title= "NWS Stations and flash flood events in Onondaga, Madison, 
            and Oneida County NY", legend.title.size= 1.5, legend.text.size= 1, 
            legend.position= c("left", "top"), inner.margins= 0.08, 
            title.fontface = "bold")+
  tm_shape(city_label)+
  tm_borders(lwd=1, #line thickness
             lty=1, col= "grey")+
  tm_text("NAME")+
  tm_shape(cny)+
  tm_borders(lwd=2, lty=1, col= "black")

row1 <- loc_storm_mad[1,]
start_points <- list(rbind(c(row1$BEGIN_LON, row1$BEGIN_LAT), 
                     c(row1$BEGIN_LON, row1$END_LAT), 
                     c(row1$END_LON, row1$END_LAT), 
                     c(row1$END_LON, row1$BEGIN_LAT),
                     c(row1$BEGIN_LON, row1$BEGIN_LAT)))
                           
pts <- st_sfc(st_polygon(start_points))

poly <- st_sf(row1,geometry= pts)
#poly1 <- st_cast(st_bbox(pts), to= "POLYGON")
plot(poly$geometry)
plot(p_storm_mad$geometry, add=TRUE)
#plot(bbox, col= "red")

tm_shape(flash)+
  tm_borders()
poly <- row1 %>%
  st_as_sf(coords = c())

year_ev <- list()
stat_ev_sav <- list()
flash_comb <- rbind(p_storm_onon, p_storm_mad, p_storm_onei)

for(i in 1:length(year)){
  year_sub[[i]] <- stations[year[i] >= stations$first_year & 
                              year[i] <= stations$last_year,]
  year_ev[[i]] <- flash_comb[year[i] >= stations$first_year & #probably should combine storm event datasets
                             year[i] <= stations$last_year,]
  mapsave <- tm_shape(cny, unit= "mi")+
    tm_borders(lwd=2, lty=1, col= "black")+
    tm_shape(year_sub[[i]])+
    tm_dots(size= 0.3, title= "NWS Weather Stations", col= "red")+
    tm_layout(title= year[i])+
    tm_shape(cny_cities)+
    tm_borders(lwd=1, #line thickness
               lty=1, col= "grey")+ #line type
    tm_scale_bar(position=c("center", "top"), text.size= 1)+
    tm_compass(position = c("RIGHT", "bottom"), size = 3)+
    tm_add_legend(title= "Legend", type = "symbol", labels= c("NWS weather stations"), 
                  col= c("red"))
  
  
  
  tmap_save(mapsave, paste0("C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year",year[i],".png"), 
            width= 5, height= 5, units= "in", dpi= 200)
  
}

voronoi <- voronoi_polygon(stations,x="x",y="y")
#voronoi polygons