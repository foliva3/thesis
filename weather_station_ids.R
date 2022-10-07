#install.packages('terra')
library(terra)
library(rnoaa)
library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages('sf')
library(sf)
stationsny <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\station_info.csv")
counties <- st_read("C:\\Users\\foliv\\Documents\\thesis data\\NYS_Civil_Boundaries.shp\\Counties.shp")
plot(counties$geometry)
cny <- counties[counties$NAME =="Oneida"|
                  counties$NAME =="Madison"|
                  counties$NAME == "Onondaga",]
counties.points <- st_as_sf(stationsny, coords = c("long","lat"), 
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
id_station <- stations$station_id
#pull data from stations using rnoaa
daily_ghcnd <- meteo_pull_monitors(
  id_station,
  keep_flags = FALSE,
  date_min = "1950-01-01",
  date_max = "2021-12-31",
  var = "all")
#removes any rows where prcp is NA
w_prcp_daily <- subset(daily_ghcnd, !is.na(prcp))
all_stations <- ghcnd_stations()
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
