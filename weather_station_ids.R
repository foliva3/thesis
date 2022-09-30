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
plot(cny$geometry)
plot(counties.p$geometry, add=TRUE, pch=19)
#plots city and town boundaries
cities <- st_read("C:\\Users\\foliv\\Documents\\thesis data\\NYS_Civil_Boundaries.shp\\Cities_Towns.shp")
cny_cities <- cities[cities$COUNTY =="Oneida"|
                       cities$COUNTY =="Madison"|
                       cities$COUNTY == "Onondaga",]
plot(cny_cities$geometry)
#sorts out stations in our tricounty area
stations <- st_intersection(counties.p, cny)
#plot to visualize this was done correctly
plot(cny$geometry)
plot(stations$geometry, add =TRUE)
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
w_prcp_daily[w_prcp_daily$COUNTY =="Oneida"|
                       cities$COUNTY =="Madison"|
                       cities$COUNTY == "Onondaga",]
categories <- unique(yourDataFrame$yourColumn)
