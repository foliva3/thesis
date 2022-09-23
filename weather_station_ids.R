install.packages('terra')
library(terra)
library(rnoaa)
library(dplyr)
library(lubridate)
install.packages('sf')
library(sf)
stationsny <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\
                       station_info.csv")
counties <- st_read("C:\\Users\\foliv\\Documents\\thesis data\\
                    NYS_Civil_Boundaries.shp\\Counties.shp")
plot(counties$geometry)
cny <- counties[counties$NAME =="Oneida"|
                  counties$NAME =="Madison"|
                  counties$NAME == "Onondaga",]
counties.points <- st_as_sf(stationsny, coords = c("long","lat"), 
                            crs=4326)
counties.p <- st_transform(counties.points,st_crs(cny))
plot(cny$geometry)
plot(counties.p$geometry, add=TRUE, pch=19)
stations <- st_intersection(counties.p, cny)
plot(cny$geometry)
plot(stations$geometry, add =TRUE)
