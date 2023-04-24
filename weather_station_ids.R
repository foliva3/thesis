#install.packages('terra')
#install.packages('spatialEco')
library(spatialEco)
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
#NAD83 UTM18N
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
#hillside <- meteo_distance(
  #all_stations,
  #43.3570138,-75.3873953,units = "deg",
  #radius = 30,
  #limit = NULL)
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

#there's something currently wrong with this for loop, its marking 
#all years after 1931 as active for over 50 years and including 2022
#for loop that shows weather stations active in 2022 and for at least 50 years
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
    colorv[i] <- "red"
  }
  mapsave <- tm_shape(cny, unit= "mi")+
    tm_borders(lwd=2, lty=1, col= "black")+
    tm_shape(cny_cities)+
    tm_borders(lwd=1, #line thickness
               lty=1, col= "grey")+ #line type
    tm_shape(year_sub[[i]])+
    tm_dots(size= 0.3, title= "NWS Weather Stations", col= colorv[i])+
    tm_shape(year_sub_current[[i]])+
    tm_dots(size= 0.3, col= colorv[i])+
    tm_layout(title= year[i], legend.title.size= 1, legend.text.size= 0.75, 
              inner.margins= 0.1, title.fontface = "bold", legend.width= 0.8, 
              legend.position = c("LEFT", "top"))+
    tm_scale_bar(position=c("center", "top"), text.size= 0.75)+
    tm_compass(position = c("RIGHT", "bottom"), size = 3)+
    tm_add_legend(title= "Legend", type = "symbol", 
                  labels= c("Active NWS stations", "NWS station active in 2022 and active for at least 50 years"), 
                  col= c("red", "black"))
  
  
  
  tmap_save(mapsave, paste0("C:\\Users\\foliv\\Documents\\thesis data\\active_stations\\year",year[i],".png"), 
            width= 6, height= 6, units= "in", dpi= 150)
  
}


#install.packages("magick")
library(magick)
img_list <- lapply(c("C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1893.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1894.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1895.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1896.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1897.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1898.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1899.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1900.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1901.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1902.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1903.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1904.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1905.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1906.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1907.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1908.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1909.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1910.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1911.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1912.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1913.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1914.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1915.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1916.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1917.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1918.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1919.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1920.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1921.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1922.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1923.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1924.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1925.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1926.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1927.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1928.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1929.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1930.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1931.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1932.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1933.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1934.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1935.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1936.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1937.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1938.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1939.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1940.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1941.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1942.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1943.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1944.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1945.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1946.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1947.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1948.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1949.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1950.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1951.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1952.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1953.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1954.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1955.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1956.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1957.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1958.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1959.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1960.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1961.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1962.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1963.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1964.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1965.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1966.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1967.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1968.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1969.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1970.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1971.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1972.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1973.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1974.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1975.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1976.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1977.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1978.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1979.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1980.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1981.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1982.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1983.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1984.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1985.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1986.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1987.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1988.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1989.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1990.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1991.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1992.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1993.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1994.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1995.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1996.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1997.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1998.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year1999.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2000.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2001.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2002.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2003.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2004.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2005.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2006.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2007.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2008.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2009.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2010.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2011.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2012.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2013.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2014.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2015.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2016.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2017.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2018.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2019.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2020.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2021.png",
                     "C:\\Users\\foliv\\Documents\\thesis data\\time_sequence\\year2022.png"), image_read)

# join the images together
img_joined <- image_join(img_list)

# animate at 4 frames per second
img_animated <- image_animate(img_joined, fps = 4)

# save
image_write(image = img_animated,
            path = "C:\\Users\\foliv\\Documents\\thesis data\\gif_time_seq_3.gif")



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
#map with stations, flash flood, and city labels

cities_ev_stations <- tm_shape(stations, unit= "mi")+
  tm_dots(size= 0.2, title= "NWS Stations", col="red")+
  tm_shape(p_storm_mad)+
  tm_dots(size= 0.2, title= "Madison Flash Floods", col= "blue")+
  tm_shape(p_storm_onei)+
  tm_dots(size= 0.2, title= "Oneida Flash Floods", col= "blue")+
  tm_shape(p_storm_onon)+
  tm_dots(size= 0.2, title= "Onondaga Flash Floods", col= "blue")+
  tm_shape(cny)+
  tm_borders(lwd=2, lty=1, col= "black")+
  tm_shape(cny_cities)+
  tm_borders(lwd=0.5, #line thickness
             lty=1, col= "grey")+ #line type
  tm_scale_bar(position=c("left", "BOTTOM"), text.size= 1)+
  tm_compass(position = c("RIGHT", "bottom"), size = 3)+
  tm_add_legend(title= "Legend", type = "symbol", labels= c("NWS stations", "Flash flood events"), 
                col= c("red", "blue"), size=0.5)+
  tm_layout(title= "NWS Stations and flash flood events in Onondaga, Madison, and Oneida County NY", 
            legend.title.size= 1.15, legend.text.size= 0.75, 
            legend.position= c("left", "top"), inner.margins= 0.17, 
            title.fontface = "bold", title.position = c("LEFT", "TOP"), title.size = 1.16)+
  tm_shape(city_label)+
  tm_borders(lwd=0, #line thickness
             lty=1, col= "grey")+
  tm_text("NAME", shadow=TRUE, fontface = "bold", auto.placement = TRUE)

tmap_save(cities_ev_stations, "C:\\Users\\foliv\\Documents\\thesis data\\cities_events_stations.png", 
            width= 5, height= 5, units= "in", dpi= 200)

#initial attempt at splitting the tricounty areas into polygons for each 
#weather station
#row1 <- loc_storm_mad[1,]
#start_points <- list(rbind(c(row1$BEGIN_LON, row1$BEGIN_LAT), 
                     #c(row1$BEGIN_LON, row1$END_LAT), 
                     #c(row1$END_LON, row1$END_LAT), 
                     #c(row1$END_LON, row1$BEGIN_LAT),
                     #c(row1$BEGIN_LON, row1$BEGIN_LAT)))
                           
#pts <- st_sfc(st_polygon(start_points))

#poly <- st_sf(row1,geometry= pts)
#poly1 <- st_cast(st_bbox(pts), to= "POLYGON")
#plot(poly$geometry)
#plot(p_storm_mad$geometry, add=TRUE)
#plot(bbox, col= "red")

#tm_shape(flash)+
  #tm_borders()
#poly <- row1 %>%
  #st_as_sf(coords = c())

year_ev <- list()
stat_ev_sav <- list()
flash_comb <- rbind(p_storm_onon, p_storm_mad, p_storm_onei)

#for loop that shows active weather stations for each year
for(i in 1:length(year)){
  year_sub[[i]] <- stations[year[i] >= stations$first_year & 
                              year[i] <= stations$last_year,]
  year_ev[[i]] <- flash_comb[year[i] >= stations$first_year & #probably should combine storm event datasets
                             year[i] <= stations$last_year,]
  mapsave <- tm_shape(cny, unit= "mi")+
    tm_borders(lwd=2, lty=1, col= "black")+
    tm_layout(title= year[i])+
    tm_shape(cny_cities)+
    tm_borders(lwd=1, #line thickness
               lty=1, col= "grey")+ #line type
    tm_shape(year_sub[[i]])+
    tm_dots(size= 0.3, title= "NWS Weather Stations", col= "red")+
    tm_scale_bar(position=c("center", "top"), text.size= 1)+
    tm_compass(position = c("RIGHT", "bottom"), size = 3)+
    tm_add_legend(title= "Legend", type = "symbol", labels= c("NWS weather stations"), 
                  col= c("red"))
  
  
  
  tmap_save(mapsave, paste0("C:\\Users\\foliv\\Documents\\thesis data\\wstations_active\\year",year[i],".png"), 
            width= 5, height= 5, units= "in", dpi= 200)
  
}
#install.packages("ggmap")
library(ggmap)
#install.packages("ggvoronoi")
library(ggvoronoi)
#voronoi polygons
# proximity (Voronoi/Thiessen) polygons
stationsV <- vect(prcp_stations)
v <- voronoi(stationsV)
plot(v)
points(stationsV)
vstations <- crop(v, vect(st_union(cities)))
plot(vstations)
points(stationsV)
spat_vstat <- sf::st_as_sf(vstations)
crop_vstat <- st_crop(spat_vstat, cny)
inter_vstat <- st_intersection(spat_vstat, cny)

#install.packages("viridis")
library(viridis)
tmap_voronoi <- tm_shape(inter_vstat, unit= "mi")+
  tm_borders()+
  tm_fill(col="total_years", style= "jenks", n=5, 
          title= "Total years active", 
          palette = viridis(n = 3, direction = -1, option= "G"))+
  tm_shape(cny)+
  tm_borders(lwd=2, lty=1, col= "black")+
  tm_scale_bar(position=c("left", "BOTTOM"), text.size= 1)+
  tm_compass(position = c("RIGHT", "bottom"), size = 4)+
  tm_layout(title= "Weather station coverage using Voronoi diagram", 
            inner.margins= 0.17, title.fontface = "bold", title.position = c("LEFT", "TOP"), 
            legend.text.size = 0.8, legend.title.size = 1.25)+ 
  tm_shape(stations)+
  tm_dots(size= 0.25, title= "NWS Weather Stations", col= "red")

tmap_save(tmap_voronoi, "C:\\Users\\foliv\\Documents\\thesis data\\voronoi_06.png", 
            width= 6, height= 5, units= "in", dpi= 200)
tmap_voronoi


proj_flash <- st_transform(flash_comb, 26918)
comb_event_v <- st_join(proj_flash, spat_vstat)
comb_event_v2 <- cbind(comb_event_v[,1:61],comb_event_v[,64:66])
#st_write(comb_event_v2, "C:\\Users\\foliv\\Documents\\thesis data\\comb_event_v2.shp")


distance_wstations <- comb_event_v2$id
loc_comb_stat <- prcp_stations[prcp_stations$id %in% distance_wstations,]
loc_comb_stat <- cbind(loc_comb_stat[,1:24],loc_comb_stat[,27])
#st_write(loc_comb_stat, "C:\\Users\\foliv\\Documents\\thesis data\\wstations3.shp")

dist_event_stat <- read.csv("C:\\Users\\foliv\\Documents\\thesis data\\w_station_event_dist\\distance.dbf.csv")
#make summary table for distance between events and stations
#install.packages('vtable')
library(vtable)
sumtable(dist_event_stat, vars=c("Distance_between_event_and_station", "ttl_yrs"))
#plot(dist_event_stat)
#################mapping climate variability###################
#making separate columns for year, month, day
w_prcp_daily_v2 <- w_prcp_daily %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))
avg_precip <- w_prcp_daily_v2 %>% #Acquiring the mean rain per station
  group_by(id,month,year) %>% #This command groups the mean data by month
  summarise(avg_prcp = sum(prcp),
            n_count= length(prcp)) #This summarises the station,month combination 
#units for prcp are tenths of mm
avg_precip$cm_avg_prcp <- as.numeric(avg_precip$avg_prcp)/100
sum_prcp <- avg_precip %>%
  filter(n_count >= 28)
sum_prcp_v2 <- inner_join(sum_prcp, stationsny, by= c("id"= "station_id"))
sum_prcp_v3 <- st_as_sf(sum_prcp_v2, coords= c("long", "lat"), crs=4326)
sum_prcp_v4 <- st_transform(sum_prcp_v3, 26918)
#for loop that shows avg rain recorded at each station by month
month <- seq(1, 12)
years <- seq(2000, 2021)
for_month <- list()
avg_prcp_ser <- list()
for_year <- list()
for(i in 1:length(month)){
  for_month[[i]] <- sum_prcp_v4[sum_prcp_v4$month == month[i],]
  for(k in 1:length(years)){
  for_year[[k]] <- for_month[[i]][for_month[[i]]$year == years[k],]
  avg_prcp_ser <- tm_shape(cny, unit= "mi")+
    tm_borders(lwd=2, lty=1, col= "black")+
    tm_layout(title= paste0("Month ",month[i],"Year ",years[k]), 
              inner.margins= 0.17)+
    tm_shape(cny_cities)+
    tm_borders(lwd=1, #line thickness
               lty=1, col= "grey")+ #line type
    tm_shape(for_year[[k]])+
    tm_symbols(size= 0.3, col= "cm_avg_prcp",
               palette = "Blues")+
    tm_text("cm_avg_prcp", just= "bottom", size=0.5)+
    tm_scale_bar(position=c("LEFT", "BOTTOM"), text.size= 1)+
    tm_compass(position = c("RIGHT", "bottom"), size = 3)
    
  
  
  
  tmap_save(avg_prcp_ser, paste0("C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_",month[i],"year_",years[k],".png"), 
            width= 5, height= 5, units= "in", dpi= 200)
  
}}

clim_var_ser <- lapply(c("C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_1year_",years,".png", 
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_2year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_3year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_4year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_5year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_6year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_7year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_8year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_9year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_10year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_11year_",years,".png",
                              "C:\\Users\\foliv\\Documents\\thesis data\\avg_prcp_ser\\month_12year_",years,".png"), image_read)
join_clim <- image_join(clim_var_ser)
# animate at 1 frame per second
clim_gif <- image_animate(join_clim, fps = 4)

# save
image_write(image = clim_gif,
            path = "C:\\Users\\foliv\\Documents\\thesis data\\clim_anim.gif")

avg_month <- sum_prcp_v4 %>%
  group_by(month, year) %>%
  summarise(av_month= mean(cm_avg_prcp), 
            sd_month= sd(cm_avg_prcp), 
            n_month= n())
avg_month2 <- avg_month %>% 
  filter(year>=2000)
avg_month2 <- avg_month2[,1:5]
sum_prcp_v5 <- inner_join(data.frame(sum_prcp_v4), avg_month, 
                          by= c("month", "year"))

#makes anomaly column
sum_prcp_v5$anomaly <- sum_prcp_v5$cm_avg_prcp-sum_prcp_v5$av_month
#coefficient of variation
sum_prcp_v5$c.variation <- ((sum_prcp_v5$sd_month/sum_prcp_v5$av_month)*100)
#############################plots for each month's anomaly##########################
sum_prcp_v5 %>% 
filter(month==1)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "January") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==2)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "February") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==3)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "March") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==4)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "April") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==5)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "May") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==6)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "June") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==7)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "July") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==8)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "August") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==9)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "September") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==10)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "October") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==11)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "November") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==12)%>%
  ggplot(aes(x = as.factor(year), y = anomaly)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Difference from mean monthly total precipitation (cm)") + 
  labs(title = "December") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(-16, 14, by = 4), 
                     limits = c(-16, 14))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)

library(magick)
anomaly_ser <- lapply(c("C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\1.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\2.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\3.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\4.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\5.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\6.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\7.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\8.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\9.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\10.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\11.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_differences\\12.png"), image_read)
join_anomaly <- image_join(anomaly_ser)
# animate at 1 frame per second
anomaly_gif <- image_animate(join_anomaly, fps = 1)

# save
image_write(image = anomaly_gif,
            path = "C:\\Users\\foliv\\Documents\\thesis data\\anomaly_anim.gif")
####################################total monthly precip##############################
min(sum_prcp_v5$cm_avg_prcp)
sum_prcp_v5 %>% 
  filter(month==1)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "January") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==2)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "February") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==3)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "March") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==4)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "April") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==5)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "May") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==6)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "June") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==7)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "July") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==8)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "August") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==9)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "September") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==10)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "October") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==11)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "November") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)
sum_prcp_v5 %>% 
  filter(month==12)%>%
  ggplot(aes(x = as.factor(year), y = cm_avg_prcp)) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Total monthly precipitation (cm)") + 
  labs(title = "December") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40))+
  scale_x_discrete(breaks = seq(1950, 2021, by= 5), 
                   na.translate= FALSE)

library(magick)
tot_prcp_ser <- lapply(c("C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\1.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\2.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\3.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\4.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\5.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\6.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\7.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\8.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\9.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\10.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\11.png",
                        "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_box\\12.png"), image_read)
join_tot_prcp <- image_join(tot_prcp_ser)
# animate at 1 frame per second
tot_prcp_gif <- image_animate(join_tot_prcp, fps = 1)

# save
image_write(image = tot_prcp_gif,
            path = "C:\\Users\\foliv\\Documents\\thesis data\\tot_prcp_anim.gif")

########################correlogram######################
#october 2005
sum_prcp_v6 <- st_sf(sum_prcp_v5)
anom_oct_05 <- sum_prcp_v6 %>% 
  filter(month==10 & year==2005)
anom_oct_05 <- st_as_sf(anom_oct_05)
df_oct_05 <- spatialEco::correlogram(anom_oct_05, v= anom_oct_05$anomaly, 
                                     dist=3000, ns= 1000)
df_oct_05[["autocorrelation"]]$dist_km <- df_oct_05[["autocorrelation"]]$dist/1000
plot_oct_05<- ggplot(df_oct_05[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                     limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(5,100, by=10), 
                     limits = c(3, 99))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "October 2005")
ggsave("oct_05.png", width = 6, height = 4)
#may 2005
anom_may_05 <- sum_prcp_v6 %>% 
  filter(month==5 & year==2005)
anom_may_05 <- st_as_sf(anom_may_05)
df_may_05 <- spatialEco::correlogram(anom_may_05, v= anom_may_05$anomaly, 
                                     dist=3000, ns= 1000)
df_may_05[["autocorrelation"]]$dist_km <- df_may_05[["autocorrelation"]]$dist/1000
plot_may_05<- ggplot(df_may_05[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                     limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(5,102, by=15), 
                     limits = c(3, 102))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "May 2005")
ggsave("may_05.png", width = 6, height = 4)
#october 2019
anom_oct_19 <- sum_prcp_v6 %>% 
  filter(month==10 & year==2019)
anom_oct_19 <- st_as_sf(anom_oct_19)
df_oct_19 <- spatialEco::correlogram(anom_oct_19, v= anom_oct_19$anomaly, 
                        dist=7000, ns= 1000)
df_oct_19[["autocorrelation"]]$dist_km <- df_oct_19[["autocorrelation"]]$dist/1000
plot_oct_19<- ggplot(df_oct_19[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                    limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(14,100, by=14), 
                     limits = c(7, 105))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "October 2019")
#ggsave("oct_19.png", width = 6, height = 4)

#March 2010
anom_mar_10 <- sum_prcp_v6 %>% 
  filter(month==3 & year==2010)
anom_mar_10 <- st_as_sf(anom_mar_10)
df_mar_10 <- spatialEco::correlogram(anom_mar_10, v= anom_mar_10$anomaly, 
                        dist=3000, ns= 2000)
df_mar_10[["autocorrelation"]]$dist_km <- df_mar_10[["autocorrelation"]]$dist/1000
plot_mar_10<- ggplot(df_mar_10[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                     limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(3,108, by=15), 
                     limits = c(3, 108))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "March 2010")
#ggsave("mar_10.png", width = 6, height = 4)

#aug 2019
anom_aug_19 <- sum_prcp_v6 %>% 
  filter(month==8 & year==2019)
anom_aug_19 <- st_as_sf(anom_aug_19)
df_aug_19 <- spatialEco::correlogram(anom_aug_19, v= anom_aug_19$anomaly, 
                        dist=6000, ns= 1000)
df_aug_19[["autocorrelation"]]$dist_km <- df_aug_19[["autocorrelation"]]$dist/1000
plot_aug_19<- ggplot(df_aug_19[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                     limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(5,113, by=15), 
                     limits = c(6, 114))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "August 2019")
#ggsave("aug_19.png", width = 6, height = 4)

#oct 2010
anom_oct_10 <- sum_prcp_v6 %>% 
  filter(month==10 & year==2010)
anom_oct_10 <- st_as_sf(anom_oct_10)
df_oct_10 <- spatialEco::correlogram(anom_oct_10, v= anom_oct_10$anomaly, 
                        dist=6000, ns= 1000)
df_oct_10[["autocorrelation"]]$dist_km <- df_oct_10[["autocorrelation"]]$dist/1000
plot_oct_10<- ggplot(df_oct_10[["autocorrelation"]], 
                     aes(x = dist_km, y = autocorrelation, group = 1))+ 
  geom_line(col='black', linewidth=1)+
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.4), 
                     limits = c(-0.8, 0.8))+
  scale_x_continuous(breaks= seq(10,120, by=20), 
                     limits = c(6, 120))+
  xlab("Distance (kilometers)")+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, fill='blue')+
  theme(axis.text=element_text(size=12), 
        axis.title= element_text(size=14), 
        plot.title= element_text(size=14, face="bold"))+
  labs(title= "October 2010")
#ggsave("oct_10.png", width = 6, height = 4)
#citation("rnoaa")
#version$version.string
#citation("spatialEco")
#citation("sf")
#citation("ggplot2")
#citation("tmap")
#citation("magick")
#citation("terra")
#citation("viridis")
help(correlogram)
###############coefficient of variation interpolate################
avg_month3 <- sum_prcp_v2 %>%
  group_by(month, id) %>%
  summarise(av_month= mean(cm_avg_prcp), 
            sd_month= sd(cm_avg_prcp), 
            n_month= n())

avg_month3 <- left_join(sum_prcp_v2, avg_month3, by= c("id"= "id"))
min(avg_month3$n_month)
max(avg_month3$n_month)
#filters out stations with less than 30 years of data
avg_month4 <- avg_month3 %>% 
  filter(n_month>=30)
#coefficient of variation
avg_month4$c.variation <- ((avg_month4$sd_month/avg_month4$av_month)*100)

#install.packages('gstat')
library(gstat)
library(sp) 

bbox <- st_bbox(sum_prcp_v4)
bbox
cell_size <- 40
x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)
avg_month4_grid <- expand.grid(x=x, y=y)
st_write(prcp_stations,"C:\\Users\\foliv\\Documents\\thesis data\\prcp_stations.shp")
