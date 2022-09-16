#install county weather package
install.packages('countyweather')
install.packages('devtools')
library(devtools)
install_github("leighseverson/countyweather", force=TRUE)
library(countyweather)
#pulling madison county data
madison_daily <- daily_fips(fips = "36053", date_min = "1950-01-01", 
                            date_max = "2021-12-31", var = "prcp")
any(grepl("^\\.Renviron", list.files("~", all.files = TRUE)))

