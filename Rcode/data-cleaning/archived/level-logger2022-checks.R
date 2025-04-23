### Level logger formatting

### Determine level loggers and locations and determine if any of them can be reprocessed to have 
### water level corrected for atmospheric pressure

# --- packages ----
library(tidyverse)
library(lubridate)
library(leaflet)


# ----- data -----
loggers <- read.csv("./data-working/output/logger-data-2022-full.csv") |> 
  mutate(Date.Time = ymd_hms(Date.Time),
         Date.deployed.MYD = mdy(Date.deployed.MDY),
         Time.deployed = dmy_hm(paste(Date.deployed.MDY, Time.deployed, sep = " ")),
         DL.Date = ymd(DL.Date))

head(loggers)
glimpse(loggers)

level.logger <- loggers |> 
  filter(Logger.Type == "Level")

head(level.logger)

station <- read.csv( "./data-working/Loggers_2022.csv",
                     skip = 1,
                     header = TRUE) |>
  rename(Logger.Type = "Logger.Type.Note..minilog...level.are.not.bluetooth") |>
  # dplyr::select( River,
  #                Lat, 
  #                Long, 
  #                Date.deployed.MDY, 
  #                Time.deployed, 
  #                Recording,Serial, 
  #                Logger.Type,
  #                Depth.cm, 
  #                Date.Downloaded,
  #                Location.description, 
  #                Deployed.by,
  #                Notes) |>
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Serial = as.integer(Serial))

head(station)
glimpse(station)

EC <- read.csv("./data-working/baro-data/ECStations.csv")

# ---- map -----
leaflet() |> 
  setView(lng = -55.67, lat=48.93, zoom=5 ) |>  
  addTiles() |> 
  addCircleMarkers( data = level.logger |> distinct(River, Serial, Region, Lat, Long),
              color='black',
              fillColor = 'purple',
              popup = ~paste0(River,
                              "<br>SN: ", Serial,
                              "<br>Lat: ", Lat,
                              "<br>Long: ", Long),
              radius = 5,
              stroke = TRUE,
              weight = 1,
              fillOpacity = .8) |> 
  # addCircles( data = level.logger |> distinct(River, Serial, Region, Lat, Long),
  #             color='purple',
  #             opacity = .3,
  #             radius = 30000) |> 
  addCircleMarkers(data = station |> 
                     filter(!Serial %in% level.logger$Serial) |> 
                     filter(Recording == "Level") |> 
                     distinct(River, Serial, Lat, Long),
                   color='black',
                   fillColor = 'violet',
                   popup = ~paste0(River,
                                   "<br>SN: ", Serial,
                                   "<br>Lat: ", Lat,
                                   "<br>Long: ", Long),
                   radius = 5,
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = .8) |> 
  # addCircles( data = station |> 
  #               filter(!Serial %in% level.logger$Serial) |> 
  #               filter(Recording == "Level") |> 
  #               distinct(River, Serial, Lat, Long),
  #             color = 'violet',
  #             opacity = 0.3,
  #             radius = 30000) |> 
  addCircleMarkers(data = station |> 
                     filter(Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station") |> 
                     distinct(River, Serial, Lat, Long),
                   color='black',
                   fillColor = 'darkorange',
                   popup = ~paste0(River,
                                   "<br>SN: ", Serial,
                                   "<br>Lat: ", Lat,
                                   "<br>Long: ", Long),
                   radius = 5,
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = .8) |> 
  addCircleMarkers(data = EC |> distinct(Location, Latitude, Longitude),
                   color = 'black',
                   fillColor = 'green',
                   popup = ~paste0("Station: ", Location,
                                   "<br>Lat: ", Latitude,
                                   "<br>Long: ", Longitude),
                   radius = 5,
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = 0.8)



  addScaleBar(position = "bottomright") %>%
  addLegend( position = "topright",
             colors = c('green', 'purple', 'darkorange'),
             labels = c("EC Baro", "Level", "Cellular Station") )

  
  
# check distances between remote level loggers and available baro data stations
library(raster)
# Northwest gander to EC stations
pointDistance(c(-55.504167,48.58245 ), EC[EC$Location == "Badger (AUT)", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), EC[EC$Location == "St. Alban's", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), EC[EC$Location == "Gander Intl A", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), EC[EC$Location == "Millertown RCS", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), EC[EC$Location == "Terra Nova National Park", c("Longitude", "Latitude")], lonlat = TRUE)/1000

# Badger 59 km

# Northwest Gander to Cellular stations
pointDistance(c(-55.504167,48.58245 ), station[station$Logger.Type == "Cellular Station" & station$River == "Exploits at Bishops Falls", c("Long", "Lat")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), station[station$Logger.Type == "Cellular Station" & station$River == "Caroline Brook", c("Long", "Lat")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), station[station$Logger.Type == "Cellular Station" & station$River == "Amy's Lake", c("Long", "Lat")], lonlat = TRUE)/1000
pointDistance(c(-55.504167,48.58245 ), station[station$Logger.Type == "Cellular Station" & station$River == "Terra Nova Lower Fishway", c("Long", "Lat")], lonlat = TRUE)/1000


# Highlands to EC stations
pointDistance(c( -58.78328,48.10851), EC[EC$Location == "Wreckhouse", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c( -58.78328,48.10851), EC[EC$Location == "Port Aux Basques", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c( -58.78328,48.10851), EC[EC$Location == "Burgeo", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c( -58.78328,48.10851), EC[EC$Location == "Stephenville A", c("Longitude", "Latitude")], lonlat = TRUE)/1000

# Peterview to Cellular stations
pointDistance(c( -55.38389,49.11018), EC[EC$Location == "Badger (AUT)", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c( -55.38389,49.11018), EC[EC$Location == "Gander Airport CS", c("Longitude", "Latitude")], lonlat = TRUE)/1000
pointDistance(c( -55.38389,49.11018), EC[EC$Location == "Twillingate (AUT)", c("Longitude", "Latitude")], lonlat = TRUE)/1000
