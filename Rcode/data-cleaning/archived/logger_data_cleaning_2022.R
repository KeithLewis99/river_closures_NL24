## Purpose: preliminary data cleaning for 2022 logger data
## specific purpose is to filter out data collected before deployments and after retreival
## Filter out data when loggers are out of water (using level loggers only)

##----packages ----
library(tidyverse)
library(lubridate)

## ---- data -----
temperature <- read.csv("./data-working/output/logger-data-2022-full.csv") |> 
  ## format date
  mutate(Date = str_sub(Date.Time, start = 1, end = 10),
         Time = str_sub(Date.Time, start = 12)) |> 
  ## replace "" with 00:00 (the zero's keep getting removed for somereason)
  mutate(Time = replace(Time, Time == "", "00:00:00")) |> 
  ## create full Time column
  mutate(Time = ymd_hms(paste(Date, Time, sep = " "))) |> 
  ## format date column
  mutate(Date = ymd(Date)) |> 
  ## create a year only column
  mutate(Year = year(Date)) |> 
  ## format date deployed
  ## start with the ones with years only
  mutate(Year.deployed = NA,
         Year.deployed = replace(Year.deployed, Date.deployed.MDY == "2019", 2019),
         Year.deployed = replace(Year.deployed, Date.deployed.MDY == "2018", 2018),
         Year.deployed = replace(Year.deployed, Date.deployed.MDY == "2021", 2021)) |> 
  ## correct the diff format date
  mutate(Date.deployed.MDY = replace(Date.deployed.MDY, Date.deployed.MDY == "30-Jun", "6/30/2022")) |> 
  ## format remaining dates
  mutate(Date.deployed.MDY = mdy(Date.deployed.MDY)) |> 
  ## format time deployed
  mutate(Time.deployed = replace(Time.deployed, Time.deployed == "900", "9:00"),
         Time.deployed = replace(Time.deployed, Time.deployed == "", NA),
         Time.deployed = str_pad(Time.deployed, width = 5, side = "left", pad = "0")) |> 
  ## time deployed as date/time
  mutate(Time.deployed = ymd_hm(paste(Date.deployed.MDY, Time.deployed, sep = " "))) |> 
  ## arrange columns in proper order
  dplyr::select(Time, Temp.C, Level.m, Serial, Recording, Station, Lat, Long, SFA, River.Number, River.Name,
                Logger.Type, Depth.cm, Date, Year, Date.Downloaded, Location.description, Deployed.by,
                Date.deployed.MDY, Time.deployed, Year.deployed, Notes)

## ---- filter data based on deployment time -----

t2 <- temperature |> 
  filter(Time > Time.deployed | Date > Date.deployed.MDY | Year >= Year.deployed)

## check differences
checkdiff <- anti_join(temperature, t2)

nrow(t2) + nrow(checkdiff) == nrow(temperature)

ggplot(checkdiff) +
  geom_point(aes(x = Time, y = Temp.C)) +
  facet_wrap(~Serial, scales = "free")
