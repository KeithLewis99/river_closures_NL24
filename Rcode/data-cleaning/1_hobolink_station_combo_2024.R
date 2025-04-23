# combine all hobo station data to single file

### packages
library(tidyverse)
library(lubridate)



### data

## Station data
source("./Rcode/data-cleaning/metadata-setup.R")


## 2024 cellular stations
hobo24 <- read.csv("./data-working/New Cellular station Lat and Long.csv") |>
  mutate(Recording = "Hobo Weather Station") |>
  rename(name = Station,
         serial_no = Serial,
         recording = Recording)

stations <- loggers |> 
  filter(eq_type == "Cellular Station" | eq_type == "Wifi Station" | eq_type == "Cellular") |> 
  add_row(name = "Shinneys", lat = 52.59578, long = -56.05356, River.Name = "Shinney's River", recording = "Hobo Weather Station", serial_no = 21474804) |> 
  # select first deployment entry for each station
  group_by(serial_no) |> 
  mutate(dply_dt_tm = ymd_hm(dply_dt_tm)) |> 
  filter(dply_dt_tm == min(dply_dt_tm) | is.na(dply_dt_tm)) |> 
  ungroup() |> 
  mutate(long = x,
         lat = y) |> 
  mutate(name = replace(name, name == "South Branch" & eq_type == "Cellular", "South Branch Codroy")) |> 
  dplyr::select(-SFA, -River.Name, -River.Number) |> 
  bind_rows(hobo24)


sfa <- read.csv("./data-working/loggerSFAs.csv") |> 
  distinct()


  

## temperature data
# files <- list.files( path = "./data-working/cellular_stations/2024", pattern = ".csv", full.names = TRUE )
files <- list.files( path = "./data-working/CJP_verified_files", pattern = ".csv", full.names = TRUE )

hobo.list <- lapply( files, read_csv)

# redundancy in object just EG being lazy (her words) - no big deal
hobo24 <- bind_rows(hobo.list) |> 
  mutate(Time_UTC = mdy_hm(Time_UTC)) |> 
  ## convert Time_NDT to Time_UTC (to standardize)
  # mutate(Time_NDT = mdy_hm(Time_NDT, tz = "America/St_Johns")) |> 
  # rename(Time_NST = Time_NDT) |> 
  # mutate(Time_UTC2 = with_tz(Time_NST, tzone = "UTC")) |> 
  # mutate(Time_UTC = coalesce(Time_UTC,Time_UTC2),
         # Time_UTC = with_tz(Time_UTC, tzone = "UTC")) |> 
  mutate(Time = with_tz(Time_UTC, tzone = "America/St_Johns")) |> 
  rename(Time.UTC = Time_UTC) |>
  ## remove duplicates
  distinct() |> 
  ## correct station names
  #mutate(River = str_trim(River, side = "both")) |> 
  mutate(River = replace(River, River == "Amys Lake", "Amy's Lake"),
         River = replace(River, River == "Burlington", "Burlington River"),
         River = replace(River, River == "Campbellton", "Campbellton River"),
         River = replace(River, River == "Hunt River", "Hunt River Labrador"),
         River = replace(River, River == "Trepassey NW Brook", "Northeast Brook, Trepassey"),
         River = replace(River, River == "Stoney", "Stoney Brook"),
         River = replace(River, River == "Terra Nova River", "Terra Nova Lower Fishway"),
         River = replace(River, River == "Tommys Arm", "Tommy's Arm")) |> 
  left_join(sfa, join_by(River == name)) |> 
  ## add station data
  ## to fix this: need to have station name and year as well as serial (for when stations move around)
  left_join(stations, by = join_by("Serial" == "serial_no", River == name), relationship = "many-to-many") |> 
  dplyr::select(-locn_desc, -recording, -dply_by, -x, -y) |> 
  # add shinny's lat and long
  mutate(lat = replace(lat, River == "Shinneys", 52.595785),
         long = replace(long, River == "Shinneys", -56.053565)) |>
  ## add Shinney's SFA and river number
  mutate(SFA = replace(SFA, River == "Shinneys", "2"),
         River.Number = replace(River.Number, River == "Shinneys", 15)) |> 
  rename(Station = River)

rm(list = c("hobo.list", "loggers","stations", "files"))

