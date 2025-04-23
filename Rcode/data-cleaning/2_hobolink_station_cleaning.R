##Runs after hobolink station combo which seems to have been split into years
## this can be archived
library(tidyverse)
library(lubridate)

## Station data
#source("./Rcode/data-cleaning/1_hobolink_station_combo.R")
source("./Rcode/data-cleaning/1_hobolink_station_combo_2023.R")

hobo <- hobo23 |> # KL added the 23
  # filter out temperature when sensor is out of water
  mutate(out.of.water = 0,
         out.of.water = replace(out.of.water, WaterLevel_meters <=0, 1))

# hobo |>
#   filter(Station == "Shinneys") |>
#   filter(!is.na(WaterTemperature_C)) |> 
#   # filter(Time >= as_datetime("2023-05-30 00:00:00", tz = "America/St_Johns")) |> 
#   # filter(Time <= as_datetime("2023-06-06 00:00:00", tz = "America/St_Johns")) |> 
#   # filter(Time >= as_datetime("2023-04-19 00:00:00", tz = "America/St_Johns")) |> ## start point
#   # View()
#   filter(Time >= as_datetime("2023-10-01 00:00:00", tz = "America/St_Johns")) |> 
#   ggplot() +
#   # geom_line(aes( x= Time, y = AirTemperature_C), colour = 'red') +
#   geom_line(aes( x = Time, y = WaterTemperature_C), colour = 'blue') +
#   geom_line(aes( x= Time, y = WaterLevel_meters), colour = 'black') +
#   facet_wrap(~Station, scales = "free")



## Remove bad data but not quite sure why

NEPlacentia_bad <- hobo |> 
  filter(Station == "North East Placentia") |> 
  filter(Time <= as_datetime("2022-06-16 05:00:00", tz = "America/St_Johns") | Time >= as_datetime("2022-10-07 13:00:00", tz = "America/St_Johns")) |> 
  filter(year(Time) == 2022)

Trepassey_bad <- hobo |> 
  filter(Station == "Trepassey NW Brook") |> 
  filter(Time < as_datetime("2022-10-12 00:00:00", tz = "America/St_Johns"))

# Shinneys_bad <- hobo |> 
#   filter(Station == "Shinneys") |> 
#   filter(Time <= as_datetime("2023-06-05 20:00:00", tz = "America/St_Johns"))

Exploits_bad <- hobo |> 
  filter(Station == "Exploits Goodyears Dam") |> 
  filter(Time < as_datetime("2023-05-19 00:00:00", tz = "America/St_Johns"))


Stoney_bad <- hobo |> 
  filter(Station == "Stoney") |> 
  filter(WaterPressure_kPa < -800)

baddata <- bind_rows(NEPlacentia_bad, Trepassey_bad, Exploits_bad, Stoney_bad) |> 
  mutate(include = 0)

## 
offloads <- downloads |> 
  filter(eq_type == "Cellular Station" | eq_type == "Wifi Station" | eq_type == "Cellular") |> 
  filter(re_dply == "No") |> 
  mutate(dnld_dt = ymd_hm(dnld_dt)) |> 
  dplyr::select(name, dnld_dt, serial_no) |> 
  rename(Station = name,
         Serial = serial_no)

# join baddata and off loads to hobo data; 
hobo <- hobo |> 
  left_join(baddata) |> 
  mutate(include = replace(include, is.na(include), 1)) |> 
  filter(include == 1) |> 
  dplyr::select(-include) |> 
  ## offload data from field app
  mutate(dnld_dt = ymd_hm(dnld_dt)) |> 
  left_join(offloads) |> 
  ## update removed stations that weren't recorded in field app
  ## Bay de l'Eau Aug 30
  mutate(dnld_dt = replace(dnld_dt, Station == "Bay de l'Eau", as_datetime("2023-08-30 09:00:00")),
         ## South Branch Sept 7
         dnld_dt = replace(dnld_dt, Station == "South Branch Codroy", as_datetime("2023-09-07 09:00:00")),
         ## Garnish Aug 22
         dnld_dt = replace(dnld_dt, Station == "Garnish River", as_datetime("2023-08-22 09:00:00"))) |> 
  ## filter out data after stations removed
  filter(Time <= dnld_dt | is.na(dnld_dt)) |> 
  ## filter out broken sensor data
  filter(WaterTemperature_C > -800 | is.na(WaterTemperature_C)) |>
  filter(WaterTemperature_C < 50 | is.na(WaterTemperature_C))

rm(list = c("baddata", "NEPlacentia_bad", "Shinneys_bad", "Trepassey_bad", "Exploits_bad", "Stoney_bad",
            "downloads", "loggers", "offloads"))  

