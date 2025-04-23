### Labrador data loggers 2023

library(tidyverse)
library(lubridate)

## 2023 station data
# source("./Rcode/data-cleaning/metadata-setup.R") 


# loggers2023 <- loggers2023 |> 
#   mutate(Group = "CAFE") |> 
#   mutate(Group = replace(Group, dply_by == "Kevin Power", "SAEN")) |> 
#   rename(Station = name,
#          Long = x,
#          Lat = y,
#          Location.description = locn_desc,
#          Serial = serial_no,
#          Logger.Type = eq_type,
#          Recording = recording) |> 
#   mutate(Recording = replace(Recording, Logger.Type == "Cellular" | Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station", "Hobo Weather Station")) |> 
#   mutate(Time.deployed = ymd_hm(dply_dt_tm),
#          Date.deployed.MDY = date(Time.deployed)) |> 
#   ##update which cellular are from CP
#   mutate(Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Burlington", "C&P"),
#          Group = replace(Group, Recording == "Hobo Weather Station" & Station == "BDL", "C&P"),
#          Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Salmon Brook Cell Station", "C&P"),
#          Group = replace(Group, Recording == "Hobo Weather Station" & Station == "South Branch", "C&P"),
#          Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Stoney River", "C&P")) |> 
#   mutate(Depth.cm = depth_m*100) |> 
#   dplyr::select(-depth_m, -dply_by, -dply_dt_tm)



## eagle river
eagle <- read.csv("./data-working/Labrador/eagle river 2023.csv") |> 
  gather(key = "Serial", value = "Temp.C", 2:3) |> 
  mutate(Serial = as.numeric(str_remove(Serial, "X"))) |> 
  # mutate(Date.Time = mdy_hms(date, tz = "Canada/Atlantic")) |> 
  dplyr::select(date, Temp.C, Serial) |> 
  mutate(Station = "Eagle River",
         Recording = "Water",
         SFA = 2,
         River.Number = 10,
         River.Name = "Eagle River",
         Group = "NCC") |> 
  mutate(Date.Time = mdy_hms(date))
  

# eagle |> 
#   filter(Date.Time >= as_datetime("2023-06-28 10:00:00", tz = "Canada/Atlantic") & 
#            Date.Time <= as_datetime("2023-06-28 15:00:00", tz = "Canada/Atlantic")) |> 
#   filter(Serial == 21022452) |> 
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C))


eagleRemove <- eagle |> 
  filter(Date.Time >= as_datetime("2023-06-28 10:00:00") & 
           Date.Time <= as_datetime("2023-06-28 15:00:00")) |> 
  filter(Serial == 21022452) |> 
  mutate(Remove = "YES")

# eagle |>
#   left_join(eagleRemove) |> 
#   mutate(Remove = replace(Remove, is.na(Remove), "NO")) |> 
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C, colour = Remove))

## Shinneys Level
shinneySN <- read.csv("./data-working/Labrador/Level_Shinneys_21400367_2023.csv", nrow = 1, header = FALSE) |> 
  mutate(Serial = as.numeric(str_extract(V1, "[0-9]+"))) |> 
  dplyr::select(-V1)
shinneysLevel <- read.csv("./data-working/Labrador/Level_Shinneys_21400367_2023.csv", skip = 2, header = FALSE) |> 
  rename(Date.Time = V2,
         Temp.C = V4,
         Level.m = V6) |> 
  dplyr::select(Date.Time, Temp.C, Level.m) |> 
  filter(!is.na(Temp.C)) |> 
  mutate(Serial = shinneySN$Serial) |> 
  mutate(Station = "Shinney's Highway",
         Recording = "Level",
         SFA = 2,
         River.Number = 15,
         River.Name = "Shinney's River",
         Group = "NCC") |> 
  mutate(Date.Time = mdy_hms(Date.Time))


# shinneysLevel |>
#   mutate(out.of.water = 0) |> 
#   mutate(out.of.water = replace(out.of.water, Level.m <= 0, 1)) |> 
#   filter(Date.Time <= as_datetime("2023-06-22 10:00:00", tz = "Canada/Atlantic")) |>
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C, colour = as.character(out.of.water)))
# 
# shinneysLevel |>
#   mutate(out.of.water = 0) |> 
#   mutate(out.of.water = replace(out.of.water, Level.m <= 0, 1)) |> 
#   filter(Date.Time >= as_datetime("2023-11-20 14:00:00", tz = "Canada/Atlantic")) |>
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C, colour = as.character(out.of.water)))

ShinneysRemove <- shinneysLevel |> 
  filter(Date.Time <= as_datetime("2023-06-22 10:00:00") | 
                                  Date.Time >= as_datetime("2023-11-20 14:00:00")) |> 
  mutate(Remove = "YES")

# shinneysLevel |>
#   left_join(ShinneysRemove) |> 
#   mutate(Remove = replace(Remove, is.na(Remove), "NO")) |> 
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C, colour = Remove))

## Hunt River
hunt <- read.csv("./data-working/Labrador/HuntRiverTemperature.csv") |> 
  mutate(Date.Time = ymd_hms(paste(Date, Time))) |> 
  dplyr::select(Date.Time, Temp.C) |> 
  filter(!is.na(Date.Time)) |> 
  mutate(Station = "Hunt River Loggers",
         Recording = "Water",
         SFA = 1,
         River.Number = 2,
         River.Name = "Hunt River",
         Group = "AROC") 

hunt1 <- read.csv("./data-working/Labrador/20542211 Hunt Falls 2023-08-30 10_54_25 ADT (Data NDT)(1).csv", skip = 1, header = FALSE) |> 
  mutate(Date.Time = mdy_hms(V2)) |> 
  rename(Temp.C = V3) |> 
  dplyr::select(Date.Time, Temp.C) |> 
  mutate(Serial = 20542211,
         Station = "Hunt Falls",
         SFA = 1,
         River.Number = 2,
         River.Name = "Hunt River",
         Group = "AROC")
hunt2 <- read.csv("./data-working/Labrador/21031907 Hunt Gorge Dock 2023-08-30 11_04_15 ADT (Data NDT)(1).csv", skip = 1, header = FALSE) |> 
  mutate(Date.Time = mdy_hms(V2)) |> 
  rename(Temp.C = V3) |> 
  dplyr::select(Date.Time, Temp.C) |> 
  mutate(Serial = 21031907,
         Station = "Hunt Gorge Dock",
         SFA = 1,
         River.Number = 2,
         River.Name = "Hunt River",
         Group = "AROC")
hunt3 <- read.csv("./data-working/Labrador/21031929 Hunt Falls 2 2023-08-30 10_51_33 ADT (Data NDT)(1).csv", skip = 1, header = FALSE) |> 
  mutate(Date.Time = mdy_hms(V2)) |> 
  rename(Temp.C = V3) |> 
  dplyr::select(Date.Time, Temp.C) |> 
  mutate(Serial = 21031929,
         Station = "Hunt River Falls 2",
         SFA = 1,
         River.Number = 2,
         River.Name = "Hunt River",
         Group = "AROC") |> 
  filter(Date.Time > as_datetime("2022-08-26 10:00:00"))

huntloggers <- bind_rows(hunt1, hunt2, hunt3)
# huntloggers |>
#   ggplot(aes(x = Time, y = Temp.C)) +
#   geom_point() +
#   facet_wrap(~Station)

# huntloggers |> 
#   filter(Serial == 21031907) |> 
#   filter(Date.Time <= as_datetime("2021-07-29 00:00:00", tz = "Canada/Atlantic")) |> 
#   ggplot() +
#   geom_point(aes(x = Date.Time, y = Temp.C))

huntRemove <- huntloggers |> 
  filter(Serial == 21031907) |> 
  filter(Date.Time <= as_datetime("2021-07-29 00:00:00")) |> 
  mutate(Remove = "YES")

Removals <- bind_rows(eagleRemove, ShinneysRemove, huntRemove) |> 
  dplyr::select(-Remove)

labLogs <- bind_rows(eagle, shinneysLevel, hunt, huntloggers) |> 
  anti_join(Removals) |> 
  dplyr::select(-date)


rm(list = c("eagle", "hunt", "shinneysLevel", "shinneySN", "eagleRemove", "ShinneysRemove", "Removals", "hunt1", "hunt2", "hunt3", "huntRemove", "huntloggers"))
