# I created this because this cleaning data is required for enviornmental-protocol.Rmd and Closure_summariesKL.Rmd
#knitr::opts_knit$set(root.dir = 'C:/Users/lewiske/Documents/CAFE/projects/temperature/river_closures')
#options(knitr.kable.NA = '')

# library ----
library(tidyverse)
library(lubridate)
library(knitr)
library(ggpubr)
library(osmdata)
library(sf)
library(geodata)

# source ----
source("Rcode/data-cleaning/0_logger-metadata-2024.R")

# import ----
water22 <- read.csv("./data-working/output/compiled-water-temperature-2014to2022.csv") |> 
   mutate(Serial = as.character(Serial),
          SFA = as.character(SFA))
water23 <- read.csv("./data-working/output/compiled-water-temperature-2023.csv") |> 
   mutate(Serial = as.character(Serial),
          SFA = as.character(SFA))
water24 <- read.csv("./data-working/output/compiled-water-temperature-2024.csv") |> 
   mutate(SFA = as.character(SFA))

partner.data <- read.csv("./data-working/output/compiled-partner-data-2024.csv") |> 
   mutate(Serial = as.character(Serial)) 


# manipulate ----
## by year ----
waterfull <- bind_rows(water22, water23, water24, partner.data) |> 
   mutate(Date.UTC = ymd(Date.UTC),
          Time.UTC = ymd_hms(paste(Date.UTC, Time.UTC), tz = "UTC")) |> 
   filter(Year >= 2022) |> 
   mutate(Time = with_tz(Time.UTC, tzone = "America/St_Johns")) |> 
   mutate(Date = date(Time))

## additional station/river info ----
rivers <- data.frame(Station = c("GreyRiver", "Eagle", "Sandhill", "Lomond", "NEPlacentia", "NorthArm", "Salmonier", "TraverseBrook", " CharBrook2", "CharBrook", "CharBrook1", "HuntRiver", "HuntRiverTroutPool", "MacclesMouth", "StLewisRiver", "Connie River ", "Torrent River"),
                     River.Name2 = c("Grey River", "Eagle River", "Sand Hill River", "Lomond Main River", "Northeast River, Placentia", "North Arm River, Holyrood", "Salmonier River", "Traverse Brook", "Char Brook", "Char Brook", "Char Brook", "Hunt River", "Hunt River", "Terra Nova River", "St. Lewis River", "Conne River", "Torrent River"),
                     River.Number2 = c(121, 10, 11, 154, 90, 75, 81, 58, 1, 1, 1, 2, 2, 62, 179, 116, 162),
                     SFA2 = c('11','2', '2', '14A', '10', '7', '9', '5', '1', '1', '1', '1', '1', '5', '2', '11', '14A'))


# for 2023
water1 <- waterfull |> 
   filter(out.of.water == 0) |> 
   filter(!is.na(Temp.C)) |> 
   ## remove the northwest brook short term loggers
   mutate(Serial = as.integer(Serial)) |> 
   filter(!is.na(Serial)) |> 
   mutate(River.Name = replace(River.Name, River.Name == "", NA)) |> 
   left_join(rivers, join_by(Station)) |> 
   mutate(River.Name = coalesce(River.Name, River.Name2),
          River.Number = coalesce(River.Number, River.Number2),
          SFA = coalesce(SFA, SFA2)) |> 
   dplyr::select(-River.Name2, -River.Number2, -SFA2) |> 
   filter(!is.na(SFA)) |> 
   ## select necessary data for this analysis
   filter(Year == 2023) |> 
   filter(!River.Name %in% c("Char Brook", "Eagle River", "Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River", "South Branch of Great Codroy", "Exploits River"))

# for 2024
water2 <- waterfull |> 
   filter(out.of.water == 0) |> 
   filter(!is.na(Temp.C)) |> 
   ## remove the northwest brook short term loggers
   mutate(Serial = as.integer(Serial)) |> 
   filter(!is.na(Serial)) |> 
   mutate(River.Name = replace(River.Name, River.Name == "", NA)) |> 
   left_join(rivers, join_by(Station)) |> 
   mutate(River.Name = coalesce(River.Name, River.Name2),
          River.Number = coalesce(River.Number, River.Number2),
          SFA = coalesce(SFA, SFA2)) |> 
   dplyr::select(-River.Name2, -River.Number2, -SFA2) |> 
   filter(!is.na(SFA)) |> 
   ## select necessary data for this analysis
   filter(Year == 2024) |> 
   filter(!River.Name %in% c("Char Brook", "Burlington River", "Shinney's River", "Traverse Brook", "Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River", "Lomond Main River", "Exploits River", "Stoney Brook"))

water <- bind_rows(water1, water2)


# hobo <- hobo |> 
#   mutate(Station2 = factor(paste(River.Number, ". ", Station, sep = ""))) |>
#   mutate(Station2 = fct_reorder(Station2, River.Number)) |> 
#   mutate(Time = with_tz(Time.UTC, tzone = "America/St_Johns")) |>  
#   arrange(SFA, River.Number, Serial, Time) |> 
#   mutate(Date = date(Time)) |> 
#   mutate(WaterTemperature_C = replace(WaterTemperature_C, out.of.water !=0, NA))

# removes ----
removes <- current.loggers |> 
   filter(Recording == "Hobo Weather Station") |> 
   filter(Station %in% c("BDL", "Goodyears Dam", "North East Placentia")) |> 
   mutate(remove = "yes")

# partner data ----
partner.loggers <- partner.data |> 
   distinct(Recording, Partner, Station, Serial, River, Lat, Long, SFA, River.Number) |> 
   rename(Group = Partner,
          River.Name = River)

## loggers ----
loggers <- current.loggers |> 
   left_join(removes) |> 
   filter(is.na(remove)) |> 
   dplyr::select(-remove) |> 
   bind_rows(partner.loggers) |> 
   ## add Gray River and Grandy's River
   add_row(Station = "Grey River", 
           Lat = 47.689433,
           Long = -57.001846,
           Recording = "Water",
           SFA = "11",
           River.Number = 121,
           River.Name = "Grey River",
           Group = "CAFE") |> 
   add_row(Station = "Grandys River", 
           Lat = 47.689433,
           Long = -57.001846,
           Recording = "Water",
           SFA = "12",
           River.Number = 125,
           River.Name = "Grandy's River",
           Group = "CAFE") |> 
   ## make sure group is correctly reflected
   mutate(Group = replace(Group, is.na(Group) & River.Number == 61 & Logger.Type == "Tidbit", "FABEC"),
          Group = replace(Group, is.na(Group) & River.Number == 60 & Logger.Type == "Tidbit", "FABEC"),
          Group = replace(Group, is.na(Group) & River.Number == 59 & Logger.Type == "Tidbit", "FABEC"),
          Group = replace(Group, is.na(Group) & River.Number == 62 & Logger.Type == "Tidbit", "FABEC")) |> 
   mutate(Group = replace(Group, is.na(Group) & Serial %in% c(21417670, 21290092, 21042542, 21290104), "SAEN")) |>
   mutate(Group = replace(Group, is.na(Group) & Recording == "Water" & River.Name == "Hunt River", "AROC")) |> 
   mutate(Group = replace(Group, Recording == "Hobo Weather Station" & Serial %in% c(21560024, 21560027,	21931677, 21560020,	21560026, 21931676,21560019,21931680, 21931681, 21560023,	21368518), "C&P")) |> 
   mutate(Group = replace(Group, Recording == "Level" & Station == "Shinneys River", "NCC")) |> 
   ## all remaining are cafe
   mutate(Group = replace(Group, is.na(Group), "CAFE")) |> 
   ## remove bad data
   filter(!Serial %in% c(86,46,48,123456)) |> 
   dplyr::select(Station, Lat, Long, Recording, Serial, Logger.Type, SFA, River.Number, River.Name, Group) |> 
   distinct() |> 
   ## remove duplicates
   filter(Station != "Indian Bay Brook") |> 
   filter(Serial != 21436595)


# import closures ----
real.closures <- read.csv("./data-working/Environmental Protocol Analysis 2020 to 2024 CDV 12-13-2024.csv") |> 
   rename(SFA = Salmon.Fishing.Area) |> 
   filter(Year == 2024) |>
   dplyr::select(SFA, River.Number, Date.of.Closure, Re.open.Date) |> 
   mutate(Date.of.Closure = ymd_h(paste(Date.of.Closure, 5)),
          Re.open.Date = ymd_h(paste(Re.open.Date, 5)))

# this is from the same file as above but filtered by Year == 2023
real.closures23 <- read.csv("./data-working/Environmental Protocol Analysis 2020 to 2024 CDV 12-13-2024.csv") |> 
   rename(SFA = Salmon.Fishing.Area) |> 
   filter(Year == 2023) |>
   dplyr::select(SFA, River.Number, Date.of.Closure, Re.open.Date) |> 
   mutate(Date.of.Closure = ymd_h(paste(Date.of.Closure, 5)),
          Re.open.Date = ymd_h(paste(Re.open.Date, 5)))

# seasons ----
# take water, select columns, filer by year, and filter by date
season24 <- water |> 
   dplyr::select(Time, Station, Serial, Temp.C, Level.m, SFA, River.Number, River.Name, Recording, Date, Year) |> 
   distinct() |> 
   filter(Year == 2024) |> 
   # filter(month(Time)>= 6) |>
   filter(date(Time) >= as_date("2024-06-01") & date(Time) <= as_date("2024-09-07"))
season23 <- water |> 
   dplyr::select(Time, Station, Serial, Temp.C, Level.m, SFA, River.Number, River.Name, Recording, Date, Year) |> 
   distinct() |> 
   filter(Year == 2023) |> 
   # filter(month(Time)>= 6) |>
   filter(date(Time) >= as_date("2023-06-01") & date(Time) <= as_date("2023-09-07"))

season20 <- bind_rows(season24, season23) |> 
   group_by(Year, Time, River.Number, River.Name, SFA) |>
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |>
   ungroup() |> 
   mutate(above20 = Temp.C >= 20) |>
   # filter(River.Name == "Terra Nova River") |>
   # group_by(Station, Serial, SFA, River.Number, River.Name) |>
   group_by(Year, SFA, River.Number, River.Name) |>
   arrange(River.Number, Time) |>
   mutate(period = NA,
          period = replace(period, above20 == lag(above20), 0),
          period = replace(period, above20 != lag(above20), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |>
   mutate(hoursdiff = difftime(lead(Time), Time, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |>
   ungroup() |>
   group_by(Year, SFA, River.Number, River.Name, period, above20) |>
   # group_by(Station, Serial, SFA, River.Number, River.Name, period, above20) |>
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup()

# season20 <- water |>
#   dplyr::select(Time, Station, Serial, Temp.C, Level.m, SFA, River.Number, River.Name, Recording, Date, Year) |>
#   distinct() |>
#   filter(Year == 2024) |>
#   # filter(month(Time)>= 6) |>
#   filter(date(Time) >= as_date("2024-06-01") & date(Time) <= as_date("2024-09-07")) |>
#   group_by(Time, River.Number, River.Name, SFA) |>
#   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |>
#   mutate(above20 = Temp.C >= 20) |>
#   # filter(River.Name == "Terra Nova River") |>
#   # group_by(Station, Serial, SFA, River.Number, River.Name) |>
#   group_by(SFA, River.Number, River.Name) |>
#   arrange(River.Number, Time) |>
#   mutate(period = NA,
#          period = replace(period, above20 == lag(above20), 0),
#          period = replace(period, above20 != lag(above20), 1),
#          period = replace(period, is.na(period), 1),
#          period = cumsum(period)) |>
#   mutate(hoursdiff = difftime(lead(Time), Time, units = "hours"),
#          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |>
#   ungroup() |>
#   group_by(SFA, River.Number, River.Name, period, above20) |>
#   # group_by(Station, Serial, SFA, River.Number, River.Name, period, above20) |>
#   mutate(hours = cumsum(hoursdiff)) |>
#   ungroup()


# closures ----
closures20 <- season20 |> 
   filter(above20 == TRUE) |> 
   filter(hours >= 72) |> 
   # group_by(Station, Serial, SFA, River.Number, River.Name, period) |> 
   group_by(Year, SFA, River.Number, River.Name, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Time),
             stop = max(Time),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |> 
   ungroup()

closure.summary20 <- closures20 |> 
   ## number of closures and total closure days
   group_by(Year, SFA, River.Number, River.Name) |> 
   summarise(num.closures = n(),
             total.days = sum(days.closed),
             Average.Temp = mean(Avg.Temp)) |> 
   ungroup()



season19 <-bind_rows(season24, season23) |> 
   group_by(Year, Time, River.Number, River.Name, SFA) |> 
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |> 
   ungroup() |> 
   mutate(above19 = Temp.C >= 19) |> 
   # filter(River.Name == "Terra Nova River") |>
   # group_by(Station, Serial, SFA, River.Number, River.Name) |> 
   group_by(Year, SFA, River.Number, River.Name) |> 
   arrange(River.Number, Time) |> 
   mutate(period = NA,
          period = replace(period, above19 == lag(above19), 0),
          period = replace(period, above19 != lag(above19), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |> 
   mutate(hoursdiff = difftime(lead(Time), Time, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |> 
   ungroup() |> 
   group_by(Year, SFA, River.Number, River.Name, period, above19) |> 
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup() 

closures19 <- season19 |> 
   filter(above19 == TRUE) |> 
   filter(hours >= 72) |> 
   # group_by(Station, Serial, SFA, River.Number, River.Name, period) |> 
   group_by(Year, SFA, River.Number, River.Name, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Time),
             stop = max(Time),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |> 
   ungroup()

closure.summary19 <- closures19 |> 
   ## number of closures and total closure days
   group_by(Year, SFA, River.Number, River.Name) |> 
   summarise(num.closures = n(),
             total.days = sum(days.closed),
             Average.Temp = mean(Avg.Temp)) |> 
   ungroup()


season18 <- bind_rows(season24, season23) |> 
   group_by(Year, Time, River.Number, River.Name, SFA) |> 
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |> 
   ungroup() |> 
   mutate(above18 = Temp.C >= 18) |> 
   # filter(River.Name == "Terra Nova River") |>
   # group_by(Station, Serial, SFA, River.Number, River.Name) |> 
   group_by(Year, SFA, River.Number, River.Name) |> 
   arrange(River.Number, Time) |> 
   mutate(period = NA,
          period = replace(period, above18 == lag(above18), 0),
          period = replace(period, above18 != lag(above18), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |> 
   mutate(hoursdiff = difftime(lead(Time), Time, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |> 
   ungroup() |> 
   group_by(Year, SFA, River.Number, River.Name, period, above18) |> 
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup() 

closures18 <- season18 |> 
   filter(above18 == TRUE) |> 
   filter(hours >= 72) |> 
   # group_by(Station, Serial, SFA, River.Number, River.Name, period) |> 
   group_by(Year, SFA, River.Number, River.Name, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Time),
             stop = max(Time),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |> 
   ungroup()

closure.summary18 <- closures18 |> 
   ## number of closures and total closure days
   group_by(Year, SFA, River.Number, River.Name) |> 
   summarise(num.closures = n(),
             total.days = sum(days.closed),
             Average.Temp = mean(Avg.Temp)) |> 
   ungroup()


# summary ----
## for tables
sum18 <- closure.summary18 |> 
   group_by(Year) |> 
   summarise(Env.Protocol.Temp = 18,
             Num.Rivers.Closed = n(),
             Total.Closures = sum(num.closures),
             Max.Days.Closed = max(total.days),
             Average.Days.Closed = mean(total.days),
             Average.Temp.During.Closure = mean(Average.Temp),
             SD.Temp = sd(Average.Temp)) |> 
   ungroup()

sum19 <- closure.summary19 |> 
   group_by(Year) |> 
   summarise(Env.Protocol.Temp = 19,
             Num.Rivers.Closed = n(),
             Total.Closures = sum(num.closures),
             Max.Days.Closed = max(total.days),
             Average.Days.Closed = mean(total.days),
             Average.Temp.During.Closure = mean(Average.Temp),
             SD.Temp = sd(Average.Temp)) |> 
   ungroup()

sum20 <- closure.summary20 |> 
   group_by(Year) |> 
   summarise(Env.Protocol.Temp = 20,
             Num.Rivers.Closed = n(),
             Total.Closures = sum(num.closures),
             Max.Days.Closed = max(total.days),
             Average.Days.Closed = mean(total.days),
             Average.Temp.During.Closure = mean(Average.Temp),
             SD.Temp = sd(Average.Temp)) |> 
   ungroup()

overall2023 <- bind_rows(sum20, sum19, sum18) |> 
   filter(Year == 2023) |> 
   mutate(Num.Rivers.Assessed = 29) |> 
   mutate(Max.Days.Closed = ceiling(Max.Days.Closed),
          Avg.Days.Closed = round(Average.Days.Closed, 1),
          Avg.Temp.During.Closure = round(Average.Temp.During.Closure,1),
          SD.Temp = round(SD.Temp,1)) |> 
   mutate(Avg.Temp.During.Closure = paste(Avg.Temp.During.Closure, "±", SD.Temp)) |> 
   dplyr::select(Env.Protocol.Temp, Num.Rivers.Assessed, Num.Rivers.Closed, Total.Closures, Max.Days.Closed, Avg.Days.Closed, Avg.Temp.During.Closure)

overall2024 <- bind_rows(sum20, sum19, sum18) |> 
   filter(Year == 2024) |> 
   mutate(Num.Rivers.Assessed = 30) |> 
   mutate(Max.Days.Closed = ceiling(Max.Days.Closed),
          Avg.Days.Closed = round(Average.Days.Closed, 1),
          Avg.Temp.During.Closure = round(Average.Temp.During.Closure,1),
          SD.Temp = round(SD.Temp,1))|> 
   mutate(Avg.Temp.During.Closure = paste(Avg.Temp.During.Closure, "±", SD.Temp)) |> 
   dplyr::select(Env.Protocol.Temp, Num.Rivers.Assessed, Num.Rivers.Closed, Total.Closures, Max.Days.Closed, Avg.Days.Closed, Avg.Temp.During.Closure)


# df ----
# these are just the dataframe for the temps 18, 19, 20
## take seasons and ......
# the mutate on real.closures is not needed as they are already filtered
df20 <- season20 |> 
   left_join(closures20 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")

df19 <- season19 |> 
   left_join(closures19 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")

df18 <- season18 |> 
   left_join(closures18 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")