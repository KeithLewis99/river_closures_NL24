# This is just to make sure that this stuff works before making an Rmd file
### this is somewhat outdated - some work done in scratch_pad_figuress
### the purpose of this file is to present all CJP_verified files


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
source("closure_FUN.R")

# import ----
water22 <- read.csv("./data-working/output/compiled-water-temperature-2014to2022.csv") |> 
   mutate(Serial = as.character(Serial),
          SFA = as.character(SFA))

water23 <- read.csv("./data-working/output/compiled-water-temperature-2023.csv") |> 
   mutate(Serial = as.character(Serial),
          SFA = as.character(SFA)) |>
   filter(River.Number != 1 & 
             River.Number != 47 & 
             River.Name != "Northwest Gander tributary" & 
             River.Name != "Salmon Brook" &
             River.Name != "Northeast Brook, Trepassey")

water23 <- water23 |>
   filter(!(River.Name == "Shinney's River" & 
               Date.UTC < as.Date("2023-06-06")) 
   ) 

# removes Char bRook 
# removes removRattling which is not a salmon river


water24 <- read.csv("./data-working/output/compiled-water-temperature-2024.csv") |> 
   mutate(SFA = as.character(SFA)) |>
   filter(River.Number != 125 & 
             River.Number != 47 & 
             River.Name != "Northwest Gander tributary" & 
             River.Name != "Salmon Brook" &
             River.Name != "Northeast Brook, Trepassey" &
             River.Name != "Northwest Brook, Trepassey" &
             Serial != 21038064 & # these Salmonier - extra loggers with odd values
             Serial != 	21042585) 
# removes Grandy's River which for 2024, has only a few days of data
# removes Rattling which is not a salmon river
# removes Northwest Gander tributary

water24 <- water24 |>
   filter(!(River.Name == "Tommy's Arm River" & Time.UTC < as.Date("2024-06-10"))) |>
   filter(!(River.Name == "Humber River" & Time.UTC < as.Date("2024-06-27"))) |> # & Time.UTC > as.Date("2024-06-25")
   filter(!(River.Name == "Piercey's Brook" & Time.UTC < as.Date("2024-06-25"))) |>
   filter(!(River.Name == "Garnish River" & Time.UTC < as.Date("2024-06-24"))) |>
   filter(!(River.Name == "Renews River" & Time.UTC < as.Date("2024-06-25"))) |>
   filter(!(River.Name == "Little Barachois Brook" & Time.UTC > as.Date("2024-08-24"))) |>
   filter(!(River.Name == "South Branch of Great Codroy" & 
               (Time.UTC < as.Date("2024-06-27") | 
                   Time.UTC > as.Date("2024-09-03")))) 
 # removes data before data as logger wasn't deployed 
# removes data before data as logger wasn't deployed


partner.data <- read.csv("./data-working/output/compiled-partner-data-2024.csv") |> 
   mutate(Serial = as.character(Serial))

partner.data <- partner.data |>
   filter(River.Number != 1) |> # removes Charbrook
   filter(!(Station == "Sandhill" & Date.Time == "2024-07-08 11:00:00")
          ) |> # bad temp
   filter(!(River == "Lomond River" & Year == 2024)) |>
   filter(!(River == "Lomond River" & 
               Date.UTC > as.Date("2023-06-16") & 
               Date.UTC < as.Date("2023-06-25"))) |>
   filter(!(River == "Grey River" & 
               Date.UTC < as.Date("2023-06-03")) 
   )



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
   filter(!River.Name %in% c("Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River")) # Emilies original c("Char Brook", "Eagle River", "Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River", "South Branch of Great Codroy", "Exploits River")

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
   filter(!River.Name %in% c("Traverse Brook", "Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River", "Lomond Main River")) # Emilies original: c("Char Brook", "Burlington River", "Shinney's River", "Traverse Brook", "Shoal Harbour River, Trinity Bay", "Come By Chance River", "Conne River", "Lomond Main River", "Exploits River", "Stoney Brook")

water <- bind_rows(water1, water2)



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
## see attached file and filter for River.Number == 50 (Gander) & 150 (Humber)  
### for Gander, 2024-07-11 - All tributaries of the main stem of Gander River 
      #### for 2024-07-16"50. *Gander River & tributary streams, Northwest Gander tributary, All other tributaries of the Main Stem and Southwest Gander, Waters above Great Gull Falls on Great Gull River (tributary Northwest Gander), Waters above Big Dead Wolf Falls, including Waters and Caribou Brook (tributary Southwest Gander)"
### for Humber, 2024-07011 is lower Humber River, 2024-07-16 is Humber River & Tributary streams including Adies Lake

### removing first date for now to remove duplicates in df below and problems with creating vertical lines
real.closures <- read.csv("./data-working/Environmental Protocol Analysis 2020 to 2024 CDV 12-13-2024.csv") |> 
   rename(SFA = Salmon.Fishing.Area) |> 
   filter(Year == 2024) |>
   dplyr::select(SFA, River.Number, Date.of.Closure, Re.open.Date) |> 
   mutate(Date.of.Closure = ymd_h(paste(Date.of.Closure, 5)),
          Re.open.Date = ymd_h(paste(Re.open.Date, 5)))
real.closures <- real.closures[-c(35, 116),]




# this is from the same file as above but filtered by Year == 2023
real.closures23 <- read.csv("./data-working/Environmental Protocol Analysis 2020 to 2024 CDV 12-13-2024.csv") |> 
   rename(SFA = Salmon.Fishing.Area) |> 
   filter(Year == 2023) |>
   dplyr::select(SFA, River.Number, Date.of.Closure, Re.open.Date) |> 
   mutate(Date.of.Closure = ymd_h(paste(Date.of.Closure, 5)),
          Re.open.Date = ymd_h(paste(Re.open.Date, 5)))

# seasons ----
## set dates ----
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
## need to find a way to get Recordings back in to filter on that but for now, just proceed.
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

# # just testing some data
# water24 |> 
#    filter(Recording == "Hobo Weather Station") |>
#    filter(River.Name == "Biscay Bay River")
# riv_24 |> filter(River.Name == "Biscay Bay River")
# riv_24 |> filter(River.Name == "South Branch of Great Codroy")
# riv_23 |> filter(River.Name == "South Branch of Great Codroy")
# 
# # these are only the 
# riv_24 <- water24 |> 
#    filter(Recording == "Hobo Weather Station") |>
#    select(Year, SFA, River.Number, River.Name) |>
#    group_by(River.Name) |>
#    arrange(River.Name) |> 
#    filter(row_number()==1) |>
#    print(n = Inf)
# 
# riv_24 <- water |> 
#    filter(Recording == "Hobo Weather Station" & Year == 2024) |>
#    select(Year, SFA, River.Number, River.Name) |>
#    group_by(River.Name) |>
#    arrange(River.Name) |> 
#    filter(row_number()==1) |>
#    print(n = Inf)
# 
# riv_23 <- water23 |> 
#    filter(Recording == "Hobo Weather Station") |>
#    select(Year, SFA, River.Number, River.Name) |>
#    group_by(River.Name) |>
#    arrange(River.Name) |> 
#    filter(row_number()==1) |>
#    print(n = Inf)
#   
# 
# full_join(riv_24, riv_23, by = c("River.Name", "River.Number")) |>
#    arrange(River.Name) |>
#    print(n = Inf)
# 
# unique(df20$Year)
# unique(df20_23$Year)
# 
# 
# riv1_24 <- df20 |> 
#    select(Year, SFA, River.Number, River.Name) |>
#    filter(Year == 2024) |>
#    group_by(River.Name) |>
#    arrange(River.Name) |> 
#    filter(row_number()==1) |>
#    print(n = Inf)
# 
# riv1_23 <- df20_23 |> 
#    select(Year, SFA, River.Number, River.Name) |>
#    group_by(River.Name) |>
#    arrange(River.Name) |> 
#    filter(row_number()==1) |>
#    print(n = Inf)
# 
# full_join(riv1_24, riv1_23, by = c("River.Name", "River.Number")) |>
#    arrange(River.Name) |>
#    print(n = Inf)


# my new stuff----
# this is new to create the same for 2023
# the mutate on real.closures23 is not needed as they are already filtered
df20_23 <- season20 |> 
   left_join(closures20 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures23 |> mutate(Year = 2023), join_by(Year, SFA, River.Number), relationship = "many-to-many")


df19_23 <- season19 |> 
   left_join(closures19 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures23 |> mutate(Year = 2023), join_by(Year, SFA, River.Number), relationship = "many-to-many")

df18_23 <- season18 |> 
   left_join(closures18 |> mutate(status = 'closed'),
             join_by(Year, River.Name, SFA, River.Number, period)) |>
   mutate(status = replace(status, Time < start, "monitored")) |> 
   mutate(status = replace(status, is.na(status), "open")) |> 
   left_join(real.closures23 |> mutate(Year = 2023), join_by(Year, SFA, River.Number), relationship = "many-to-many")

#rivers <- unique(season20$River.Number)
#unique(season20$River.Name)


# manipulations ----
# bind status for 18 & 19 to 20
df20 <- cbind(df20, status19 = df19$status)
df20 <- cbind(df20, status18 = df18$status)

df20_23 <- cbind(df20_23, status19 = df19_23$status)
df20_23 <- cbind(df20_23, status18 = df18_23$status)






## set single river ----
#unique(df20$River.Name)
#river_name <- "Traverse Brook" # "Renews River" #  "Biscay Bay River" # "Renews River" #
river_name <- unique(df20$River.Name)
# "Renews River" 
#  "Biscay Bay River" 
# "Salmonier River"
# "Campbellton River" 
# "Exploits River"
#"Terra Nova River"
#View(df20 |> filter(River.Name == river_name) |> select(Year, Time, River.Name,  Temp.C, status, status19, status18))

## vertical lines ----

v20_2023 <- df20 |>
   filter(Year == 2023 & River.Name == river_name[1] & status == "monitored") |>
   slice_min(Time)
v19_2023 <- df20 |>
   filter(Year == 2023 & River.Name == river_name & status19 == "monitored") |>
   slice_min(Time)
v18_2023 <- df20 |>
   filter(Year == 2023 & River.Name == river_name & status18 == "monitored") |>
   slice_min(Time)

# v19_2024 <- df20 |>
#    filter(Year == 2024 & River.Name == river_name & status19 == "monitored") |>
#    slice_min(Time)
v18_2024 <- df20 |>
   filter(Year == 2024 & River.Name == river_name & status18 == "monitored") |>
   slice_min(Time)

# these are last day closed - so open after
vc19_2023 <- df20 |>
   filter(Year == 2023 & River.Name == river_name & status19 == "closed") |>
   slice_max(Time)
#v19_2023
vc19_2024 <- df20 |>
   filter(Year == 2024 & River.Name == river_name & status19 == "closed") |>
   slice_max(Time)

vc18_2023 <- df20 |>
   filter(Year == 2023 & River.Name == river_name & status18 == "closed") |>
   slice_max(Time)
vc18_2024 <- df20 |>
   filter(Year == 2024 & River.Name == river_name & status18 == "closed") |>
   slice_max(Time)

# v18_2024$Time
# v19_2024$Time
# vc18_2024$Time
# vc19_2024$Time

# loops vert lines ----
# this is just to create a dataframe for all of the vertical lines by year
year <- 2023
temp <-  df20 |>
   select(Year, SFA, River.Number, River.Name, Time, Temp.C, status, status19, status18) |>
   filter(Year == year)
## set all rivers for 2023



temp1 <- temp[1:(length(river_name)), ]
temp1[1:(length(river_name)), 1:length(temp1)] <- NA
#str(temp1)

# this is for when the monitoring "should" start
v20_2023 <-  vert_lines_min(df20, river_name, 2023, status, "monitored")
#v20_2023

v19_2023 <-  vert_lines_min(df20_23, river_name, 2023, status19, "monitored")
#v19_2023

v18_2023 <-  vert_lines_min(df20_23, river_name, 2023, status18, "monitored")
#v18_2023

# this is for when the closure should begin - mc = monitoring ends, closure begins
v20mc_2023 <-  vert_lines_min(df20, river_name, 2023, status, "closed")
#v20_2023

v19mc_2023 <-  vert_lines_min(df20_23, river_name, 2023, status19, "closed")
#v19_2023

v18mc_2023 <-  vert_lines_min(df20_23, river_name, 2023, status18, "closed")
#v18_2023



#source("closure_FUN.R")
# this is for when the closure should end
vc19_2023 <-  vert_lines_max(df20_23, river_name, 2023, status19, "closed")
#vc19_2023

vc18_2023 <- vert_lines_max(df20_23, river_name, 2023, status18, "closed")
#vc18_2023


### 2024 ----
## Duplicates in Gander and Humber River for closures - see closure_summaries.R, import closures for notes
### took a lot of time to figure out why this was messing up the code below and causing warnings
year <- 2024
temp <-  df20 |>
   select(Year, SFA, River.Number, River.Name, Time, Temp.C, status, status19, status18) |>
   filter(Year == year)
### set all rivers for 2024
river_name <- unique(temp$River.Name)


temp2 <- temp[1:(length(river_name)), ]
temp2[1:(length(river_name)), 1:length(temp2)] <- NA
#str(temp2)
temp1 <- temp2 # this is probably poor form - should make tempX an arguement
#str(temp1)


v19_2024 <-  vert_lines_min(df20, river_name, 2024, status19, "monitored")
# v19_2024
v18_2024 <-  vert_lines_min(df20, river_name, 2024, status18, "monitored")
# v18_2024

vc19_2024 <- vert_lines_max(df20, river_name, 2024, status19, "closed")
# vc19_2024

vc18_2024 <- vert_lines_max(df20, river_name, 2024, status18, "closed")
# vc18_2024

# df20 |>
#    select(Year, SFA, River.Number, River.Name, Time,  Temp.C, status, status19, status18) |>
#    filter(Year == year & River.Name == river_name[9] & status19 == "closed") |>
#    slice_max(Time)
# df20[df20$Year == year & df20$River.Name == river_name[9] & df20$status19 != "open",]
# df20[df20$Year == year & df20$River.Name == river_name[9] & df20$status19 == "closed",]

## but this seems to work
# for(i in seq_along(river_name)){
#    temp_i <- df20 |>
#       select(Year, SFA, River.Name, Time,  Temp.C, status, status19, status18) |>
#       filter(Year == year & River.Name == river_name[i] & status19 == "closed") |>
#       slice_min(Time)
#    if(nrow(temp_i) == 0){
#       temp_i <- df20 |>
#          select(Year, SFA, River.Name, Time,  Temp.C, status, status19, status18) |>
#          filter(Year == year & River.Name == river_name[i]) |>
#          slice_min(Time)
#       temp_i[1, 5:8] <- NA
#    }
#    vc19_2024[i,] <- temp_i
# }



## merge vertical lines ----
df_stat_2024 <- left_join(v19_2024, v18_2024, by = c("Year", "SFA", "River.Number", "River.Name")) |>  
   select("Year", "SFA", "River.Number", "River.Name", mon19_24 = "Time.x", mon18_24 = "Time.y")

df_stat_2024 <- left_join(df_stat_2024, vc19_2024, by = c("Year", "SFA", "River.Number", "River.Name")) |>
   select("Year", "SFA", "River.Number", "River.Name", "mon19_24", "mon18_24", close19_24 = "Time")

df_stat_2024 <- left_join(df_stat_2024, vc18_2024, by = c("Year", "SFA", "River.Number", "River.Name")) |>
   select("Year", "SFA", "River.Number", "River.Name", "mon19_24", "mon18_24", "close19_24", close18_24 = "Time")

df_stat_2023 <- left_join(v19_2023, v18_2023, by = c("Year", "SFA", "River.Number", "River.Name")) |> 
   select("Year", "SFA", "River.Number", "River.Name", mon19_23 = "Time.x", mon18_23 = "Time.y")

df_stat_2023 <- left_join(df_stat_2023, vc19_2023, by = c("Year", "SFA", "River.Number", "River.Name")) |>
   select("Year", "SFA", "River.Number", "River.Name", "mon19_23", "mon18_23", close19_23 = "Time")

df_stat_2023 <- left_join(df_stat_2023, vc18_2023, by = c("Year", "SFA", "River.Number", "River.Name")) |>
   select("Year", "SFA", "River.Number", "River.Name", "mon19_23", "mon18_23", "close19_23", close18_23 = "Time")

df_stat_all <- full_join(df_stat_2024, df_stat_2023, by = c("SFA", "River.Number", "River.Name"))  |> arrange(River.Number, River.Name)
#df_stat_all[, 1:5]

# minDate to 18 ----
#source("closure_FUN.R")
# this is just to create a dataframe for all of the vertical lines by year
year <- 2023
temp <-  df20 |>
   select(Year, SFA, River.Number, River.Name, Time, Temp.C) |>
   filter(Year == year)
## set all rivers for 2023

temp1 <- temp[1:(length(river_name)), ]
temp1[1:(length(river_name)), 1:length(temp1)] <- NA
#str(temp1)

# this is for when the temp first hits 18 degrees
# minDate_2023 <-  minDate_temp(df20, river_name, 2023, 18) # but this is for when dates are already trimmed to the season
minDate_2023 <-  minDate_temp(water, river_name, 2023, 18)
minDate_2023$SFA <- as.integer(minDate_2023$SFA)
minDate_2023 <- minDate_2023 |> rename(min18 = Time)
#str(minDate_2023)


# water |>
#    select(Year, SFA, River.Number, River.Name, Time,  Temp.C) |>
#    filter(Year == 2023) |>
#    group_by(River.Name) |>
#    slice_min(Time) 
#v20_2023
# this is just to check that minDate_2023 is right
# temp18_23 <- water |> filter(Date.UTC > "2023-01-01" | Date.UTC < "2023-12-31") |>
#    group_by(River.Name) |>
#    filter(Temp.C > 18) |>
#    slice_min(Time) |>
#    select(River.Name, Serial, Date.UTC, Temp.C) |>
#    print(n = Inf)



year <- 2024
temp <-  df20 |>
   select(Year, SFA, River.Number, River.Name, Time, Temp.C) |>
   filter(Year == year)
## set all rivers for 2023

temp1 <- temp[1:(length(river_name)), ]
temp1[1:(length(river_name)), 1:length(temp1)] <- NA
#str(temp1)

# this is for when the temp first hits 18 degrees
# minDate_2023 <-  minDate_temp(df20, river_name, 2023, 18) # but this is for when dates are already trimmed to the season
minDate_2024 <-  minDate_temp(water, river_name, 2024, 18)
minDate_2024$SFA <- as.integer(minDate_2024$SFA)
minDate_2024 <- minDate_2024 |> rename(min18 = Time)

# minDate_18 <- full_join(minDate_2023, minDate_2024, by = c("SFA", "River.Number", "River.Name")) |> 
#    select("SFA", "River.Number", "River.Name", minDate18_23 = "Date_min18", minDate18_24 = "Time.y")

# water levels ----
## set dates ----
### for right axis
#water_lev <- water |>
#   filter(River.Name == river_name[1] & date(Time) >= as_date("2024-06-15") & date(Time) <= as_date("2024-09-07"))
water_lev <- water |>
   filter(date(Time) >= as_date("2024-06-01") & date(Time) <= as_date("2024-09-07"))
# originally had start time as 2024-06-15 but not sure why
#water_lev |> filter(River.Name == river_name) |> select(Date.UTC, Level.m) # this to show that waterlevels only start June 21

# https://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value

# this is to scale the water levels so that they display properly on the right axis
water_lev$percent <- (water_lev$Level.m - min(water_lev$Level.m)) / (max(water_lev$Level.m) - min(water_lev$Level.m))
water_lev$percent <- (water_lev$Level.m - 0) / (1 - 0)
water_lev$new <- ((15-10) *water_lev$percent) + 10
#plot(density(water_lev$new, na.rm = TRUE))

# this is just to check that the right values are being plotted
#temp <- water_lev |> filter(River.Name == river_name & !is.na(Level.m)) |> select(Time, Level.m) # this to show that waterlevels only start June 21




# max-min ----
## this is to create df_range
### create dataframe
year <- 2023
temp <-  df20_23 |>
   select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
   filter(Year == year)
river_name <- unique(temp$River.Name)
#str(temp)

temp3 <- temp[1:(length(river_name)), ]
temp3[1:(length(river_name)), 1:length(temp3)] <- NA
#temp1$Date <- NA
temp3$Time <- ymd_hms(temp3$Time)
temp3$Date.of.Closure <- ymd_hms(temp3$Date.of.Closure)
#temp3$Re.open.Date <- ymd_hms(temp3$Re.open.Date)
#temp1$Date <- ymd(temp1$Date)

#str(temp3)
#i <- 3
temp_min <- temp3
#year <- 2023
#df20_23[!is.na(df20_23$Date.of.Closure), c(1:6, 17:18)] # this gives 
for(i in seq_along(river_name)){
   temp_i <- df20_23 |>
      filter(River.Name == river_name[i] & Year == year) |>
      select(Year, SFA, Time, River.Number, River.Name, Date.of.Closure, Re.open.Date) |>
      slice(which.min(Date.of.Closure))
   if(nrow(temp_i) == 0){
      temp_i <- df20_23 |>
         select(Year, SFA, Time, River.Number, River.Name, Date.of.Closure, Re.open.Date) |>
         filter(Year == year & River.Name == river_name[i]) |>
         slice_min(Time)
      temp_i[1, 6:7] <- NA
   }
   temp_min[i,] <- temp_i
}
#temp_min 
# str(temp_min)
# temp_min |> print(n = Inf)


### temp max ----

temp_max <- temp3
#i <- 1
for(i in seq_along(river_name)){
   temp_i <- df20_23 |>
      filter(River.Name == river_name[i] & Year == year) |>
      select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
      slice(which.max(Re.open.Date))
   if(nrow(temp_i) == 0){
      temp_i <- df20_23 |>
         select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
         filter(Year == year & River.Name == river_name[i]) |>
         slice(which.max(Time))
      temp_i[1, 6:7] <- NA
   }
   temp_max[i,] <- temp_i
}
# temp_max
# str(temp_max)


df20_23_range <- full_join(temp_min[, c(1:2, 4:6)], temp_max[, c(1:2, 4:5, 7)], by = c("Year", "SFA", "River.Number", "River.Name"))
#df20_23_range |> arrange(River.Number, River.Name) |> print(n = Inf)


### 2024 ----
year <- 2024
temp <-  df20 |>
   select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
   filter(Year == year)
river_name <- unique(temp$River.Name)
#str(temp)

temp4 <- temp[1:(length(river_name)), ]
temp4[1:(length(river_name)), 1:length(temp4)] <- NA
#temp1$Date <- NA
temp4$Time <- ymd_hms(temp4$Time)
temp4$Date.of.Closure <- ymd_hms(temp4$Date.of.Closure)
#temp3$Re.open.Date <- ymd_hms(temp3$Re.open.Date)
#temp1$Date <- ymd(temp1$Date)

#str(temp4)
#i <- 3
temp_min24 <- temp4

for(i in seq_along(river_name)){
   temp_i <- df20 |>
      filter(River.Name == river_name[i] & Year == year) |>
      select(Year, SFA, Time, River.Number, River.Name, Date.of.Closure, Re.open.Date) |>
      slice(which.min(Date.of.Closure))
   if(nrow(temp_i) == 0){
      temp_i <- df20 |>
         select(Year, SFA, Time, River.Number, River.Name, Date.of.Closure, Re.open.Date) |>
         filter(Year == year & River.Name == river_name[i]) |>
         slice_min(Time)
      temp_i[1, 6:7] <- NA
   }
   temp_min24[i,] <- temp_i
}
#temp_min24 
#str(temp_min24)



### temp max ----

temp_max24 <- temp4
#i <- 1
for(i in seq_along(river_name)){
   temp_i <- df20 |>
      filter(River.Name == river_name[i] & Year == year) |>
      select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
      slice(which.max(Re.open.Date))
   if(nrow(temp_i) == 0){
      temp_i <- df20 |>
         select(Year, SFA, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
         filter(Year == year & River.Name == river_name[i]) |>
         slice(which.max(Time))
      temp_i[1, 6:7] <- NA
   }
   temp_max24[i,] <- temp_i
}
#temp_max
#str(temp_max)


df20_24_range <- full_join(temp_min24[, c(1:2, 4:6)], temp_max24[, c(1:2, 4:5, 7)], by = c("Year", "SFA", "River.Number", "River.Name"))
#df20_24_range #|> arrange(River.Number, River.Name) |> print(n = Inf)




# check data sets that they are in order ito SFA, River.Number, and River.Name
# df_test <- df20  |> 
#    filter(River.Name == river_name[1])
# 
# df20  |> 
#    filter(River.Name == river_name[1]) |>
#    slice(which.min(Time))


# check ----
#### all data sets have same order and right number of rows
# all river anmes
river_name <- df_stat_all$River.Name

# water_lev |> select(Year, River.Number, River.Name, Time) |>
#    group_by(River.Name) |>
#    slice(which.min(Time)) |>
#    arrange(River.Number) |>
#    print(n = Inf)
# str(water_lev)
# water_lev[water_lev$River.Name == "Shinney's River", ]
water_lev <- full_join(water_lev, df_stat_all[, 2:4], by = c("SFA", "River.Number", "River.Name")) |> arrange(River.Number, River.Name)
# str(water_lev)
# water_lev[water_lev$River.Name == "Shinney's River", ]


# df20_23_range |> select(Year, River.Number, River.Name) |>
#    arrange(River.Number, River.Name) |>
#    print(n = Inf)


df20_23_range <- full_join(df20_23_range, df_stat_all[, 2:4], by = c("SFA", "River.Number", "River.Name")) |> arrange(River.Number, River.Name)
#str(df20_23_range)
df20_23_range$Year <- ifelse(is.na(df20_23_range$Year), 
                             year, 
                             df20_23_range$Year)

df20_24_range <- full_join(df20_24_range, df_stat_all[, 2:4], by = c("SFA", "River.Number", "River.Name")) |> arrange(River.Number, River.Name)
#str(df20_24_range)
df20_24_range$Year <- ifelse(is.na(df20_24_range$Year), 
                             year, 
                             df20_24_range$Year)
#df20_23_range |> print(n = Inf)


#df_stat_all[, 1:5]


# new plot ----
# this works but make it a loop

# for (i in seq_along(rivers[1:3])) {
#    p1 <-    df20  |>
#       filter(River.Name == river_name[i]) |>
#       ggplot() +
#       geom_rect(aes(xmin = Date.of.Closure, xmax = Re.open.Date,
#                     ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'lightgrey') +
#       {if(!is.na(df20_23_range$Year[1])) {
#          geom_rect(data = df20_23_range, aes(xmin = min(Date.of.Closure),
#                                              xmax = max(Re.open.Date), ymin = -Inf, ymax = Inf), alpha = 0.3, fill               = 'darkgrey')
#       }}  +
#       geom_line(aes(x = Time, y = Temp.C, color = "open")) +
#       geom_line(data = df20 |>
#                    filter(River.Name == river_name[i]) |>
#                    filter(status == "closed"),
#                 aes(x = Time, y = Temp.C, group = period, color = "closed")) +
#       geom_line(data = df20 |>
#                    filter(River.Name == river_name[i]) |>
#                    filter(status == "monitored"),
#                 aes(x = Time, y = Temp.C, group = period, color = "monitored")) +
#       geom_hline(yintercept = 20, linetype = 'dashed') +
#       geom_hline(yintercept = 19, linetype = 'dotdash') +
#       geom_hline(yintercept = 18, linetype = 'dotted') +
#    if(!is.na(v19_2023$status19)){
#       geom_vline(xintercept = v19_2023$Time, linetype = 'dotdash')      
#    } +
#       # geom_vline(xintercept = v18_2023$Time, linetype = 'dotted') +
#       # geom_vline(xintercept = v19_2024$Time, linetype = 'dotdash') +
#       # geom_vline(xintercept = v18_2024$Time, linetype = 'dotted') +
#       # geom_vline(xintercept = vc19_2023$Time, linetype = 'dotdash') +
#       # geom_vline(xintercept = vc18_2023$Time, linetype = 'dotted') +
#       # geom_vline(xintercept = vc19_2024$Time, linetype = 'dotdash') +
#       # geom_vline(xintercept = vc18_2024$Time, linetype = 'dotted') +
#       geom_line(data = water_lev |> filter(River.Name == river_name), aes(x = Time, y = new), colour = 'blue') +
#       theme_bw(base_size = 12) +
#       scale_color_manual(name = "Temperature-based river status",
#                          labels = c("open", "monitored", "closed"),
#                          breaks = c("open", "monitored", "closed"),
#                          values = c('black', 'darkorange', 'red')) +
#       labs(x = "Date",
#            #y = "Temperature (°C)",
#            title = paste(
#               df20$River.Name[df20$River.Name == river_name[i]][1],
#               ", River # = ",
#               df20$River.Number[df20$River.Name == river_name[i]][1],
#               "\n SFA = ",
#               df20$SFA[df20$River.Name == river_name[i]][1],
#               "at 18-20°C threshold")) +
#       scale_y_continuous(name = "Water Temperature (°C)",
#                          sec.axis = sec_axis(trans= ~./15, name = "Water Level (meters)"), limits = c(10, 33)) + # /10 to reduce the axis
#       theme(legend.position = "bottom") + facet_wrap(~Year, scales = "free_x")
#    print(p1)
# 
# }
# 
# #   cat('\n\n')

## temp ----
# temp_min_test <- df20_23 |>
#    filter(River.Name == river_name[12]) |>
#    select(Year, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
#    slice(which.min(Date.of.Closure))
# 
# v19_2023$Time[v19_2023$River.Name == river_name[i],]
# river_name
# year <- 2024
# unique(df20[df20$Year == year,][[4]])
# unique(df20$Year)
# i <- 20
# df20  |>
#    filter(River.Name == river_name[i] & Year == year) |>
#    select(Year, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
#    slice(which.min(Date.of.Closure)) 
# 
# df20  |>
#    filter(River.Name == river_name[i] & Year == year) |>
#    select(Year, Time, River.Number, River.Name, SFA, Date.of.Closure, Re.open.Date) |>
#    slice(which.max(Re.open.Date)) 
# 
# unique(df20$River.Name)
# min(df20$Date.of.Closure, na.rm = T)
# 
# df20_23
# 
# for (i in seq_along(rivers)) {
#    p1 <- df20  |>
#       filter(River.Name == river_name[i]) |>
#       ggplot() +
#       geom_rect(aes(xmin = Date.of.Closure, xmax = Re.open.Date,
#                     ymin = -Inf, ymax = Inf), alpha = 0.2, fill = 'lightgrey') +
#       {if(!is.na(df20_23_range$Year[1])) {
#          geom_rect(data = df20_23_range |> filter(River.Name == river_name[i]), 
#                    aes(xmin = min(Date.of.Closure), 
#                   xmax = max(Re.open.Date), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = 'darkgrey')
#       }}  +
#       geom_line(aes(x = Time, y = Temp.C, color = "open")) +
#       geom_line(data = df20 |>
#                    filter(River.Name == river_name[i]) |>
#                    filter(status == "closed"),
#                 aes(x = Time, y = Temp.C, group = period, color = "closed")) +
#       geom_line(data = df20 |>
#                    filter(River.Name == river_name[i]) |>
#                    filter(status == "monitored"),
#                 aes(x = Time, y = Temp.C, group = period, color = "monitored")) +
#       geom_hline(yintercept = 20, linetype = 'dashed') +
#       geom_hline(yintercept = 19, linetype = 'dotdash') +
#       geom_hline(yintercept = 18, linetype = 'dotted') +
#       geom_vline(xintercept = v19_2023[v19_2023$River.Name == river_name[i],][[4]], linetype = 'dotdash') +
#       geom_vline(xintercept = v18_2023[v18_2023$River.Name == river_name[i],][[4]], linetype = 'dotted') +
#       geom_vline(xintercept = v19_2024[v19_2024$River.Name == river_name[i],][[4]], linetype = 'dotdash') +
#       geom_vline(xintercept = v18_2024[v18_2024$River.Name == river_name[i],][[4]], linetype = 'dotted') +
#       geom_vline(xintercept = vc19_2023[vc19_2023$River.Name == river_name[i],][[4]], linetype = 'dotdash') +
#       geom_vline(xintercept = vc18_2023[vc18_2023$River.Name == river_name[i],][[4]], linetype = 'dotted') +
#       geom_vline(xintercept = vc19_2024[vc19_2024$River.Name == river_name[i],][[4]], linetype = 'dotdash') +
#       geom_vline(xintercept = vc18_2024[vc18_2024$River.Name == river_name[i],][[4]], linetype = 'dotted') +
#       #### Water level
#       geom_line(data = water_lev |> filter(River.Name == river_name), aes(x = Time, y = new), colour = 'blue') +
#       theme_bw(base_size = 12) +
#       scale_color_manual(name = "Temperature-based river status",
#                          labels = c("open", "monitored", "closed"),
#                          breaks = c("open", "monitored", "closed"),
#                          values = c('black', 'darkorange', 'red')) +
#       labs(x = "Date",
#            #y = "Temperature (°C)",
#            title = paste(
#               df20$River.Name[df20$River.Name == river_name[i]][1],
#               ", River # = ",
#               df20$River.Number[df20$River.Name == river_name[i]][1],
#               "\n SFA = ",
#               df20$SFA[df20$River.Name == river_name[i]][1],
#               "at 18-20°C threshold")) +
#       scale_y_continuous(name = "Water Temperature (°C)",
#                          sec.axis = sec_axis(trans= ~./15, name = "Water Level (meters)"), limits = c(10, 33)) + # /10 to reduce the axis
#       theme(legend.position = "bottom") + facet_wrap(~Year, scales = "free_x")
#    print(p1)
# 
# }

#   cat('\n\n')

# # loop ----
# ## select essential cols ----
# # year <- 2023
# # temp <-  df20 |> 
# #    select(Year, SFA, Time, River.Name,  Temp.C, status, status19, status18) |>
# #    filter(Year == year)
# # river_name <- unique(temp$River.Name)
# # temp |> filter(River.Name == "Char Brook")
# # 
# # # start ----
# # tab1 <- temp[1:(length(river_name)*3), ]
# # tab1[, 1:length(tab1)] <- NA
# # tab1$Date <- NA
# # tab1$Time <- ymd_hms(tab1$Time)
# # tab1$Date <- ymd(tab1$Date)
# # str(tab1)
# # i <- 12
# # 
# # 
# # temp |> filter(River.Name == river_name[1] & status == "monitored") |> slice_min(Time)
# # 
# # # loop-start ----
# # for(i in seq_along(river_name)){
# #    #browser()
# #    tab <- 
# #       bind_rows(
# #          temp |> filter(River.Name == river_name[i] & status == "monitored") |> slice_min(Time), 
# #          temp |> filter(River.Name == river_name[i] & status19 == "monitored") |> slice_min(Time),
# #          temp |> filter(River.Name == river_name[i] & status18 == "monitored") |> slice_min(Time)
# #       ) |>
# #       mutate(Date = date(Time))
# #    # if(nrow(tab) == 2){
# #    #    tab <- bind_rows(
# #    #       tab,
# #    #    temp |> filter(River.Name == river_name[i] & status18 == "monitored") |> slice_min(Time) |>
# #    #       mutate(Date = date(Time))
# #    #    )
# #    # }
# #    if(nrow(tab) == 2){
# #       tab <- tab |>
# #          add_row(tab, .before = 1) 
# #       tab[1, 4:8] <- NA
# #    }
# #    
# #    # if(nrow(tab) == 1){
# #    #    tab <- bind_rows(
# #    #       tab,
# #    #       tab,
# #    #       tab
# #    #    )
# #    # }
# #    if(nrow(tab) == 1){
# #       tab <- tab |>
# #          add_row(tab, .before = 1) |>
# #          add_row(tab, .before = 1)
# #       tab[1:2, 4:8] <- NA
# #    }
# #    if(nrow(tab) == 0){
# #       tab <- tab |>
# #          add_row(tab, .before = 1) 
# #       tab[1:3, 4:8] <- NA
# #    }
# #    # if(nrow(tab) < 3){
# #    #       tab <- tab |>
# #    #          add_row(tab[, 1:3], .before = 1)
# #    #    }
# #    if(i ==1) {
# #       tab1[c(i, i+1, i+2),] <- tab      
# #    } else {
# #       tab1[c((i*2)+(i-2), (i*2)+(i-1), (i*2)+(i)),] <- tab
# #    }
# # }   
# # 
# # #source("closure_FUN.R")
# # 
# # #river_status(temp)
# # 
# # 
# # 
# # # clean ----
# # tab1$temp <- as.integer(rep(c("20", "19", "18"), length(river_name)))
# # 
# # date_fill <- ymd("1900-01-01") 
# # tab1 <- tab1 |> mutate(Date = case_when(is.na(Date) ~ as.Date(date_fill), TRUE ~ Date))
# # str(tab1)   
# # 
# # 
# # # make table ----
# # tab1_wide <- tab1  |>
# #    select(Year, SFA, River.Name, Date, temp) |>
# #    group_by(temp) |>
# #    mutate(pivot_id = 1:n()) |>
# #    ungroup () |>
# #    pivot_wider(
# #       names_from = temp, 
# #       names_prefix = "Temp_",
# #       values_from = Date
# #    ) |>
# #    select(-pivot_id)
# # 
# # 
# # tab1_wide
# # tab1_wide <- na.omit(tab1_wide)
# # str(tab1_wide)
# # tab1_wide$SFA <- as.integer(tab1_wide$SFA)
# # is.na(tab1_wide) <- tab1_wide == "1900-01-01"
# # tab1_wide$diff19 <- as.numeric(tab1_wide$Temp_20 - tab1_wide$Temp_19)
# # tab1_wide$diff18 <- as.numeric(tab1_wide$Temp_20 - tab1_wide$Temp_18)
# # View(tab1_wide)
# # 
# # view(tab1_wide |> arrange(River.Name))
# # kbl(tab1_wide |> arrange(SFA), align = 'lllccccc') |>
# #    add_header_above(header = c(" " = 3, "Temp" = 3, "Days" = 2)) |>
# #    column_spec(3, width = "65mm") |>
# #    column_spec(1:2, width = "18mm") |>
# #    column_spec(4:6, width = "30mm") |>
# #    column_spec(7:8, width = "18mm")
# # 
# # 
# # # END ----
# # # first attempt at the table
# # # df20 |> filter(River.Name == "Renews River") |> 
# # #    select(Year, Time, River.Name,  Temp.C, status, status19, status18) |>
# # c