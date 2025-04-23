## Logger meta data for 2024

# ---- Load packages -----
library(tidyverse) 
library(lubridate) # working with dates


# ---- Upload station data ----
# rivers by sfa
#sfas <- read.csv("../../data-working/loggerSFAs.csv") # works when running R chunk but not markdown
sfas <- read.csv("data-working/loggerSFAs.csv") # keep this - its a little redundant with import in metadata-setup.R but not worth fixing

# station data
## his file is for loggers deployed pre-ETA and was created manually before Emilie came on staff; need to get all loggers in the ETA at some point but this will take time; need to check if ETA is fully up-to-date; then won't need this file. 
loggers2022 <- read.csv( "./data-working/Loggers_2022.csv",
                         skip = 1,
                         header = TRUE) |>
  rename(Logger.Type = "Logger.Type.Note..minilog...level.are.not.bluetooth") |>
  dplyr::select(2:17) |>
  dplyr::select(-SFA) |>
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Serial = as.integer(Serial),
         Depth.cm = as.integer(Depth.cm)) |>
  rename(Station = River) |>
  left_join(sfas, join_by(Station == name)) |>
  ## format deployement dates
  ## correct the diff format date
  mutate(Date.deployed.MDY = replace(Date.deployed.MDY, Date.deployed.MDY == "30-Jun", "6/30/2022")) |>
  ## format remaining dates
  mutate(Date.deployed.MDY = mdy(Date.deployed.MDY)) |>
  ## format time deployed
  mutate(Time.deployed = replace(Time.deployed, Time.deployed == "900", "9:00"),
         Time.deployed = replace(Time.deployed, Time.deployed == "", "20:00"),
         Time.deployed = str_pad(Time.deployed, width = 5, side = "left", pad = "0")) |>
  ## time deployed as date/time
  mutate(Time.deployed = ymd_hm(paste(Date.deployed.MDY, Time.deployed, sep = " "))) |>
  ## for special cases where logger was redeployed and date deployed isn't accurate
  mutate(Time.deployed = replace(Time.deployed, Serial == 21031919, NA),
         Time.deployed = replace(Time.deployed, Serial == 21022451, NA)) |>
  mutate(Date.deployed.MDY = replace(Date.deployed.MDY, Serial == 21031919, NA),
         Date.deployed.MDY = replace(Date.deployed.MDY, Serial == 21022451, NA)) |>
  mutate(Recording = replace(Recording, Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station", "Hobo Weather Station")) |>
  distinct() |>
  mutate(Serial = as.character(Serial))

# ## 2024 station data
source("./Rcode/data-cleaning/metadata-setup.R")

## 2024 cellular stations ----
##### ETA wasn't working in 2024 so not all sites were entered in field app and it wasn't up to date so it was a quick fix

hobo24 <- read.csv("./data-working/New Cellular station Lat and Long.csv") |>
  rename(Lat = lat,
         Long = long) |>
  mutate(Recording = "Hobo Weather Station") |>
  left_join(sfas, join_by(Station == name)) |>
  mutate(Serial = as.character(Serial))

# from meta-data-setup
loggers <- loggers |>
  mutate(Group = "CAFE") |>
  mutate(Group = replace(Group, dply_by == "Kevin Power", "SAEN")) |>
  rename(Station = name,
         Long = x,
         Lat = y,
         Location.description = locn_desc,
         Serial = serial_no,
         Logger.Type = eq_type,
         Recording = recording) |>
  mutate(Recording = replace(Recording, Logger.Type == "Cellular" | Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station", "Hobo Weather Station")) |>
  mutate(Time.deployed = ymd_hm(dply_dt_tm),
         Date.deployed.MDY = date(Time.deployed)) |>
  ##update which cellular are from CP
  mutate(Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Burlington", "C&P"),
         Group = replace(Group, Recording == "Hobo Weather Station" & Station == "BDL", "C&P"),
         Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Salmon Brook Cell Station", "C&P"),
         Group = replace(Group, Recording == "Hobo Weather Station" & Station == "South Branch", "C&P"),
         Group = replace(Group, Recording == "Hobo Weather Station" & Station == "Stoney River", "C&P")) |>
  mutate(Depth.cm = depth_m*100) |>
  dplyr::select(-depth_m, -dply_by, -dply_dt_tm)  |>
  mutate(Serial = as.character(Serial))


## SAEN station data
### same as for New Cellular station Lat and Long; this was for when only one partner; it could come out; transition with partner data from only SAEN
saen <- read.csv("./data-working/SAEN Loggers 2023.csv",
                 skip = 1,
                 header = TRUE) |>
  rename(Logger.Type = "Logger.Type.Note..minilog...level.are.not.bluetooth") |>
  dplyr::select(2:17) |>
  dplyr::select(-SFA) |>
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Serial = as.integer(Serial),
         wpt = as.integer(wpt)) |>
  rename(Station = River) |>
  left_join(sfas, join_by(Station == name)) |>
  mutate(Group = "SAEN") |>
  ## format remaining dates
  mutate(Date.deployed.MDY = mdy(Date.deployed.MDY)) |>
  ## format time deployed
  mutate(Time.deployed = replace(Time.deployed, Time.deployed == "900", "9:00"),
         Time.deployed = replace(Time.deployed, Time.deployed == "", "20:00"),
         Time.deployed = str_pad(Time.deployed, width = 5, side = "left", pad = "0")) |>
  ## time deployed as date/time
  mutate(Time.deployed = ymd_hm(paste(Date.deployed.MDY, Time.deployed, sep = " "))) |>
  distinct() |>
  mutate(Serial = as.character(Serial))

## missing metadata ----
### ## manual fix because ETA wasn't complete
missing <- read.csv("./data-working/missing-metadata_2024.csv") |>
  left_join(sfas, join_by(Station == name))

## loggers that are not recorded in FieldApp
loggers22only <- loggers2022 |>
  filter(!Serial %in% loggers$Serial)

# looks like this binds the above dataframes and then standardizes names, adds Shinneys, adds "NA", rounds Lat/Long, changes dates, serial numbers, and removes NAs and doubles
full.loggers <- bind_rows(loggers22only, loggers, saen, missing, hobo24) |>
  mutate(Recording = replace(Recording, Logger.Type == "Cellular" | Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station", "Hobo Weather Station"),
         Recording = replace(Recording, Logger.Type == "Level", "Level"),
         Recording = replace(Recording, Logger.Type == "Pendant" & Recording == "", "Water"),
         Recording = replace(Recording, Logger.Type == "Tidbit" & Recording == "", "Water")) |>
  # filter(Logger.Type != "Minilog") |>
  add_row(Station = "Shinneys", Lat = 52.59578, Long = -56.05356, River.Name = "Shinney's River", Recording = "Hobo Weather Station", Group = "CAFE", Serial = "21474804") |>
  mutate(River.Name = replace(River.Name, River.Name == "", NA)) |>
  mutate(Lat = round(Lat, 3),
         Long = round(Long, 3)) |>
  ## add a datetime to Shinneys and South Branch
  mutate(Date.deployed.MDY = replace(Date.deployed.MDY, Station == "South Branch" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Date.deployed.MDY = replace(Date.deployed.MDY, Station == "Shinneys" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Time.deployed = replace(Time.deployed, Station == "South Branch" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Time.deployed = replace(Time.deployed, Station == "Shinneys" & Recording == "Hobo Weather Station", as_date("2023-05-01"))) |>
  # mutate(Station = replace(Station, Recording == "Hobo Weather Station" & is.na(Time.deployed), NA)) |>
  ## correct serial number
  mutate(Serial = replace(Serial, Station == "South Branch" & Recording == "Hobo Weather Station", 21560026
  )) |>
  filter(!is.na(Station)) |>
  distinct()

## remove 2023 doubles
removes <- full.loggers |> filter(Serial %in% c(21436593, 21560027)) |>
  group_by(Serial) |>
  filter(Date.deployed.MDY == min(Date.deployed.MDY)) |>
  ungroup() |>
  mutate(remove = "yes")

# this removes doubles, na's and streamlines variables - it appears in code (sometimes commented out) for 1_Ppartner*.R, 1_TN_*.R, 1_cp_*.R, and in 1_logger-data-combo 
current.loggers <- full.loggers|>
  group_by(Station, Lat, Long, Recording) |>
  # filter(Time.deployed == max(Time.deployed) | is.na(Time.deployed)) |>
  ungroup() |>
  mutate(Group = factor(Group, levels = c("CAFE", "C&P", "FABEC", "SAEN", "Individual")),
         Recording = factor(Recording, levels = c("Water", "Air", "Level", "Hobo Weather Station"))) |>
  ## remove the first campbellton cellular deployement
  # filter(Station != "Campbellton" | Recording != "Hobo Weather Station" | Logger.Type == "Cellular Station") |>
  arrange(Recording, Group) |>
  dplyr::select(-X.1, -X.2, -lat, -long) |>
  distinct() |>
  left_join(removes) |> ## get rid of the doubles
  filter(is.na(remove)) |>
  filter(!is.na(Serial)) |>
  mutate(Serial = as.character(Serial)) |>
  dplyr::select(Station, Lat, Long, Date.deployed.MDY, Time.deployed, Recording, Serial, Logger.Type,
                Depth.cm, Location.description, SFA, River.Number, River.Name, dnld_dt, re_dply, Group) |>
  ## remove extra white spaces
  mutate(Serial = str_trim(Serial))

rm(list = c('full.loggers', 'loggers2022', 'loggers22only', 'missing', 'removes', 'saen', 'sfas', 'hobo24'))
