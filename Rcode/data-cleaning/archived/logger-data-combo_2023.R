# 2023 Temperature logger data processing 
# Purpose: combine data logger files into single CSV file for 2023 season
# Created by: Emilie Geissinger

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates


# ---- Upload station data ----
# rivers by sfa
sfas <- read.csv("./data-working/loggerSFAs.csv")

# station data
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
  distinct()

## 2023 station data
source("./Rcode/data-cleaning/metadata-setup.R") 


loggers2023 <- loggers2023 |> 
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
  dplyr::select(-depth_m, -dply_by, -dply_dt_tm) 


## SAEN station data
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
  distinct()

## loggers that are not reocrded in FieldApp
loggers22only <- loggers2022 |> 
  filter(!Serial %in% loggers2023$Serial)


full.loggers <- bind_rows(loggers22only, loggers2023, saen) |> 
  mutate(Recording = replace(Recording, Logger.Type == "Cellular" | Logger.Type == "Cellular Station" | Logger.Type == "Wifi Station", "Hobo Weather Station"),
         Recording = replace(Recording, Logger.Type == "Level", "Level"),
         Recording = replace(Recording, Logger.Type == "Pendant" & Recording == "", "Water"),
         Recording = replace(Recording, Logger.Type == "Tidbit" & Recording == "", "Water")) |> 
  # filter(Logger.Type != "Minilog") |> 
  add_row(Station = "Shinneys", Lat = 52.59578, Long = -56.05356, River.Name = "Shinney's River", Recording = "Hobo Weather Station", Group = "CAFE") |> 
  mutate(River.Name = replace(River.Name, River.Name == "", NA)) |> 
  mutate(Lat = round(Lat, 3),
         Long = round(Long, 3)) |>
  ## add a datetime to Shinneys and South Branch
  mutate(Date.deployed.MDY = replace(Date.deployed.MDY, Station == "South Branch" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Date.deployed.MDY = replace(Date.deployed.MDY, Station == "Shinneys" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Time.deployed = replace(Time.deployed, Station == "South Branch" & Recording == "Hobo Weather Station", as_date("2023-05-01")),
         Time.deployed = replace(Time.deployed, Station == "Shinneys" & Recording == "Hobo Weather Station", as_date("2023-05-01"))) |> 
  mutate(Station = replace(Station, Recording == "Hobo Weather Station" & is.na(Time.deployed), NA)) |> 
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
  left_join(removes) |> 
  filter(is.na(remove)) |> 
  dplyr::select(-remove) |> 
  filter(!is.na(Serial)) 

# directories within current working directory 2022-11-16
dirs <- list.dirs( path = "./data-working/Temperature-Files/Files2023/" ) # figure out how to automatically select relevant directory


# ---- Upload: Air ----

# read in list of files from each region

air.dirs <- dirs[ grepl('air', dirs) ]
airfiles <- list.files( path = air.dirs, pattern = ".csv", full.names = TRUE )
air.list <- lapply( airfiles, read.csv, row.names = NULL, header = FALSE, skip = 2 )
# list of serial numbers
air.serial <- lapply ( airfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                   col.names = c( 'Serial' ) )


# add logger serial number to each entry in list
for ( i in seq_along( air.list ) ) {
  
  air.list[[i]] <- cbind( air.list[[i]], air.serial[[i]] ) # serial number associated with each dataset added
  air.list[[i]] <- air.list[[i]] |>  mutate(V3 = as.character(V3))
}

# check data
head(air.list[[1]])
glimpse(air.list[[1]])

# combine all air data into single tibble
air.long <- air.list |>
  bind_rows() |>
  mutate( Date.Time = mdy_hms( V2 ),
         Recording = 'Air',
         Temp.C = as.numeric( V3 ) ) |>
  dplyr::select( Date.Time, Temp.C, Serial, Recording) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# ---- Upload: Water ----

# Minilog files
minilog.dirs <- dirs[ grepl( 'minilog', dirs ) ]
minilogfiles <- list.files( path = minilog.dirs, pattern = ".csv", full.names = TRUE )
minilog.list <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 9 )
# first few columns of each file (to extra SN)
minilog.serial <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 0, nrows = 1,
                          col.names = c("Serial"))



# add logger serial number to each entry in list
for ( i in seq_along( minilog.list ) ) {

  minilog.list[[i]] <- cbind( minilog.list[[i]], minilog.serial[[i]] ) # serial number associated with each dataset added
  
}

head(minilog.list[[2]])

# combine miniog files into single tibble
water.minilog.long <- minilog.list |>
  bind_rows() |>
  mutate( Date.Time = ymd_hms( paste( V1, V2 ) ),
          Recording = 'Water' ) |>
  rename( Temp.C = V3 ) |>
  select( Date.Time, Temp.C, Serial, Recording ) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# tidbit

# read in list of files from each region
tidbit.dirs <- dirs[ grepl( 'tidbit', dirs ) ]
tidbitfiles <- list.files( path = tidbit.dirs, pattern = ".csv", full.names = TRUE )
tidbit.list <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, skip = 4 )
# list of serial numbers
tidbit.serial <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                   col.names = c( 'Serial' ) )



# check tidbit files (always the challenge)

glimpse(tidbit.list[[1]])
glimpse(tidbit.list[[23]])



# add logger serial number to each entry in list
for ( i in seq_along( tidbit.list ) ) {
  
  tidbit.list[[i]] <- tidbit.list[[i]] |> 
    dplyr::select( 1:3 ) |>
    mutate( V1 = as.character(V1),
            V3 = as.numeric( V3 ) ) |>
    bind_cols( tidbit.serial[[i]][1,1] ) |>  # serial number associated with each dataset added
    rename(Serial = `...4`)
  
}

head( tidbit.list[[1]] )
# file 20 to file 25

for (i in 20:25) {
  tidbit.list[[i]] <- tidbit.list[[i]] |>
    mutate(V2 = paste(V2, ":00", sep = ""))
}


# long format
water.tidbit.long <- tidbit.list |>
  bind_rows() |>
  mutate( Recording = 'Water',
          Temp.C = as.numeric( V3 ),
          Date.Time = mdy_hms(V2)) |>
  select( Date.Time, Temp.C, Serial, Recording ) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many") |>
  as_tibble()

# ----- Uplaod: Level ----

# read in list of files from each region
level.dirs <- dirs[ grepl( 'level', dirs ) ]
## remove dirs "Hobo level data
level.dirs <- level.dirs[!grepl("Hobo level data", level.dirs)]
levelfiles <- list.files( path = level.dirs, pattern = ".csv", full.names = TRUE )
level.list <- lapply( levelfiles, read.csv, row.names = NULL, header = FALSE, skip = 3 )
# first few rows of each datafile to later extract serial number
level.serial <- lapply( levelfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                        col.names = c("Serial"))

level.list[[2]]$V4 <- NA
level.list[[5]]$V4 <- NA

# add logger serial number to each entry in list
for ( i in seq_along( level.list ) ) {
  
  level.list[[i]] <- level.list[[i]] |>
    dplyr::select( 1:4 ) |>
    mutate( V1 = as.character(V1),
            V3 = as.numeric( V3 ),
            V4 = as.numeric( V4 ) ) |>
    bind_cols( level.serial[[i]][1,1] ) |>  # serial number associated with each dataset added
    rename(Serial = `...5`)
  
}


# combine into a single tibble
level.long <- level.list |>
  bind_rows() |>
  mutate( Date.Time.UTC = mdy_hms( V2, tz = "UTC" ),
          Recording = 'Level' ) |>
  mutate(Date.Time = with_tz(Date.Time.UTC,  "America/St_Johns")) |> 
  rename( Temp.C = V3, 
          Level.m = V4 ) |>
  select( Date.Time, 
          Temp.C, 
          Level.m, 
          Serial, 
          Recording ) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# combine water temperature 
# minilog, tidbit, level

water.long <- bind_rows( water.minilog.long, water.tidbit.long )


### ----- Upload: Labrador ----
source("./Rcode/data-cleaning/logger_data_lab_2023.R")

# ---- full data  ----
head( air.long )
head( water.long )
head( level.long )
head(labLogs)

# labrador stations

labstations <- labLogs |> 
  dplyr::select(Station, Serial, SFA, River.Number, River.Name, Group) |> 
  mutate(SFA = as.character(SFA)) |> 
  distinct() |> 
  mutate(Serial = replace(Serial, is.na(Serial), 0)) |> 
  filter(!Station %in% current.loggers$Station)

lab.long <- labLogs |> 
  mutate(Serial = replace(Serial, Station == "Hunt River Loggers", 0)) |> 
  dplyr::select(-SFA, -River.Number, -River.Name, -Group, -Station)

### missing serial numbers in datafile
temp1 <- read.csv("./data-working/Temperature-Files/addedFiles2023/21233617 2023-12-08 12_30_41 NST (Data NST)(1).csv",
                  skip = 1, header = FALSE) |> 
  mutate(Date.Time = mdy_hms(V2)) |> 
  rename(Temp.C = V3) |> 
  mutate(Serial = 21233617,
         Recording = "Water") |> 
  dplyr::select(Date.Time, Temp.C, Serial, Recording)



# ------ Extra files -----
## add in files that were emailed separately (aka not on Teams)



# combine "long" files into singe file
tlong <- water.long |>
  bind_rows(temp1) |> 
  bind_rows( air.long ) |> 
  filter(!is.na(Temp.C)) |>
  bind_rows( level.long ) |>
  bind_rows(lab.long) |> 
  left_join(current.loggers |> 
 bind_rows(labstations) |> distinct() |> 
              dplyr::select(-Recording), join_by(Serial)) |> 
  arrange(SFA, River.Number, Date.Time) |> 
  # filter(!is.na(Date.Time)) |> 
  ## make a separate date and time colummn (helps with excel issues)
  mutate(Date = date(Date.Time),
         Time = format(as.POSIXct(Date.Time), format = "%H:%M:%S")) |> 
  dplyr::select(-Date.Downloaded, -Redeployed..Y.N.) |> 
  distinct() #|> 
  # filter(!is.na(Serial))

write.csv(tlong,'./data-working/output/logger-data-2023-full.csv',row.names = FALSE,na="")

