# 2024 Temperature logger data processing 
# Purpose: combine  (blue tooth) data logger files into single CSV file for 2023 season
# Created by: Emilie Geissinger

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates

source("./Rcode/data-cleaning/0_logger-metadata-2024.R")

# directories within current working directory 2022-11-16
# dirs <- list.dirs( path = "./data-working/Temperature-Files/2024-11-12/" ) # figure out how to automatically select relevant directory

dirs <- list.dirs(path = "./Drop Temeprature Files Here/")
allfiles <- list.files(path = dirs, pattern = ".csv", full.names = TRUE)

# file data (name, serial number, type, location)
file.short <- list.files(path = dirs, pattern = ".csv", full.names = FALSE)

meta <- as.data.frame(file.short) |> 
  separate(file.short, into = c('v1', 'v2', 'v3', 'v4'), sep = "_", remove = FALSE) |> 
  rename(Recording = v1,
         Station = v2,
         Serial = v3,
         offload = v4) |> 
  mutate(Recording = str_trim(Recording),
         Station = str_trim(Station),
         Serial = str_trim(Serial),
         offload = str_remove(offload, ".csv")) |> 
  ## format date
  mutate(offload = dmy(offload)) |> 
  # add a space in station based on lower case/upper case; for example "LowerFishway" becomes "Lower Fishway"
  mutate(Station = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", Station)) 

# ---- Upload: Air ----

# read in list of files from each region

airfiles <- allfiles[ grepl("Air", allfiles) ]
## short names of air files
air.short <- file.short[ grepl("Air", file.short) ]
air.list <- lapply(airfiles, read.csv, row.names = NULL, header = FALSE, skip = 1)


# add logger serial number to each entry in list
for ( i in seq_along( air.list ) ) {
  
  air.list[[i]] <- cbind( air.list[[i]], air.short[[i]] ) # add file name to each datafile (for later use)
  air.list[[i]] <- air.list[[i]] |> 
    rename(air.short = "air.short[[i]]") |> 
    mutate(V3 = as.character(V3)) |> 
    mutate(V1 = as.integer(V1))
}

# check data
head(air.list[[1]])
glimpse(air.list[[1]])

# combine all air data into single tibble
air.long <- air.list |>
  bind_rows() |>
  mutate( Date.Time = mdy_hms( V2 ),
         Temp.C = as.numeric( V3 ) ) |>
  left_join(meta, join_by(air.short == file.short)) |> 
  dplyr::select( Date.Time, Temp.C, Serial, Recording, Station, offload) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# ---- Upload: Water ----

# water files
# water.dirs <- dirs[ grepl( 'water', dirs ) ]
# waterfiles <- list.files( path = water.dirs, pattern = ".csv", full.names = TRUE )
# water.list <- lapply( waterfiles, read.csv, row.names = NULL, header = FALSE, skip = 9 )
# # first few columns of each file (to extra SN)
# water.serial <- lapply( waterfiles, read.csv, row.names = NULL, header = FALSE, skip = 0, nrows = 1,
#                           col.names = c("Serial"))

waterfiles <- allfiles[ grepl("Water", allfiles) ]
## short names of air files
water.short <- file.short[ grepl("Water", file.short) ]
water.list <- lapply(waterfiles, read.csv, row.names = NULL, header = FALSE, skip = 1)


## Star Odi loggers
## files 16-19
## these files have different date and aren't separated properly

for (i in c(16:19)) {
  water.list[[i]] <- water.list[[i]] |> 
    # V2 is the decimal of the tempearture (not sure why it comes out in this format)
    ## rename V2 temporarily
    rename(V4 = V2) |> 
    separate(V1, into = c("V1", "V2", "V3"), sep = ";") |> 
    mutate(V3 = paste(V3, V4, sep = ".")) |> 
    dplyr::select(-V4)
}



# prep to add logger serial number to each entry in list
for ( i in seq_along( water.list ) ) {

  water.list[[i]] <- cbind( water.list[[i]], water.short[[i]] ) # add file name to each datafile (for later use)
  water.list[[i]] <- water.list[[i]] |> 
    rename(water.short = "water.short[[i]]") |> 
    mutate(V3 = as.character(V3)) |> 
    mutate(V1 = as.integer(V1))
  
}

head(water.list[[2]])



## format files with different date
## 16-19
for (i in c(16:19)) {
  water.list[[i]] <- water.list[[i]] |> 
    mutate(V2 = dmy_hms(V2)) |> 
    mutate(V3 = as.numeric(V3))
}

## format date for all other files
for (i in c(1:15, 20:25)) {
  water.list[[i]] <- water.list[[i]] |> 
    mutate(V2 = mdy_hms(V2)) |> 
    mutate(V3 = as.numeric(V3))
}

# combine water files into single tibble
water.long <- water.list |>
  bind_rows() |>
  mutate( Date.Time = V2,
          Temp.C = as.numeric( V3 ) ) |>
  left_join(meta, join_by(water.short == file.short)) |> 
  dplyr::select( Date.Time, Temp.C, Serial, Recording, Station, offload) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# ----- Upload: Level ----

 
levelfiles <- allfiles[ grepl("Level", allfiles) ]
## short names of air files
level.short <- file.short[ grepl("Level", file.short) ]
level.list <- lapply(waterfiles, read.csv, row.names = NULL, header = FALSE, skip = 1)



# add to add logger serial number to each entry in list
for ( i in seq_along( level.list ) ) {
  
  level.list[[i]] <- cbind( level.list[[i]], level.short[[i]] ) # add file name to each datafile (for later use)
  level.list[[i]] <- level.list[[i]] |> 
    rename(level.short = "level.short[[i]]") |> 
    mutate(V3 = as.character(V3)) |> 
    mutate(V1 = as.integer(V1))
  
}

# combine into a single tibble
level.long <- level.list |>
  bind_rows() |>
  mutate( Date.Time = V2,
          Temp.C = as.numeric( V3 ) ) |>
  left_join(meta, join_by(level.short == file.short)) |> 
  dplyr::select( Date.Time, Temp.C, Serial, Recording, Station, offload) |>
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble()

# ---- full data  ----
head( air.long )
head( water.long )
# head( level.long )
# head(labLogs)


# combine "long" files into singe file
tlong <- water.long |>
  bind_rows( air.long ) |> 
  filter(!is.na(Temp.C)) |>
  # bind_rows( level.long ) |>
  ## remove extra white spaces
  mutate(Serial = str_trim(Serial)) |> 
  ## Join logger metadata
  left_join(current.loggers, join_by(Serial, Recording)) |> 
  arrange(SFA, River.Number, Date.Time) |> 
  # filter(!is.na(Date.Time)) |> 
  ## make a separate date and time colummn (helps with excel issues)
  mutate(Date = date(Date.Time),
         Time = format(as.POSIXct(Date.Time), format = "%H:%M:%S")) |> 
  distinct() #|> 
  # filter(!is.na(Serial))

## check station names from metadata
tlong |> filter(Station.x != Station.y) |> distinct(Station.x, Station.y) |> print(n = "Inf")

## correct names
tlong2 <- tlong |> 
  mutate(Station.x = replace(Station.x, Station.x == "NEPlacentia", "NE Placentia"),
         Station.x = replace(Station.x, Station.x == "Conne Camp1", "Conne Camp 1"),
         Station.x = replace(Station.x, Station.x == "South Brook Burego", "South Brook Burgeo"),
         Station.x = replace(Station.x, Station.x == "NEPlacentia", "NE Placentia"),
         Station.x = replace(Station.x, Station.x == "Crabbes River", "Crabbs River, Burgeo"),
         Station.x = replace(Station.x, Station.x == "Highlands", "Highlands River"),
         Station.x = replace(Station.x, Station.x == "Main River", "Main River, Sops Arm"),
         Station.x = replace(Station.x, Station.x == "Carolineby Flatwater", "Caroline by Flatwater"),
         Station.x = replace(Station.x, Station.x == "NW Gander", "Northwest Gander"),
         Station.x = replace(Station.x, Station.x == "NWGander Bridge", "NW Gander Bridge"),
         Station.x = replace(Station.x, Station.x == "Northwest Brook Mile Pool3", "Northwest Brook Mile Pool 3"),
         Station.x = replace(Station.x, Station.x == "Northwest Brook Mile Pool1", "Northwest Brook Mile Pool 1"),
         Station.x = replace(Station.x, Station.x == "Northwest Brook Mile Pool2", "Northwest Brook Mile Pool 2"),
         Station.x = replace(Station.x, Station.x == "Grandys", "Grandys Brook")) |> 
  rename(Station = Station.x) |> 
  dplyr::select(-Station.y)

write.csv(tlong2,'./data-working/output/logger-data-2024-full.csv',row.names = FALSE,na="")

