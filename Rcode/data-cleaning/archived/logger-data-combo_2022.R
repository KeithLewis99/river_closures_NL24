# 2022 Temperature data processing 
# Purpose: combine data logger files into single CSV file for 2022 season
# Created by: Emilie Geissinger

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates


# ---- Upload data ----

# rivers by sfa
sfas <- read.csv("./data-working/loggerSFAs.csv")

# station data
station <- read.csv( "./data-working/Loggers_2022.csv",
                     skip = 1,
                     header = TRUE) |>
  rename(Logger.Type = "Logger.Type.Note..minilog...level.are.not.bluetooth") |>
  dplyr::select(2:17) |> 
  dplyr::select(-SFA) |> 
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long),
         Serial = as.integer(Serial)) |> 
  rename(Station = River) |>
  left_join(sfas, join_by(Station == name))


dirs <- list.dirs( path = "./data-working/Temperature-Files/Files2022/" ) # figure out how to automatically select relevant directory




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
  Serial <- air.serial[[i]][1,1]
  air.list[[i]] <- cbind( air.list[[i]], Serial ) # serial number associated with each dataset added
  air.list[[i]] <- air.list[[i]] %>% mutate(V3 = as.character(V3))
}

# check data
head(air.list[[1]])

## fix 14 by removing extra 2 rows
head(air.list[[14]])
air.list[[14]] <- air.list[[14]][-1:-2,]
air.list[[14]] <- air.list[[14]] |> 
  mutate(V1 = as.integer(V1))
glimpse(air.list[[14]])


# combine all air data into single tibble
air.long <- air.list |>
  bind_rows() |>
  mutate( Date.Time = mdy_hms( V2 ),
         Recording = 'Air',
         Temp.C = as.numeric( V3 ) ) |>
  dplyr::select( Date.Time, Temp.C, Serial, Recording) |>
  left_join( station, by = c( 'Serial', 'Recording' ) ) |>
  as_tibble()

# ---- Upload: Water ----

# Minilog files
minilog.dirs <- dirs[ grepl( 'minilog', dirs ) ]
minilogfiles <- list.files( path = minilog.dirs, pattern = ".csv", full.names = TRUE )
minilog.list <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 9 )
# first few columns of each file (to extra SN)
minilog.serial <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 0, nrows = 1,
                          col.names = c("Serial"))

# 
# # create an empty list to store logger serial numbers
# minilog.serial <- vector( mode = "list", length = length( minilog.head ) )

# # extract the serial number from each entry in the list
# for ( i in seq_along( minilog.head ) ) {
#   
#   minilog.serial[[i]] <- minilog.head[[i]] |>
#     mutate( Serial = as.numeric( gsub( ".*?([0-9]+).*", "\\1", V1 ) ) ) |> # select numberic values only
#     select( Serial )
#   
# }

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
  left_join( station, by = c( 'Serial', 'Recording' ) ) |>
  # mutate( Station = paste( River, Serial, sep = ": " ) ) |>
  as_tibble()

# tidbit

# read in list of files from each region
tidbit.dirs <- dirs[ grepl( 'tidbit', dirs ) ]
tidbitfiles <- list.files( path = tidbit.dirs, pattern = ".csv", full.names = TRUE )
tidbit.list <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, skip = 5 )
# list of serial numbers
tidbit.serial <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                   col.names = c( 'Serial' ) )



# check tidbit files (always the challenge)

glimpse(tidbit.list[[1]])
glimpse(tidbit.list[[23]])

# fixes to tidbit files
# need to resolve in Unix before upload for future years

tidbit.list[[13]] <- tidbit.list[[13]] |> 
  dplyr::select(V1, V2) |> 
  rename( V3 = V2,
          V2 = V1) |> 
  mutate( V1 = seq(1:n()))

# tidbit list 14 through 17 only has 3
  
tidbit.list[[28]] <- tidbit.list[[28]] |> 
  dplyr::select(V1, V2) |> 
  rename( V3 = V2,
          V2 = V1) |> 
  mutate( V1 = seq(1:n()))



# add logger serial number to each entry in list
for ( i in seq_along( tidbit.list ) ) {
  
  Serial <- tidbit.serial[[i]][1,]
  tidbit.list[[i]] <- tidbit.list[[i]][,1:3]
  tidbit.list[[i]] <- cbind( tidbit.list[[i]], Serial ) |>
    mutate( V1 = as.character( V1 ),
            V3 = as.numeric(V3) ) # serial number associated with each dataset added
  
}

head( tidbit.list[[1]] )

# dates of tidbit files are messed up. Take ones that are in diff format (ymd_hms) and match others (mdy_hms)
# 12
tidbit.list[[13]] <- tidbit.list[[13]] |> 
  mutate(Date = str_sub(V2, start = 1, end = 10),
         Time = str_sub(V2, start = 12, end = 19)) |> 
  separate(Date, into = c("Year", "Month", "Day")) |> 
  mutate( V2 = paste( paste( Month, Day, Year, sep = "-"), Time ) ) |> 
  dplyr::select(V1, V2, V3, Serial)

# 27
tidbit.list[[28]] <- tidbit.list[[28]] |> 
  mutate(Date = str_sub(V2, start = 1, end = 10),
         Time = str_sub(V2, start = 12, end = 19)) |> 
  separate(Date, into = c("Year", "Month", "Day")) |> 
  mutate( V2 = paste( paste( Month, Day, Year, sep = "-"), Time ) ) |> 
  dplyr::select(V1, V2, V3, Serial)

# 40 (format time)
tidbit.list[[44]] <- tidbit.list[[44]] |> 
  mutate(V2 = paste(V2, "00", sep = ":"))


# long format
water.tidbit.long <- tidbit.list |>
  bind_rows() |>
  mutate( Recording = 'Water',
          Temp.C = as.numeric( V3 ),
          Date.Time = mdy_hms(V2)) |>
  select( Date.Time, Temp.C, Serial, Recording ) |>
  left_join( station, by = c( 'Serial', 'Recording' ) ) |>
  as_tibble()

# ----- Uplaod: Level ----

# read in list of files from each region
level.dirs <- dirs[ grepl( 'level', dirs ) ]
levelfiles <- list.files( path = level.dirs, pattern = ".csv", full.names = TRUE )
level.list <- lapply( levelfiles, read.csv, row.names = NULL, header = FALSE, skip = 3 )
# first few rows of each datafile to later extract serial number
level.serial <- lapply( levelfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                        col.names = c("Serial"))


# add logger serial number to each entry in list
for ( i in seq_along( level.list ) ) {
  
  level.list[[i]] <- level.list[[i]] |>
    select( 1:4 ) |>
    mutate( V3 = as.numeric( V3 ),
            V4 = as.numeric( V4 ) ) |>
    bind_cols( level.serial[[i]] ) # serial number associated with each dataset added
  
}


# combine into a single tibble
level.long <- level.list |>
  bind_rows() |>
  mutate( Date.Time.UTC = mdy_hms( V2, tz = "UTC" ),
          Recording = 'Level' ) |>
  rename( Temp.C = V3, 
          Level.m = V4 ) |>
  mutate(Date.Time = with_tz(Date.Time.UTC,  "America/St_Johns")) |> 
  select( Date.Time, 
          Temp.C, 
          Level.m, 
          Serial, 
          Recording ) |>
  mutate( Serial = replace( Serial, is.na( Serial ), 21040183) ) |> # need to resolve this. shouldn't need to correct for a single occurance
  left_join( station, by = c( 'Serial', 'Recording' ) ) 

# combine water temperature 
# minilog, tidbit, level



water.long <- bind_rows( water.minilog.long, water.tidbit.long )

# ---- full data  ----
head( air.long )
head( water.long )
head( level.long )


# ## logger SFAs
# sfa <- read.csv("./data-working/loggerSFAs.csv")

# combine "long" files into singe file
tlong <- level.long |>
  bind_rows( water.long ) |>
  bind_rows( air.long ) |> 
  # dplyr::select(-SFA, -River.Number, -River.Name) |> 
  # left_join(sfa, join_by(Station == name)) |> 
  arrange(SFA, River.Number, Date.Time) |> 
  filter(!is.na(Date.Time)) |> 
  ## make a separate date and time colummn (helps with excel issues)
  mutate(Date = date(Date.Time),
         Time = format(as.POSIXct(Date.Time), format = "%H:%M:%S")) |> 
  ## remove the date.downloaded. redeployed column
  dplyr::select(-Date.Downloaded, -Redeployed..Y.N.)

write.csv(tlong,'./data-working/output/logger-data-2022-full.csv',row.names = FALSE,na="")

