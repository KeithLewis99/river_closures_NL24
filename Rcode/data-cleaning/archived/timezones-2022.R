#### Time zones

## create a file with the serial number and the time zone programed

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates


# ---- Upload data ----

dirs <- list.dirs( path = "./data-working/Temperature-Files/Files2022/" ) # figure out how to automatically select relevant directory


# ---- Upload: Air ----

# read in list of files from each region

air.dirs <- dirs[ grepl('air', dirs) ]
airfiles <- list.files( path = air.dirs, pattern = ".csv", full.names = TRUE )
air.TZ <- lapply( airfiles, read.csv, row.names = NULL, header = FALSE, skip = 1, nrows = 1 )
# list of serial numbers
air.serial <- lapply ( airfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                       col.names = c( 'Serial' ) )

air.list <- vector(mode = "list", length = length(air.TZ))
# add logger serial number to each entry in list
for ( i in seq_along( air.serial ) ) {
  Serial <- air.serial[[i]][1,1]
  TZ <- air.TZ[[i]]$V2
  DF <- as.data.frame(cbind(Serial, TZ))
  air.list[[i]] <- DF  # serial number associated with each dataset added
    
}

# combine all air data into single tibble
airTZ <- bind_rows(air.list) |>
  mutate( Recording = 'Air' ) |>
  as_tibble()

# ---- Upload: Water ----

# Minilog files
minilog.dirs <- dirs[ grepl( 'minilog', dirs ) ]
minilogfiles <- list.files( path = minilog.dirs, pattern = ".csv", full.names = TRUE )
minilog.TZ <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 4, nrows = 1 )
# first few columns of each file (to extra SN)
minilog.serial <- lapply( minilogfiles, read.csv, row.names = NULL, header = FALSE, skip = 0, nrows = 1,
                          col.names = c("Serial"))

minilog.list <- vector(mode = "list", length = length(minilog.TZ))
# add logger serial number to each entry in list
for ( i in seq_along( minilog.serial ) ) {
  Serial <- minilog.serial[[i]][1,1]
  TZ <- minilog.TZ[[i]]$V1
  DF <- as.data.frame(cbind(Serial, TZ))
  minilog.list[[i]] <- DF  # serial number associated with each dataset added
  
}

# combine all air data into single tibble
minilogTZ <- bind_rows(minilog.list) |>
  mutate( Recording = 'Water' ) |>
  as_tibble()


# tidbit

# read in list of files from each region
tidbit.dirs <- dirs[ grepl( 'tidbit', dirs ) ]
tidbitfiles <- list.files( path = tidbit.dirs, pattern = ".csv", full.names = TRUE )
tidbit.TZ <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, skip = 1, nrows = 1 )
# list of serial numbers
tidbit.serial <- lapply( tidbitfiles, read.csv, row.names = NULL, header = FALSE, nrows = 1,
                         col.names = c( 'Serial' ) )

tidbit.list <- vector(mode = "list", length = length(tidbit.TZ))
# add logger serial number to each entry in list
for ( i in seq_along( tidbit.serial ) ) {
  Serial <- tidbit.serial[[i]][1,1]
  TZ <- tidbit.TZ[[i]]$V2
  DF <- as.data.frame(cbind(Serial, TZ)) |> 
    mutate(Serial = as.character(Serial))
  tidbit.list[[i]] <- DF  # serial number associated with each dataset added
  
}

# combine all air data into single tibble
tidbitTZ <- bind_rows(tidbit.list) |>
  mutate( Recording = 'Water' ) |>
  as_tibble()



#### combind
TZs <- bind_rows(airTZ, minilogTZ, tidbitTZ)

write.csv(TZs, "./data-working/output/2022-timezones.csv", row.names = FALSE)
