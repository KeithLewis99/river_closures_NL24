### Load C&P Data


# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates

dirs <- list.dirs( path = "./data-working/C & P 2024/" ) # figure out how to automatically select relevant directory
dirs <- dirs[-1]

# water files
waterfiles <- list.files( path = dirs, pattern = ".csv", full.names = TRUE)
water.list <- lapply( waterfiles, read.csv, row.names = NULL, header = TRUE )

files.short <- list.files( path = dirs, pattern = ".csv", full.names = FALSE)



meta1 <- as.data.frame(files.short) |>
  separate(files.short, into = c('v1', 'v2', 'v3', 'v4', 'v5', sep = "_")) |>
  rename(Serial1 = v1,
         Serial2 = v2,
         Serial3 = v3) |> 
  mutate(Serial1 = as.integer(Serial1),
         Serial2 = as.integer(Serial2),
         Serial3 = as.integer(Serial3)) |> 
  mutate(Serial2 = replace(Serial2, Serial2 == 2024, NA),
         Serial3 = replace(Serial3, Serial3 == 10, NA)) |> 
  mutate(Serial = coalesce(Serial1, Serial2, Serial3)) |> 
  dplyr::select(Serial)

serial <- split(meta1, 1:NROW(meta1))


meta <- read.csv("./data-working/C & P 2024/2024 C& P Metadata.csv")

# add logger serial number to each entry in list
for ( i in seq_along( water.list ) ) {
  
  water.list[[i]] <- cbind( water.list[[i]], serial[[i]] )  # serial number associated with each dataset added
  
}

# combine water files into single tibble
cpTemp <- water.list |>
  bind_rows() |>
  rename(Time.NDT = Date.Time..NDT.,
         Temp.C = Temperature.....C.) |>
  # select(Time, Temp.C, Serial) |>
  left_join(meta, join_by(Serial == Logger.SN)) |> 
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble() |> 
  mutate(Time = mdy_hms(Time)) |> 
  filter(!is.na(River)) |> 
  mutate(Deployment.Date = mdy(Deployment.Date)) |> 
  filter(date(Time) >= Deployment.Date | is.na(Deployment.Date))

rm(list = c('meta', 'meta1', 'serial', 'water.list', 'dirs', 'files.short', 'i', 'waterfiles'))

