# 2024 Temperature logger data processing for partner loggers
# Purpose: combine data logger files into single CSV file for 2023 season
# Created by: Emilie Geissinger

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates

# ------- metadata ----------------
file.short <- list.files(path = "./data-working/Partner-loggers/", pattern = ".csv", full.names = FALSE)

meta <- as.data.frame(file.short) |> 
  separate(file.short, into = c('v1', 'v2', 'v3', 'v4'), sep = "_", remove = FALSE) |> 
  rename(Partner = v1,
         Station = v2,
         Serial = v3,
         offload = v4) |> 
  mutate(Partner = replace(Partner, Station == "EagleRiver", "Rifflin Hitch Lodge"),
         Partner = replace(Partner, Partner == "Water", "AROC"),
         Partner = replace(Partner, Station == "MacclesMouth", 'AROC')) |> 
  add_column(River = c('Grey River',
                   'Eagle River',
                   'Eagle River',
                   'Eagle River',
                   'Eagle River',
                   'Sand Hill River',
                   'Sand Hill River',
                   'Lomond River',
                   'Northeast River, Placentia',
                   'North Arm River, Holyrood',
                   'Salmonier River',
                   'Traverse Brook',
                   'Char Brook',
                   'Char Brook',
                   'Char Brook',
                   'Eagle River',
                   'Eagle River',
                   'Eagle River',
                   'Hunt River',
                   'Hunt River',
                   'Hunt River',
                   'Hunt River',
                   'Hunt River',
                   'Hunt River',
                   'Terra Nova River',
                   'St. Lewis River',
                   'St. Lewis River')) |> 
  dplyr::select(-offload)
  
positions <- data.frame(River = c("Terra Nova River", "Grey River", "Eagle River", "Sand Hill River", 'Lomond River', 'Northeast River, Placentia',
                                  'North Arm River, Holyrood', 'Salmonier River', 'Traverse Brook', 'Char Brook', 'Hunt River', 'St. Lewis River'),
                        Lat = c(48.6, 47.7, 53.5, 53.58747, 49.40222, 47.3,
                                47.4, 47.2, 48.5, 55.6, 55.5, 52.4),
                        Long = c(-54.1, -56.9, -57.5, -56.35394, -57.729906, -53.8,
                                 -53.1, -53.4, -54.1, -60.9, -60.7, -56.4))

sfa <- read.csv("./data-working/loggerSFAs.csv") |> 
  dplyr::select(-name)

metadata <- meta |> 
  left_join(positions) |> 
  left_join(sfa, join_by(River == River.Name)) |> 
  mutate(SFA = replace(SFA, River == "Sand Hill River", "2"),
         River.Number = replace(River.Number, River == "Sand Hill River", 11),
         SFA = replace(SFA, River == "Lomond River", "14A"),
         River.Number = replace(River.Number, River == "Lomond River", 154))


# ---- Upload: Water ----

# water files
waterfiles <- list.files( path = "./data-working/Partner-loggers/", pattern = ".csv", full.names = TRUE)
water.list <- lapply( waterfiles, read.csv, row.names = NULL, header = FALSE, skip = 1 )


# add logger serial number to each entry in list
for ( i in seq_along( water.list ) ) {

  water.list[[i]] <- cbind( water.list[[i]], file.short[[i]] )  # serial number associated with each dataset added
  
}

# combine water files into single tibble
water.long <- water.list |>
  bind_rows() |>
  rename(file.short = 'file.short[[i]]') |> 
  mutate(Recording = 'Water' ) |>
  rename(Time = V2,
         Temp.C = V3 ) |>
  select(Time, Temp.C, Recording, file.short) |>
  left_join(metadata) |> 
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble() |> 
  dplyr::select(-file.short)


write.csv(water.long,'./data-working/output/partner-logger-data-2024-full.csv',row.names = FALSE,na="")

