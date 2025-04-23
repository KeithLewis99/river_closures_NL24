### Terra Nova combo 2024 (and 2023)

# 2024 Temperature logger data processing for Terra Nova
# Purpose: combine data logger files into single CSV file for 2024 season
# Created by: Emilie Geissinger

# ---- Looad packages -----
library(tidyverse) 
library(lubridate) # working with dates


# ---- Upload station data ----
# rivers by sfa
sfas <- read.csv("./data-working/loggerSFAs.csv") |> 
  dplyr::select(-name)

# station data
loggers <- read.csv( "./data-working/Terra Nova Temperature Logger Folders/24-0414TempLoggerSummary.csv",
                         header = TRUE) |>
  rename(Station = "Logger.No....Location",
         Serial = "ID..",
         Deployed = Placed,
         Download2022 = X1st.Retrieval,
         Download2023 = X2nd.Retrieval) |>
  dplyr::select(-X3rd.Retrieval) |> 
  ## format columns
  separate(Coordinates, into = c("Lat", "Long"), sep = " ") |> 
  mutate(RiverCode = str_sub(Station, start = 1, end = 2)) |> 
  mutate(River = "Terra Nova River",
         River = replace(River, RiverCode == "GR", "Gambo River"),
         River = replace(River, RiverCode == "MB", "Middle Brook"),
         River = replace(River, RiverCode == "NW", "Northwest Brook, Alexander Bay")) |> 
  mutate(Deployed = mdy(Deployed),
         Download2022 = mdy(Download2022),
         Download2023 = mdy(Download2023)) |> 
  mutate(Lat = as.numeric(Lat),
         Long = as.numeric(Long)) |> 
  left_join(sfas, join_by(River == River.Name)) |> 
  distinct() |> 
  mutate(Recording = "Water",
         Recording = replace(Recording, Serial %in% c(21290096, 21401693, 21159036, 21401684, 21401681), "Air"))

# ---- Upload ----
dirs <- list.dirs( path = "./data-working/Partner-loggers/FABEC TNR Loggers/" )
dirs <- dirs[-1]
# read in list of files from each region

# air.dirs <- dirs[ grepl('air', dirs) ]
files <- list.files( path = dirs, pattern = ".csv", full.names = TRUE )

datalist <- lapply( files, read.csv, row.names = NULL, header = FALSE, skip = 1)

serial <- list.files(path = dirs, pattern = ".csv", full.names = FALSE)
## format serial
for (i in seq_along(serial)) {
  serial[[i]] <- serial[[i]] |> 
    as_tibble() |>
    separate(value, into = c('Serial', 'X1', 'X2', 'X3', 'X4'), sep = " ") |> 
    dplyr::select(Serial)
  }

# add logger serial number to each entry in list
for ( i in seq_along( datalist ) ) {

  datalist[[i]] <- cbind( datalist[[i]], serial[[i]] ) # serial number associated with each dataset added
}

# check data
head(datalist[[1]])
glimpse(datalist[[1]])

# combine all air data into single tibble
data.long <- datalist |>
  bind_rows() |>
  mutate( Date.Time = mdy_hms( V2 ),
          Temp.C = as.numeric( V3 ),
          Serial = as.integer(Serial)) |> 
  left_join(loggers, join_by(Serial)) |> 
  dplyr::select(Date.Time, Temp.C, Serial, Lat, Long, Station, Deployed, Download2022, Download2023, River, SFA, River.Number, Recording) |> 
  # left_join( current.loggers, by = c( 'Serial', 'Recording' ), relationship = "many-to-many" ) |>
  as_tibble() |> 
  ## make a separate date and time colummn (helps with excel issues)
  mutate(Date = date(Date.Time),
         Time = format(as.POSIXct(Date.Time), format = "%H:%M:%S")) |> 
  distinct()


write.csv(data.long,'./data-working/output/TNlogger-data-2023-full.csv',row.names = FALSE,na="")
