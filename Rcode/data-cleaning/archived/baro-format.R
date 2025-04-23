# ---- Baro data format ----


### packages
library(tidyverse)
library(lubridate)

check <- read.csv("./data-working/baro-data/Badger AUT/en_climate_hourly_NL_8400301_01-2023_P1H.csv")
check2 <- read.csv("./data-working/baro-data/Bonavista_LST/en_climate_hourly_NL_8400601_01-2023_P1H (2).csv")


check2 <- check |> 
  mutate(Date.Time.UTC = ymd_hm(Date.Time..UTC.),
         Date.Time.LST = as_datetime(Date.Time.UTC, tz = "America/St_Johns")) |> 
  arrange(Date.Time.LST) |> 
  mutate(Date = format(date(Date.Time.LST), "%m/%d/%Y"),
         Time = str_sub(Date.Time.LST, start = 12)) |> 
  rename("press (kPa)" = Stn.Press..kPa.) |> 
  dplyr::select(Date, Time, 'press (kPa)') |> 
  distinct()



### Environment Canada data
dirs <- list.dirs(path = "./data-working/baro-data/", recursive = FALSE)

for ( i in seq_along(dirs) ) {
  files <- list.files(path = dirs[i], full.names = TRUE)
  
  all_files <- lapply(files, read.csv)
  
  
  ### format for hobo level logger ### put into LST
  
  combined <- bind_rows(all_files) |> 
    mutate(Date.Time.UTC = ymd_hm(Date.Time..UTC.),
           Date.Time.LST = as_datetime(Date.Time.UTC, tz = "America/St_Johns")) |> 
    arrange(Date.Time.LST) |> 
    mutate(Date = format(date(Date.Time.LST), "%m/%d/%Y"),
           Time = str_sub(Date.Time.LST, start = 12)) |> 
    rename("press (kPa)" = Stn.Press..kPa.) |> 
    dplyr::select(Date, Time, 'press (kPa)') |> 
    distinct()
  
  write.csv(combined, paste("./data-working/baro-data/", basename(dirs[i]), "_combined.csv", sep =""), row.names = FALSE)
}



### Hobo Weather Station data




### data

# ## Station data
# source("./Rcode/data-cleaning/metadata-setup.R")
# 
# stations <- loggers2023 |> 
#   filter(eq_type == "Cellular Station" | eq_type == "Wifi Station" | eq_type == "Cellular") |> 
#   # fix garnish serial number
#   filter(name != "Garnish River") |> 
#   mutate(name = replace(name, serial_no == 21667530, "Garnish River")) |> 
#   # fix Campbellton serial number
#   mutate(serial_no = replace(serial_no, name == "Campbellton", 21436593)) |> 
#   mutate(name = replace(name, name == "NUMBER 90", "Campbellton")) |> 
#   # adjust terra nova sn
#   mutate(serial_no = replace(serial_no, name == "Terra Nova Lower Fishway", 21560027)) |> 
#   # fix trepassey Serial number
#   mutate(serial_no = replace(serial_no, name == "Trepassey NW Brook", 21436597)) |> 
#   # select first deployment entry for each station
#   group_by(serial_no) |> 
#   filter(dply_dt_tm == min(dply_dt_tm)) |> 
#   ungroup() |> 
#   mutate(long = x,
#          lat = y)



## temperature data
source("./Rcode/data-cleaning/hobolink_station_combo.R")

# hobo <- bind_rows(hobo.list) |> 
  # mutate(Time_UTC = mdy_hm(Time_UTC)) |> 
hobo2 <- hobo |> 
  ## create standard time column
  mutate(Time_LST = with_tz(Time_UTC, "America/St_Johns")) |> 
  ## remove duplicates
  distinct() |> 
  arrange(Time_LST) |> 
  mutate(Time2 = as.character(Time_LST)) |>
  separate(Time2, into = c("Date", "Time"), sep = " ") |> 
  mutate(Time = replace(Time, is.na(Time), "00:00:00"),
         Date = format(ymd(Date), "%m/%d/%Y")) |>
  mutate(Station = replace(Station, Station == "Salmon Brook Near Glenwood", "Salmon Brook"),
         Station = replace(Station, Station == "Terra Nova Lower Fishway", "Terra Nova")) |> 
  mutate(Station = paste(Station, Serial, sep = " ")) |> 
  dplyr::select(Station, Date, Time, BarometricPressure_kPa) |> 
  filter(!is.na(BarometricPressure_kPa)) |> 
  rename("press (kPa)" = BarometricPressure_kPa) 


#---- export to excel -----
library(openxlsx)

outputdata <- split(hobo2, hobo2$Station)

wb <- createWorkbook()

for (i in 1:length(outputdata)) {
  addWorksheet(wb, sheetName=names(outputdata[i]))
  writeData(wb, sheet=names(outputdata[i]), x=outputdata[[i]] |> dplyr::select(-Station)) # Note [[]]
}

saveWorkbook(wb, "./data-working/baro-data/cellular_barometric.xlsx", overwrite = TRUE)
