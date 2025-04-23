# Format meta data from field app output for Temperature project
## this file draws upon data from the Equipment Tracking Application aka the Field App which is downloaded from ESRI by Neil Ollerhead; this file then needs to be run through the code in the FieldApp folder on the shared drive to clean up the data; the Sites, Equipment, Downloads csv files are then extracted from here

# ---- packages -----
library(tidyverse)


# ---- data -----
#sites <- read.csv("../FieldApp/Sites.csv") |> 
 # dplyr::select(-objectid)
sites <- read.csv("./FieldApp/Sites.csv") |> 
   dplyr::select(-objectid)

#equipment <- read.csv("../FieldApp/Equipment.csv") |>   dplyr::select(-name)
# this is originally from ArcGIS from Neil Oilerhead - the Excel sheet was created by Emilie after cleaning
equipment <- read.csv("./FieldApp/Equipment.csv") |>   dplyr::select(-name)

#downloads <- read.csv("../FieldApp/Downloads.csv")
downloads <- read.csv("./FieldApp/Downloads.csv")


# this file links SFA, River Number, River Name, and names that we use.  SalmonRivers.csv matched with our stations names was entered manuualy.  Note that apostrophe's in the SalmonRivers.csv do not translate well in R - need to modify.  
sfa <- read.csv("./data-working/loggerSFAs.csv") |> 
  distinct()


# ---- Temperature metadata -----
## Sites globalid relates to equipment eq_id
## Equipement globalid relates to downloads dnld_id

field.data <- equipment |> 
  left_join(sites, by = join_by("eq_id" == "globalid")) |> 
  filter(eq_class == "Temp" | eq_class == "Other" | eq_class == "") |>
  filter(eq_class_other != "Test") |> 
  dplyr::select(name, serial_no, eq_type, recording, locn_desc, dply_dt_tm, depth_m, dply_by, lat, long, x, y)
  
data.downloads <- downloads |> 
  left_join(equipment, by = join_by("dnld_id" == "globalid")) |> 
  # globalid is globalid from downloads
  # dnld_id for data.downloads is same as globalid from equipment
  left_join(sites, by = join_by("eq_id" == "globalid")) |> 
  filter(eq_class == "Temp" | eq_class == "Other" | eq_class == "") |>
  filter(eq_class_other != "Test") |> 
  dplyr::select(name, dnld_dt, re_dply, notes, dnld_by, serial_no, eq_type, recording, locn_desc, dply_dt_tm, depth_m, dply_by, lat, long, x, y)


## Fix cellular station data ----
loggers <- field.data |> 
  
  ## fix garnish serial number
  filter(name != "Garnish River") |> 
  mutate(name = replace(name, serial_no == 21667530, "Garnish River")) |> 
  
  ## fix Campbellton serial number
  mutate(serial_no = replace(serial_no, name == "Campbellton", 21436593)) |> 
  mutate(name = replace(name, name == "NUMBER 90", "Campbellton")) |> 
  
  ## correct terra nova sn
  mutate(serial_no = replace(serial_no, name == "Terra Nova Lower Fishway", 21560027)) |> 
  
  ## add South Branch SN
  ## remove extra space after
  mutate(name = replace(name, name == "South Branch ", "South Branch"),
         serial_no = replace(serial_no, name == "South Branch", 21560019)) |> 
  
  ## fix trepassey Serial number
  mutate(serial_no = replace(serial_no, name == "Trepassey NW Brook", 21436597)) |> 
  
  ## fix Burlington (remove space after name)
  mutate(name = replace(name, name == "Burlington ", "Burlington")) |> 
  
  ## fix Tommy's Arm (remove the weird apostrophe)      
  mutate(name = replace(name, name == "Tommy’s Arm River", "Tommy's Arm River")) |> 
  ## add Grandy's
  add_row(name = "Grandys",
          serial_no = 21011380,
          recording = "Air",
          x = -58.842316,
          y = 47.621799,
  ) |> 
  ### fix Exploits detph line
  ### correct loggers with missing station info
  mutate(name = replace(name, serial_no == 21097834, "Exploits GF FB 4m"),
         name = replace(name, serial_no == 21031926, "Exploits GF FB 1m"),
         name = replace(name, serial_no == 21031918, "Exploits GF FB top"),
         name = replace(name, serial_no == 21031921, "Exploits GF FB 5m")) |> 
  mutate(eq_type = replace(eq_type, serial_no == 21097834, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031926, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031918, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031921, "Pendant")) |> 
  left_join(sfa)

# overlapping objects - OK for now; EG just being lazy (her words)
downloads <- data.downloads |> 
  
  ## fix garnish serial number
  filter(name != "Garnish River") |> 
  mutate(name = replace(name, serial_no == 21667530, "Garnish River")) |> 
  
  ## fix Campbellton serial number
  mutate(serial_no = replace(serial_no, name == "Campbellton", 21436593)) |> 
  mutate(name = replace(name, name == "NUMBER 90", "Campbellton")) |> 
  
  ## correct terra nova sn
  mutate(serial_no = replace(serial_no, name == "Terra Nova Lower Fishway", 21560027)) |> 
  
  ## add South Branch SN
  ## remove extra space after
  mutate(name = replace(name, name == "South Branch ", "South Branch"),
         serial_no = replace(serial_no, name == "South Branch", 21560019)) |> 
  
  ## fix trepassey Serial number
  mutate(serial_no = replace(serial_no, name == "Trepassey NW Brook", 21436597)) |> 
  
  ## fix Burlington (remove space after name)
  mutate(name = replace(name, name == "Burlington ", "Burlington")) |> 
  
  ## fix Tommy's Arm (remove the weird apostrophe)      
  mutate(name = replace(name, name == "Tommy’s Arm River", "Tommy's Arm River")) |> 
  ### fix Exploits detph line
  ### correct loggers with missing station info
  mutate(name = replace(name, serial_no == 21097834, "Exploits GF FB 4m"),
         name = replace(name, serial_no == 21031926, "Exploits GF FB 1m"),
         name = replace(name, serial_no == 21031918, "Exploits GF FB top"),
         name = replace(name, serial_no == 21031921, "Exploits GF FB 5m")) |> 
  mutate(eq_type = replace(eq_type, serial_no == 21097834, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031926, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031918, "Pendant"),
         eq_type = replace(eq_type, serial_no == 21031921, "Pendant")) |> 
  
  left_join(sfa)

loggers <- loggers |> 
  left_join(downloads)

rm(sites, equipment, sfa, field.data, data.downloads)

# END ----