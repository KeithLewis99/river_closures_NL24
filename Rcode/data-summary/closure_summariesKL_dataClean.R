# In water23
# removes Char Brook - not a salmon river; - For Char Brook in 2023, data are from start to 2023-07-18 (Pendant) and from 2023-08-25 to end (Partner; AROC)  
## removes Rattling (River.Number == 47) which is not a salmon river
## removes Northwest Gander tributary (River.Number == 50)


# In water24
## removes Grandy's River which for 2024, has only a few days of data(River.Number == 125)
## removes Rattling (River.Number == 47) which is not a salmon river



# Looked at Sand Hill River.  Probably deployment date was 2024-07-08 but the temp is NA until 2024-06-17
## removed 7/8/2024 11:00 because temp ~ 26 deg C
season24 |>
   filter(River.Name == "Sand Hill River" & Time >= "2024-07-08 01:00" & Time < "2024-07-08 23:00") |>
   select(River.Name, Time, Serial, Temp.C) 

str(partner.data)
partner.data |>
   filter(Station == "Sandhill" & Date.Time >= "2024-07-08 01:00" & Date.Time < "2024-07-08 23:00") |>
   select(Station, Serial, Date.Time, Temp.C) 
# fix in closure_summary.R:import partner.data

# Shinney 
## temp from 2023-06-01 to 2023-06-05 is erratic - fluctuations up to 19 and 24 and no data for 3rd or 4th of June
## start at 6/5/2023 23:00
### 2023: Curtis to but below 10 from 2023-06-05  to 2023-06-20
## for 2024, temp is all NA
# fix in 3_temperature_data_clearing_Pt2_2024.Rmd

# Burlington
water_lev |>
   filter(River.Name == "Burlington River" & Time.UTC > as.Date("2024-06-20")) |>
   select(Time.UTC, Level.m, Temp.C, Serial) # 6 hours after water_lev comes on

water_lev |>
   filter(River.Name == "Burlington River" & Time.UTC > as.Date("2024-09-04")) |>
   select(Time.UTC, Level.m, Temp.C, Serial) # 6 hours after water_lev comes on 2024-09-05 12:00:00

df20 |>
   filter(River.Name == "Burlington River" & Time > as.Date("2024-09-05")) |>
   select(Time, Temp.C) # 2024-09-06 16:30:00 - this is 6 hours before data go weird
# fix in 3_temperature_data_clearing_Pt2_2024.Rmd


# Tommy's Arm 
## For 2023-07-10 to 2023-07-19, the water level is negative and so removed
#- 2024-06-03 19:00:00 - 6 hours -> 2024-06-04 01:00:00

temp <- water24 |>
   filter(River.Name == "Tommy's Arm River")
temp |>
   filter(!(River.Name == "Tommy's Arm River" & Time.UTC < as.Date("2024-06-05"))) |>
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)

water24 |>
   filter(River.Name == "Tommy's Arm River" & Time.UTC < as.Date("2024-06-03")) |>
   select(Time.UTC, Level.m, Temp.C, Serial)

water24.remove |>
   filter(River.Name == "Tommy's Arm River" & Time.UTC > as.Date("2024-06-03")) |>
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)
tail(water24.remove |>
   filter(River.Name == "Tommy's Arm River") |>
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial))
head(water24.remove |>
        filter(River.Name == "Tommy's Arm River") |>
        select(River.Name, Time.UTC, Level.m, Temp.C, Serial))

water24 |>
   filter(River.Name == "Tommy's Arm River" & Time.UTC > "2024-06-03") |>
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)

water_lev |>
   filter(River.Name == "Tommy's Arm River" & Time.UTC > as.Date("2024-06-03")) |>
   select(Time.UTC, Level.m, Temp.C, Serial)


# fix in closure_summary.R:import water24

# Gander 
## 2023: rapid rise in waterlevel resulted in sensor repositioning
##- station is on the fringe of coverage range.  When it goes out, data is only stored for a few days so gap in 2024; notes suggest cutting off a few days at begining but I see no need - check with Curtis
## Serial # 21931681 was used to make these graphs
season24 |> 
   filter(River.Name == "Gander River") |> 
   distinct(Serial)


## NW Gander - removed
## Salmon Brook - removed
# fix in closure_summary.R:import water 23 & water 24

## Salmon River - values below 10 deg C in early June
### data series ends 2023-07-27 but not sure if data have been removed at an earlier stage

## Traverse Brook - gap bc out of water; remove

## NE Brook - remove
# # fix in closure_summary.R:import water 23 & water 24


# Renews River: large temperature swings and quick increase in water level from very low levels: filter(!(River.Name == "Renews River" & Time.UTC < as.Date("2024-06-25")))

## Salmonier - 2024-06-01 22.35 deg C and 15.43 deg C, 2024-06-02 - 19.9, 
## All high values appear to be with Serial #  21038064 & # 21042585 - zap (confirmed by Curtis)
## also has a partner logger - should see how correlated they are
df20 |>
   filter(River.Name == "Salmonier River" & Time < as.Date("2023-06-05")) |>
   select(Time, Temp.C) # 2024-09-06 16:30:00 - this is 6 hours before data go weird
# # fix in closure_summary.R:import water 24


## Northeast River, Placentia - the change in the pattern in late August 2024 is due to the rest of the time series being an average of a logger Serial # 21031923 and 21159047 which stops on 8/28/2024 14:00 - so, given the differences in the two time series, its looks a little odd when you go to just one

# Piercy's Brook: early dates in mid June show fluctuations up to 25 degrees and to less that 14 in one day- filter(!(River.Name == "Piercey's Brook" & Time.UTC < as.Date("2024-06-25")))

# Garnish #  early dates in mid June show fluctuations up to 24 degrees and to less that 17 in one day - filter(!(River.Name == "Garnish River" & Time.UTC < as.Date("2024-06-24"))) water level available in 2023 but data for graph may be from logger, not cell station - perhaps reason not in graphs


## South Branch of Grand Codroy - > 30 in 2024 - this is an obvious mistkae - leave as and example.  Clearly, we can tell by the water level 
## cut off at 6/26/2024 18:00 + 6 hrs which is the spike in water temp - 
## water level in fall goes negative at 9/4/2024 13:00

water24 |>
   filter(River.Name == "South Branch of Great Codroy") |>
   filter(!(Time.UTC < as.Date("2024-06-27")) & !(Time.UTC > as.Date("2024-09-03"))) |> #& Time.UTC < as.Date("2024-06-30") 
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)

water24 |>
   filter(River.Name == "South Branch of Great Codroy" & 
             !(Time.UTC < as.Date("2024-06-27")) & 
             !(Time.UTC > as.Date("2024-09-03"))) |> #& Time.UTC < as.Date("2024-06-30") 
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)


temp <- water24 |> 
   filter(!(River.Name == "South Branch of Great Codroy" & 
             (Time.UTC < as.Date("2024-06-27") | 
             Time.UTC > as.Date("2024-09-03")))) |> 
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)
temp |>
   filter(River.Name == "South Branch of Great Codroy")   


df20 |>
   filter(River.Name == "South Branch of Great Codroy" & 
             !(Time < as.Date("2024-06-27")) & 
             !(Time > as.Date("2024-09-03"))) |> #& Time.UTC < as.Date("2024-06-30") 
   select(River.Name, Time, Temp.C)

tail(water24 |>
        filter(River.Name == "South Branch of Great Codroy" & 
                  !(Time.UTC < as.Date("2024-06-27")) & 
                  !(Time.UTC > as.Date("2024-09-03"))) |> #& Time.UTC < as.Date("2024-06-30") 
        select(River.Name, Time.UTC, Level.m, Temp.C, Serial))


water24 |>
   filter(!River.Name == "South Branch of Great Codroy" & 
             !(Time.UTC > as.Date("2024-06-27")) & 
             !(Time.UTC < as.Date("2024-09-03"))) |> #& Time.UTC < as.Date("2024-06-30") 
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)

## Little Barachois Brook
# gap due to out of water from 8/25/2024 21:00 to 8/28/2024 23:00


# Humber
# Temp.C is.na from 2024-05-31 to 6/26/2024 16:00
## Temp.c starts at 6/26/2024 17:00 (which has high value 26.91 deg C) before Level.m which starts 6/26/2024 20:00.  So start at 6/26/2024 20:00 + 6 hrs

water24 |>
   filter(River.Name == "Humber River") |>
   select(River.Name, Time.UTC, Level.m, Temp.C, Serial)

water24 |>
   filter(River.Name == "Humber River" & Time.UTC < as.Date("2024-06-30") & Time.UTC > as.Date("2024-06-25"))

df20 |>
   filter(River.Name == "Humber River" & Time < as.Date("2024-06-30") & Time > as.Date("2024-06-25"))
   
season24 |> 
   filter(River.Name == "Humber River") |> 
   distinct(Serial)

# Grey River
## Chelsea concerned with early days of 2023 - cut off < 2023-06-03
partner.data |>
   filter(River == "Grey River") |>
   select(River, Date.UTC, Time.UTC, Temp.C, Serial)


df20 |>
   filter(River.Name == "Grey River") |>
   select(River.Name, Time, Temp.C)

season24 |>
   filter(River.Name == "Grey River") |>
   select(River.Name, Time, Temp.C, Serial) |>
   distinct(Serial)

## Lomond
### is NA from 6/8/2023 17:00 to 6/10/2023 16:00
# At 6/17/2023 9:00 is 16.6 deg C (about that it has been in previous days) but by 6/17/2023 16:00 it is 22.35 and by 6/17/2023 20:00  it is 15.4 deg C. Stays at 13-15 deg C until 6/24/2023 11:00 when it jumps to 20.93 deg C and keeps going up. Question: snip everything before the drop or just the drop??? - Curtis said to snip it
unique(water23$River.Name)
unique(partner.data$River)

temp <- partner.data |>
   filter(River == "Lomond River") |>
   select(River, Date, Temp.C, Serial)

temp1 <- temp |> filter(!(Time.UTC > as.Date("2023-06-16") | 
                        Time.UTC < as.Date("2023-06-25")))
temp1[700:900,]

# St. Lewis - out of water from 7/15/2023 11:00 to 7/24/2023 5:00






