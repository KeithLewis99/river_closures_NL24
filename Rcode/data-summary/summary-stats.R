### Temperature Data summary code

### To calculate:
## Daily and ## Monthly

## Mean, Min, Max by River, by SFA



# ---- packages ----
library(tidyverse)
library(lubridate)


# --- load data ----
water <- read.csv("./data-working/output/compiled-water-temperature-13Nov2023.csv") |> 
  mutate(Time = replace(Time, is.na(Time), "00:00:00"),
         Time = ymd_hms(paste(Date, Time, sep = " ")),
         Date = ymd(Date)) |> 
  filter(Year >= 2022) |> 
  filter(!is.na(Temp.C)) |> 
  mutate(Temp.C = replace(Temp.C, out.of.water == 1, NA))


# ----- summary data -----
## daily stats
daily <- water |> 
  group_by(Station, SFA, River.Number, River.Name, Date) |> 
  summarise(Daily_Temp = mean(Temp.C, na.rm = TRUE), sd_Temp = sd(Temp.C, na.rm = TRUE),
            Tmax = max(Temp.C, na.rm = TRUE), Tmin = min(Temp.C, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(month = month(Date)) |> 
  # filter(SFA != 11) |> 
  filter(Station != "Amy's Lake") |> 
  filter(!is.na(River.Number)) |> 
  filter(!is.na(SFA)) |> 
  mutate(year = year(Date)) |> 
  filter(!is.na(Daily_Temp)) 


daily_river <- daily |> 
  group_by(River.Name, SFA, River.Number, year, Date) |> 
  summarise(Mean_Temp = mean(Daily_Temp, na.rm = TRUE), TempSD = sd(Daily_Temp, na.rm = TRUE),
            Max = mean(Tmax, na.rm = TRUE), MaxSD = sd(Tmax, na.rm = TRUE), Min = mean(Tmin, na.rm = TRUE), MinSD = sd(Tmin, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(River.Name) |> 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |> 
  ungroup()


monthly_river <- daily |> 
  filter(!is.na(SFA)) |> 
  # filter(SFA != 11) |> 
  # filter(Station != "Amy's Lake") |> 
  # filter(!is.na(River.Number)) |> 
  # remove stations with less than 15 days within a month
  group_by(Station, SFA, River.Name, River.Number, year, month) |> 
  mutate(Days = n()) |> 
  ungroup() |> 
  filter(Days >= 15) |>
  # monthly stats by SFA 
  mutate(Month = month(Date, label = TRUE)) |> 
  group_by(River.Name, River.Number, year, month, Month) |> 
  summarise(Mean_Temp = mean(Daily_Temp, na.rm = TRUE), TempSD = sd(Daily_Temp, na.rm = TRUE),
            Max = mean(Tmax, na.rm = TRUE), MaxSD = sd(Tmax, na.rm = TRUE), Min = mean(Tmin, na.rm = TRUE), MinSD = sd(Tmin, na.rm = TRUE)) |> 
  ungroup() |> 
  # mutate(Date = my(paste(month, year, sep = "-"))) |> 
  filter(year >= 2022) |>
  filter(month == 5 | month == 6 | month == 7 | month == 8 | month == 9) |> 
  mutate(Date = ymd(paste(year, month,15))) |> 
  group_by(River.Name) |> 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 month")) |> 
  ungroup()


sfaprep <- water |> 
  mutate(Temp.C = replace(Temp.C, out.of.water == 1, NA)) |>
  filter(Station != "Amy's Lake") |>
  filter(!is.na(SFA)) |>
  # filter(!is.na(Temp.C)) |> 
  # set up date variables for summary
  mutate(Year = year(Time),
         Date = date(Time),
         Month = month(Time, label = TRUE),
         Month.full = as.character(month(Time, label = TRUE, abbr = FALSE)),
         Month.num = month(Time)) |> 
  # daily min and daily max of each station
  group_by(Station, Date, Year, Month, Month.num, Month.full, SFA, River.Number, River.Name) |>
  summarise(DailyMax = max(Temp.C, na.rm = TRUE),
            DailyMin = min(Temp.C, na.rm = TRUE),
            DailyMean = mean(Temp.C, na.rm = TRUE),
            DailySD = sd(Temp.C, na.rm = TRUE),
            DailyLevel = mean(Level.m, na.rm = TRUE)) |> 
  ungroup() |> 
  # remove stations with less than 15 days within a month
  group_by(Station, Year, Month, Month.num, Month.full) |> 
  mutate(Days = n()) |> 
  ungroup() |> 
  filter(Days >= 15) 

sfa1 <- sfaprep |>
  # monthly stats by SFA 
  group_by(Year, Month, Month.num, Month.full, SFA) |> 
  summarise(Mean_Temp = mean(DailyMean, na.rm = TRUE), SD_Temp = sd(DailyMean, na.rm = TRUE),
            Mean_DailyMax = mean(DailyMax, na.rm = TRUE), SD_DailyMax = sd(DailyMax, na.rm = TRUE),
            Mean_DailyMin = mean(DailyMin, na.rm = TRUE), SD_DailyMin = sd(DailyMin, na.rm = TRUE),
            Mean_Level = mean(DailyLevel, na.rm = TRUE), SD_Level = sd(DailyLevel, na.rm = TRUE),
            Obs.Days = round(mean(Days, na.rm = TRUE))) |> 
  ungroup() |> 
  mutate(across(is.numeric, ~round(.x, 1))) |> 
  filter( Month.num == 6 | Month.num==7 | Month.num==8 | Month.num==9) |> 
  dplyr::select(-Month.num, -Month.full)

# sfa_sum <- sfa1 |> 
#   mutate(across(contains("_"), ~ round(.x, 1))) |> 
#   ## Select Summer season only
#   filter(Month.num >= 6 & Month.num <= 9) |> 
#   ## format table
#   mutate(Mean_Temp = paste(Mean_Temp, " (", SD_Temp, ")", sep = ""),
#          Mean_DailyMin = paste(Mean_DailyMin, " (", SD_DailyMin, ")", sep = ""),
#          Mean_DailyMax = paste(Mean_DailyMax, " (", SD_DailyMax, ")", sep = ""),
#          Mean_Level = paste(Mean_Level, " (", SD_Level, ")", sep = "")) |> 
#   ## change NAs to empty spots
#   mutate(Mean_Level = replace(Mean_Level, Mean_Level == "NA (NA)", ""),
#          Mean_Level = replace(Mean_Level, Mean_Level == "NaN (NA)", "")) |> 
#   dplyr::select(SFA, Year, Month, Mean_Temp, Mean_DailyMin, Mean_DailyMax, Mean_Level, Obs.Days)

# sfa_sum22 <- sfa_sum |> 
#   filter(Year == 2022) |> 
#   rename_with(~ paste0(.x, "2022"), 4:8) |> 
#   dplyr::select(-Year)
# 
# 
# sfa_sum23 <- sfa_sum |> 
#   filter(Year == 2023) |> 
#   rename_with(~ paste0(.x, "2023"), 4:8) |> 
#   dplyr::select(-Year)
# 
# sfaFULL <- full_join(sfa_sum22, sfa_sum23) |> 
#   ungroup() |> 
#   arrange(SFA, Month) |> 
#   dplyr::select(SFA, Month, Mean_Temp2022, Mean_Temp2023, Mean_DailyMin2022, Mean_DailyMin2023, Mean_DailyMax2022, Mean_DailyMax2023, Mean_Level2022, Mean_Level2023, Obs.Days2022, Obs.Days2023)
