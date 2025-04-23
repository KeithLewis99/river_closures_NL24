
source("./Rcode/data-cleaning/hobolink_station_combo.R")
source("./Rcode/data-cleaning/hobolink_station_cleaning.R")

hobo <- hobo |> 
  mutate(River.Name = replace(River.Name, Station == "Shinneys", "Shinneys Brook"))

rivers <- c("Exploits River", "Northwest Brook, Trepassey", "Shinneys Brook", "Terra Nova River")

# 
# data22 <- hobo |> 
#   filter(River.Name %in% rivers) |> 
#   filter(!is.na(WaterTemperature_C)) |> 
#   filter(year(Time) == 2022) |> 
#   group_by(Station) |> 
#   complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
#   ungroup() |> 
#   arrange(Time)
# 
# data23 <- hobo |> 
#   filter(River.Name %in% rivers) |> 
#   filter(!is.na(WaterTemperature_C)) |> 
#   filter(year(Time) == 2023) |> 
#   group_by(Station) |> 
#   complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
#   ungroup() |> 
#   arrange(Time)


# ggplot(data22) +
#   geom_point(aes(x = Time, y = WaterTemperature_C)) +
#   facet_wrap(~Station, scales = "free", nrow = 1)
# 
# ggplot(data23) +
#   geom_line(aes(x = Time, y = WaterTemperature_C)) +
#   facet_wrap(~Station, scales = "free", nrow = 1)

## Shinneys
p1a <- t3 |> 
  filter(Station == "Shinneys River") |> 
  filter(Time >= as_datetime("2022-06-01 00:00:00")) |> 
  # group_by(Station) |> 
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = Temp.C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Shinneys River 2022") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

p2a <- hobo |> 
  filter(Station == "Shinneys") |> 
  filter(!is.na(WaterTemperature_C)) |>
  filter(year(Time) == 2023) |> 
  # group_by(Station) |>
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |>
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Shinneys River 2023") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

## Exploits
p1b <- hobo |> 
  filter(Station == "Exploits Bishops Fishway") |> 
  filter(!is.na(WaterTemperature_C)) |> 
  filter(year(Time) == 2022) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Exploits Bishops Fishway 2022") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

p2b <- hobo |> 
  filter(Station == "Exploits Bishops Fishway") |> 
  filter(!is.na(WaterTemperature_C)) |> 
  filter(year(Time) == 2023) |> 
  # group_by(Station) |> 
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Exploits Bishops Fishway 2023") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)


p1c <- t3 |> 
  filter(Station == "Goodyears Dam") |> 
  filter(Time <= as_datetime("2022-10-14 00:00:00")) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes(x = Time, y = Temp.C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Exploits Goodyears Dam 2022") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

p2c <- hobo |> 
  filter(Station == "Exploits Goodyears Dam") |> 
  filter(!is.na(WaterTemperature_C)) |> 
  filter(year(Time) == 2023) |> 
  # group_by(Station) |> 
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Exploits Goodyears Dam 2023") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)
  
## Terra Nova
p1d <- hobo |> 
  filter(Station == "Terra Nova Lower Fishway") |> 
  filter(!is.na(WaterTemperature_C)) |>
  filter(year(Time) == 2022) |> 
  filter(Time <= as_datetime("2022-11-01 00:0000")) |> 
  # group_by(Station) |>
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |>
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Terra Nova Lower Fishway 2022") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

p2d <- hobo |> 
  filter(Station == "Terra Nova Lower Fishway") |> 
  # filter(!is.na(WaterTemperature_C)) |>
  filter(year(Time) == 2023) |> 
  filter(Time >= as_datetime("2023-06-01 00:00:00")) |> 
  # group_by(Station) |>
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |>
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Terra Nova Lower Fishway 2023") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

## Trepassey NW Brook
p1e <- t3 |> 
  filter(Station == "Trepassey") |>
  filter(!is.na(Temp.C)) |> 
  # group_by(Station) |> 
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |> 
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = Temp.C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Trepassey Northwest Brook 2022") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)

p2e <- hobo |> 
  filter(Station == "Trepassey NW Brook") |> 
  filter(!is.na(WaterTemperature_C)) |>
  filter(year(Time) == 2023) |> 
  filter(Time >= "2023-05-15 00:00:00") |> 
  # group_by(Station) |>
  # complete(Time = seq.POSIXt(min(Time), max(Time), by = "1 hour")) |>
  ggplot() +
  geom_hline(yintercept = 20, color = 'red', linetype = 'dashed') +
  geom_line(aes( x = Time, y = WaterTemperature_C)) +
  theme_bw() +
  labs(x = "Time",
       y = "Water Temperature (°C)",
       title = "Trepassey Northwest Brook 2023") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,31)


library(ggpubr)

p1 <- ggarrange(p1a, p1b, p1c, p1d, p1e, nrow = 1)
p1

p2 <- ggarrange(p2a, p2b, p2c, p2d, p2e, nrow = 1)
p2

ggsave(p1, file ="./output/rivers2022.png", device = "png", dpi =250, height = 4, width = 16, unit = "in")
ggsave(p2, file ="./output/rivers2023.png", device = "png", dpi =250, height = 4, width = 16, unit = "in")



### Summary stats
sum.stats1 <- hobo |> 
  filter(River.Name %in% rivers) |> 
  filter(!is.na(WaterTemperature_C)) |> 
  mutate(Month = month(Time, label = TRUE, abbr = FALSE),
         Month2 = month(Time),
         Year = year(Time)) |> 
  filter(Month2 >= 6 & Month2 <= 8) |> 
  group_by(Station, Year, Month) |> 
  summarise(mean = mean(WaterTemperature_C), 
            sd = sd(WaterTemperature_C), 
            min = min(WaterTemperature_C), 
            max = max(WaterTemperature_C)) |> 
  ungroup()

sum.stats1 |> distinct(Station)

sum.stats2 <- t3 |> 
  filter(Station == "Trepassey" | Station == "Shinneys River" | Station == "Goodyears Dam") |> 
  filter(!is.na(Temp.C)) |> 
  mutate(Month = month(Time, label = TRUE, abbr = FALSE),
         Month2 = month(Time),
         Year = year(Time)) |> 
  filter(Month2 >= 6 & Month2 <= 8) |> 
  group_by(Station, Year, Month) |> 
  summarise(mean = mean(Temp.C), 
            sd = sd(Temp.C), 
            min = min(Temp.C), 
            max = max(Temp.C)) |> 
  ungroup()

sum.stats <- bind_rows(sum.stats1, sum.stats2) |> filter(Year != 2021) |> 
  mutate(Station = replace(Station, Station == "Trepassey", "Trepassey NW Brook"),
         Station = replace(Station, Station == "Shinneys River", "Shinneys"),
         Station = replace(Station, Station == "Exploits Goodyears Dam", "Goodyears Dam"),
         Station = replace(Station, Station == "Exploits Bishops Fishway", "Bishops Fishway")) |> 
  mutate(Station = factor(Station, levels = c("Shinneys", "Bishops Fishway", "Goodyears Dam", "Terra Nova Lower Fishway", "Trepassey NW Brook")))

sum.fig <- ggplot(sum.stats,
       aes(x = Station, 
           y = mean, 
           shape = as.character(Year),
           fill = as.character(Year),
           ymin = mean - sd, 
           ymax = mean + sd)) +
  geom_point(aes(x = Station, 
                 y = min,
                 colour = as.character(Year)), 
             position = position_dodge(width = 0.5),
             shape = 8) +
  geom_point(aes(x = Station, 
                 y = max,
                 colour = as.character(Year)), 
             position = position_dodge(width = 0.5),
             shape = 8) +
  geom_errorbar(width = .1,
                position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) + 
  labs(x = "Station", 
       y = "Temperature (°C)",
       fill = "Year",
       shape = "Year",
       colour = "Year") +
  facet_wrap(~Month) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
  scale_fill_manual(values = c('grey50', 'black')) +
  scale_colour_manual(values = c('grey50', 'black')) +
  scale_shape_manual(values = c(22, 24))

sum.fig2 <- ggplot(sum.stats,
                  aes(x = Month, 
                      y = mean, 
                      shape = as.character(Year),
                      fill = as.character(Year),
                      ymin = mean - sd, 
                      ymax = mean + sd)) +
  geom_point(aes(x = Month, 
                 y = min,
                 colour = as.character(Year)), 
             position = position_dodge(width = 0.5),
             shape = 8) +
  geom_point(aes(x = Month, 
                 y = max,
                 colour = as.character(Year)), 
             position = position_dodge(width = 0.5),
             shape = 8) +
  geom_errorbar(width = .1,
                position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) + 
  labs(x = "Month", 
       y = "Temperature (°C)",
       fill = "Year",
       shape = "Year",
       colour = "Year") +
  facet_wrap(~Station, nrow = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
        legend.position = "bottom") +
  scale_fill_manual(values = c('grey50', 'black')) +
  scale_colour_manual(values = c('grey50', 'black')) +
  scale_shape_manual(values = c(22, 24))

ggsave(sum.fig2, file = "./output/summary2023.png", device = "png", height = 5, width = 14, unit = "in")
