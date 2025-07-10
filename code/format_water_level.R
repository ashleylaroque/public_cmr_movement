
# Format water water level data

# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# # Load data
# ## data from occasion 1-10
# drive_download("05_12_23_AirTemp",
#                type = "csv",
#                path = "data_raw/data_air_temp1.csv",
#                overwrite = T)
# 
# drive_download("05_12_23_WaterTemp",
#                type = "csv",
#                path = "data_raw/data_water_temp1.csv",
#                overwrite = T)
# 
# ## data from occasion 11+
# drive_download("20240930_air",
#                type = "csv",
#                path = "data_raw/data_air_temp2.csv",
#                overwrite = T)
# 
# drive_download("20240930_water",
#                type = "csv",
#                path = "data_raw/data_water_temp2.csv",
#                overwrite = T)

# data --------------------------------------------------------------------

# Read data
df_air <- list.files("data_raw",
                     pattern = "data_air",
                     full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  transmute(dt = as.POSIXct(paste(date, time),
                            format = "%m/%d/%y %H:%M:%OS",
                            tz = "EST"),
            air_pres = abs_pres, 
            air_temp = temp)

df_water <- list.files("data_raw",
                       pattern = "data_water",
                       full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  transmute(dt = as.POSIXct(paste(date, time),
                            format = "%m/%d/%y %H:%M:%OS",
                            tz = "EST"),
            water_pres = abs_pres, 
            water_temp = temp)

# Calculate water level
## `df_water` has more rows than `df_air`
## - `df_water` must be a baseline data frame
## convert air and water pressure (from absolute pressure kPa to barometric mmHg) to water level (m) 
## NOTE: water level is not usable at this moment due to air pressure issues
df_hourly <- df_water %>% 
  left_join(df_air, by = "dt") %>%
  mutate(water_level = (13.595 * (water_pres - air_pres)) / 1000, 
         date = as.Date(dt))

# Calculate daily mean temperature and water level
df_daily <- df_hourly %>% 
  group_by(date) %>%
  summarize(n = n(),
            daily_water_temp = median(water_temp, na.rm = TRUE),
            daily_water_pres = median(water_pres, na.rm = TRUE))

# Add occasion by sampling date
## occasion was added based on the interval between fish sampling date
v_date <- read_csv("data_raw/data_cmr_src.csv") %>% 
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  group_by(Occasion) %>% 
  slice(which.min(date)) %>% 
  pull(date)

## option one
date_obs <- df_daily$date
occasion <- rep(NA, length(date_obs))

for (i in 1:(length(v_date) - 1)) {
  tf <- between(date_obs, v_date[i], v_date[i + 1] - 1)
  occasion[tf] <- i
}

df_daily <- df_daily %>% 
  mutate(occasion = occasion) 


# ## option two
# water_dat %>% 
#   mutate(occasion = case_when(between(date, v_date[1], v_date[2]) ~ 1,
#                               between(date, v_date[2], v_date[3]) ~ 2,
#                               between(date, v_date[3], v_date[4]) ~ 3,
#                               ...)
#          )


# Calculate temperature and water level by occasion
df_occ <- df_daily %>%
  group_by(occasion) %>%
  summarize(mean_temp = mean(daily_water_temp, na.rm = TRUE), # uses median from each day
            max_temp = max(daily_water_temp, na.rm = TRUE),
            sd_temp = sd(daily_water_temp, na.rm = TRUE),
            mean_wpres = mean(daily_water_pres, na.rm = TRUE),
            max_wpres = max(daily_water_pres, na.rm = TRUE),
            sd_wpres = sd(daily_water_pres, na.rm = TRUE)) %>% 
  mutate(st_occ = occasion,
         en_occ = occasion + 1) %>% 
  drop_na(occasion)

# Export file
saveRDS(df_occ, file = "data_fmt/data_water_hobo.rds")


# figure ------------------------------------------------------------------

# Hourly

## Water temperature
ggplot(df_hourly, aes(x = dt, y = water_temp)) + 
  geom_line(size = 0.8, color = "red") +
  scale_x_datetime(date_breaks = "60 day", labels = date_format("%m-%d-%y")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Hourly temperature (c)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"), 
        legend.position = c(0.10, 0.80))  # remove grid

## Water level
ggplot(df_hourly, aes(x = dt, y = water_pres)) + 
  geom_line(size = 0.8, color = "blue") +
  scale_x_datetime(date_breaks = "60 day", labels = date_format("%m-%d-%y")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Hourly water level (m)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"), 
        legend.position = c(0.10, 0.80))

# Daily mean plot

## Water temperature
ggplot(df_daily, aes(x = date, y = daily_water_temp)) + 
  geom_line(size = 0.8, color = "red") +
  scale_x_date(date_breaks = "60 day", labels = date_format("%m-%d-%y")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Mean daily temperature (c)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"), 
        legend.position = c(0.10, 0.80))

## Water level
ggplot(df_daily, aes(x = date, y = daily_water_pres)) + 
  geom_line(size = 0.8, color = "blue") +
  scale_x_date(date_breaks = "60 day", labels = date_format("%m-%d-%y")) +
  scale_y_continuous("Water temperature") +
  ggtitle("Mean daily water level (m)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"), 
        legend.position = c(0.10, 0.80))

