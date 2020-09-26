library(jsonlite)
library(sf)
library(lubridate)
library(tidyverse)

# files <- list.files("data/raw/monthly")
# for (i in files) {
#   temp <- read_csv(paste0("data/raw/monthly/",i))
#   saveRDS(temp, paste0("data/raw/monthly/rds/",i,".RDS"))
# }
# 
# kyiv_speeds <- NULL
# kyiv_speeds <- readRDS("data/kyiv_speeds.RDS")
# 
# files <- list.files("data/raw/monthly/rds", full.names = TRUE)
# 
# for (k in files) {
#   temp <- readRDS(k)
#   kyiv_speeds <- kyiv_speeds %>% 
#     bind_rows(temp)
#   saveRDS(kyiv_speeds, paste0("data/kyiv_speeds.RDS"))
#   print(k)
# }
# 
# kyiv_speeds %>% 
#   select(utc_timestamp) %>% 
#   distinct() %>% 
#   mutate(month = floor_date(utc_timestamp, unit = "months")) %>% 
#   select(month) %>% 
#   distinct() %>% 
#   View()
#   
# kyiv_speeds %>% 
#   arrange(desc(utc_timestamp)) %>% 
#   head(100) %>% View()
# speeds_import <- purrr::map_dfr(files, readRDS)





# Kyiv geometry -----------------------------------------------------------

# kyiv_crs <- "ESRI:102013"

# This the CLI command for pulling geometries
# mdt create-geometry-file kyiv 2020 --output="kyiv.geojson"
kyiv_import <- st_read("data/kyiv.geojson") 

kyiv <- kyiv_import %>% 
  st_as_sf() %>% 
  rename(osm_start_node_id = osmstartnodeid,
         osm_end_node_id = osmendnodeid,
         osm_way_id = osmwayid)

kyiv_lengths <- kyiv %>% 
  select(osm_start_node_id:osm_way_id) %>% 
  mutate(length = st_length(.) %>% as.numeric(),
         length = length/1000)

st_geometry(kyiv_lengths) <- NULL

# Speed data --------------------------------------------------------------

speeds_import <- readRDS("data/kyiv_speeds.RDS")
speeds <- speeds_import %>% 
  inner_join(kyiv_lengths, by = c("osm_way_id", "osm_start_node_id", "osm_end_node_id")) %>% 
  mutate(time = length/speed_kph_mean,
         timestamp = with_tz(utc_timestamp, tzone = "Europe/Kiev")) %>% 
  select(-utc_timestamp)
  

speeds_max <- speeds %>% 
  filter(wday(timestamp) %in% c(1,7)) %>% 
  mutate(date = as.Date(timestamp)) %>% 
  group_by(osm_way_id, osm_start_node_id, osm_end_node_id, date) %>% 
  summarise(max_speed = max(speed_kph_mean)) %>% 
  ungroup() %>% 
  group_by(osm_way_id, osm_start_node_id, osm_end_node_id) %>% 
  summarise(max_speed = mean(max_speed) %>% round(2)) %>% 
  ungroup()


# Speed Summary -----------------------------------------------------------

speeds_summary <- speeds_import %>% 
  group_by(segment_id, osm_way_id, osm_start_node_id, osm_end_node_id) %>% 
  summarise(speed = mean(speed_kph_mean)) %>% 
  ungroup()

speeds_summary_geo <- speeds_summary %>% 
  inner_join(kyiv, by = c("osm_way_id", "osm_start_node_id", "osm_end_node_id"))

breaks <- c(20,30,50,75,100,125)

ggplot()+
  geom_sf(data=speeds_summary_geo, 
          aes(geometry = geometry,
              col=speed), 
          size=0.5) +
  scale_colour_steps2(
    low = "#d73027",
    mid = "grey95",
    high = "#4575b4",
    breaks = breaks,
    midpoint = mean_speed,
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  theme_void()



# Daily summary --------------------------------------------------------

speeds_daily <- speeds %>% 
  mutate(date = as.Date(utc_timestamp)) %>% 
  group_by(date) %>% 
  summarise(distance = sum(length),
            time = sum(time)) %>% 
  ungroup() %>% 
  mutate(speed = distance / time,
         day = day(date),
         wday = wday(date),
         yday = yday(date),
         month = month(date),
         year = year(date),
         ymonth = floor_date(date, unit = "months"))

ggplot(speeds_daily, aes(x=date, y=speed)) + 
  geom_line()

ggplot(speeds_daily, aes(x=ymonth, y=wday, fill = speed)) + 
  geom_tile()+
  theme_minimal()

# Hourly summary --------------------------------------------------------

speeds_byhour <- speeds %>% 
  mutate(timestamp = with_tz(utc_timestamp, tzone = "Europe/Kiev"),
         hour = hour(timestamp)) %>% 
  group_by(hour) %>% 
  summarise(distance = sum(length),
            time = sum(time),
            speed_mean = mean(speed_kph_mean)) %>% 
  ungroup() %>% 
  mutate(speed = distance / time)

ggplot(speeds_byhour, aes(x=hour, y=speed_mean)) + 
  geom_point()+
  geom_smooth(se=FALSE, span = 0.3)+
  theme_minimal()



# Daily/Hourly summary --------------------------------------------------------

speeds_hourly <- speeds %>% 
  group_by(utc_timestamp) %>% 
  summarise(distance = sum(length),
            time = sum(time)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(utc_timestamp),
         speed = distance / time,
         day = day(date),
         wday = wday(date),
         yday = yday(date),
         month = month(date),
         year = year(date),
         ymonth = floor_date(date, unit = "months"),
         hour = hour(utc_timestamp))

ggplot(speeds_hourly, aes(x=date, y=hour, fill = speed), col = "white") + 
  geom_tile()+
  theme_minimal()

