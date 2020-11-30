library(sf)
library(osmdata)
library(lubridate)
library(tidyverse)


load(url("https://github.com/taraskaduk/kyiv_osm/blob/main/kyiv_osm.RData?raw=true"))


# This the CLI command for pulling geometries
# mdt create-geometry-file kyiv 2020 --output="kyiv.geojson"
streets_import <- st_read("data/kyiv.geojson") %>% 
  st_set_crs(4326)

streets_filtered <- streets_import %>% 
  # filter(osmhighway %in% c("primary", "primary_link", 
  #                          "secondary", "secondary_link",
  #                          "tetriary", "tertiary_link")) %>% 
  st_intersection(boundary)

# ggplot()+
#   geom_sf(data=boundary, col = "red")+
#   geom_sf(data=streets_filtered, alpha = 0.2)

streets <- streets_filtered %>% 
  rename(osm_start_node_id = osmstartnodeid,
         osm_end_node_id = osmendnodeid,
         osm_way_id = osmwayid)

segments <- streets %>% 
  select(osm_way_id, osm_start_node_id, osm_end_node_id)

st_geometry(segments) <- NULL
segments <- segments %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(index = row_number())


streets <- streets %>% 
  inner_join(segments, by=c("osm_way_id", "osm_start_node_id", "osm_end_node_id"))

streets_lengths <- streets %>% 
  select(index) %>% 
  mutate(length = st_length(.) %>% as.numeric(),
         length = length/1000)
st_geometry(streets_lengths) <- NULL

saveRDS(streets, "output/kyiv_streets.RDS")

# ggplot()+
#   geom_sf(data=boundary, col = "red")+
#   geom_sf(data=streets, alpha = 0.2) +
#   facet_wrap(~osmhighway)


# Speed data --------------------------------------------------------------

speeds_import <- readRDS("data/kyiv_speeds_2020.RDS")

speeds <- speeds_import %>% 
  inner_join(segments, by = c("osm_way_id", "osm_start_node_id", "osm_end_node_id")) %>% 
  inner_join(streets_lengths, by = "index") %>% 
  mutate(time = length/speed_kph_mean,
         timestamp = with_tz(utc_timestamp, tzone = "Europe/Kiev")) %>% 
  select(-c(utc_timestamp,osm_way_id, osm_start_node_id, osm_end_node_id))
  
mean_speed <- mean(speeds$speed_kph_mean)

speeds_max <- speeds %>% 
  filter(wday(timestamp) %in% c(1,7) & 
           hour(timestamp) >= 6 &
           hour(timestamp) <= 22
  ) %>% 
  mutate(date = date(timestamp)) %>% 
  group_by(index, date) %>% 
  summarise(max_speed = max(speed_kph_mean)) %>% 
  ungroup() %>% 
  group_by(index) %>% 
  summarise(max_speed = mean(max_speed) %>% round(2)) %>% 
  ungroup()

speeds_delay <- speeds %>% 
  inner_join(speeds_max, by = "index") %>% 
  mutate(best_time = if_else(max_speed < speed_kph_mean, time, length/max_speed),
         delay = time - best_time)
speeds_delay_light <- speeds_delay %>% 
  select(index, timestamp, year:hour, speed_kph_mean, time, best_time, delay)
  
saveRDS(speeds_delay_light, "output/kyiv_speeds_delay_light.RDS")
