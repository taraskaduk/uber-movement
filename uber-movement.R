library(sf)
library(osmdata)
library(lubridate)
library(gganimate)
library(tidyverse)
library(extrafont)

loadfonts()

theme_tk <- theme_void() +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, 
                                  family="Raleway",
                                  face = "bold"))

theme_set(theme_tk)

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
# speeds <- NULL
# for (k in files) {
#   temp <- readRDS(k)
#   speeds <- speeds %>%
#     bind_rows(temp)
#   saveRDS(speeds, paste0("data/kyiv_speeds_2020.RDS"))
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

place <- "Kyiv Ukraine"

boundary <- opq(place) %>%
  add_osm_feature(key = "boundary", 
                  value = c("administrative")) %>%
  osmdata_sf() %>% 
  osmdata::unname_osmdata_sf() %>% 
  .$osm_multipolygons %>% 
  filter(osm_id == 421866) %>% 
  dplyr::select()


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
  filter(wday(timestamp) %in% c(1,7)) %>% 
  mutate(date = as.Date(timestamp)) %>% 
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

# rm(speeds_import, speeds, speeds_max, streets_lengths, streets_import, streets_filtered)



# Speed Summary -----------------------------------------------------------

speeds_summary <- speeds_delay %>% 
  group_by(index) %>% 
  summarise(speed = mean(speed_kph_mean)) %>% 
  ungroup()

speeds_summary_geo <- speeds_summary %>% 
  inner_join(streets, by = "index")

breaks <- c(20,30,50,75,100,125)

ggplot()+
  geom_sf(data=boundary, fill="grey99")+
  #geom_sf(data=streets, fill="grey70", size=0.05, alpha=0.5)+
  geom_sf(data=speeds_summary_geo, 
          aes(geometry = geometry,
              col=speed), 
          size=0.6) +
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

speeds_daily <- speeds_delay %>% 
  mutate(date = as.Date(timestamp)) %>% 
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
  theme_minimal() +
  scale_fill_steps2(
    low = "#d73027",
    mid = "grey95",
    high = "#4575b4",
    #breaks = breaks,
    midpoint = mean(speeds_daily$speed),
    na.value = "grey50",
    guide = "coloursteps")

# Hourly summary --------------------------------------------------------

speeds_byhour <- speeds_delay %>% 
  mutate(hour = hour(timestamp)) %>% 
  group_by(hour) %>% 
  summarise(distance = sum(length),
            time = sum(time),
            best_time = sum(best_time),
            delay = sum(delay),
            speed_mean = mean(speed_kph_mean)) %>% 
  ungroup() %>% 
  mutate(speed = distance / time,
         delay_pct = delay / best_time)

ggplot(speeds_byhour, aes(x=hour, y=speed_mean)) + 
  geom_point()+
  geom_smooth(se=FALSE, span = 0.3)+
  theme_minimal()

ggplot(speeds_byhour, aes(x=hour, y=delay_pct)) + 
  geom_point()+
  geom_smooth(span = 0.4)+
  theme_minimal()




# Daily/Hourly summary --------------------------------------------------------

speeds_hourly <- speeds_delay %>% 
  group_by(timestamp) %>% 
  summarise(distance = sum(length),
            time = sum(time)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(timestamp),
         speed = distance / time,
         day = day(date),
         wday = wday(date),
         yday = yday(date),
         month = month(date),
         year = year(date),
         ymonth = floor_date(date, unit = "months"),
         hour = hour(timestamp))

ggplot(speeds_hourly, aes(x=date, y=hour, fill = speed), col = "white") + 
  geom_tile()+
  theme_minimal()




# Hourly by street --------------------------------------------------------

speeds_bystreet_byhour <- speeds_delay %>% 
  mutate(hour = hour(timestamp)) %>% 
  group_by(index, hour) %>% 
  summarise(distance = sum(length),
            time = sum(time),
            best_time = sum(best_time),
            delay = sum(delay),
            speed_mean = mean(speed_kph_mean)) %>% 
  ungroup() %>% 
  mutate(speed = distance / time,
         delay_pct = delay / best_time)

speeds_by_street_byhour_geo <- speeds_bystreet_byhour %>% 
  inner_join(streets, by = "index")

hours <- c(0:23)
for (h in hours){
  ggplot()+
    geom_sf(data=boundary, fill="grey99")+
    geom_sf(data=speeds_by_street_byhour_geo %>% filter(hour == h), 
            aes(geometry = geometry,
                col=delay_pct), 
            size=0.3) +
    scale_colour_viridis_b(
      breaks = c(0,0.25,0.5,1,2,5,10,25),
      na.value = "grey50",
      guide = "coloursteps",
      option = "magma",
      limits = c(0, 2), 
      oob = scales::squish,
      direction = -1) +
    labs(title = paste0("Kyiv city traffic delay, ", str_pad(h,2,"left",0),":00"),
         caption = "taraskaduk.com | @taraskaduk")
  
  ggsave(paste0("output/", str_pad(h,2,"left",0),".png"))
}

ggplot()+
  geom_sf(data=boundary, fill="grey99")+
  geom_sf(data=speeds_by_street_byhour_geo, 
          aes(geometry = geometry,
              col=delay_pct), 
          size=0.3) +
  theme_void() +
  scale_colour_viridis_b(
    breaks = c(0,0.25,0.5,1,2,5,10,25),
    na.value = "grey50",
    guide = "coloursteps",
    option = "magma",
    limits = c(0, 2), 
    oob = scales::squish,
    direction = -1) +
  facet_wrap(~hour, ncol = 4)

ggsave("facet.png")

p <- ggplot()+
  #geom_sf(data=boundary, fill="grey99")+
  geom_sf(data=speeds_by_street_byhour_geo, 
          aes(geometry = geometry,
              col=delay_pct), 
          size=0.3) +
  theme_void() +
  scale_colour_viridis_b(
    breaks = c(0,0.25,0.5,1,2,5,10,25),
    na.value = "grey50",
    guide = "coloursteps",
    option = "magma",
    limits = c(0, 2), 
    oob = scales::squish,
    direction = -1) +
  labs(title = 'Hour: {frame_time}') + 
  transition_time(hour)

animate(p, 
        duration = 48, 
        fps = 0.5, 
        # width = 200, 
        # height = 200, 
        renderer = gifski_renderer())
anim_save("test.gif")

ggplot()+
  geom_sf(data=boundary, fill="grey99")+
  geom_sf(data=speeds_by_street_byhour_geo, 
          aes(geometry = geometry,
              col=delay_pct,
              group=osm_way_id, osm_start_node_id, osm_end_node_id), 
          size=0.3) +
  theme_void() +
  scale_colour_viridis_c(
    na.value = "grey50",
    option = "magma",
    trans = "sqrt",
    direction = -1) 
