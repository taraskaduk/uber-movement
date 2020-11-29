library(sf)
library(osmdata)
library(lubridate)
library(tidyverse)


load("data/kyiv_osm.RData")


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

saveRDS(streets, "data/kyiv_streets.RDS")

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

speeds_check <- speeds_bystreet_byhour %>% 
  group_by(index) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n>=6) %>% 
  select(-n) %>% 
  merge(tibble(hour = c(0:23)), all=TRUE)

speeds_expanded <- speeds_check %>%  
  left_join(speeds_bystreet_byhour, by=c("index", "hour")) %>% 
  replace_na(list(delay_pct = 0))

saveRDS(speeds_expanded, "data/kyiv_speeds_street-hour_grain.RDS")




speeds_expanded_geo <- speeds_expanded %>% 
  inner_join(streets, by = "index") %>% 
  st_as_sf()



# Daily animation ---------------------------------------------------------


hours <- c(0:23)
options <- c("full", "circle1", "circle2")

hours <- 18
options <- "circle2"

for (option in options){
  if (option == "full"){
    plot_boundary <- boundary
    plot_water <- water
    plot_streets <- streets_filtered
    plot_speeds <- speeds_expanded_geo
  } else {
    
    center <- c(long = 30.5224974,
                lat = 50.4508911)
    
    if (option == "circle1") {
      dist <- 12500
    } else {
      dist <- 7500
    }
  
  
  plot_boundary <- tibble(lat = center["lat"], long = center["long"]) %>% 
    st_as_sf(coords = c("long", "lat"),crs = 4326) %>%  
    st_transform(3857) %>% 
    st_buffer(dist = dist) %>% 
    st_transform(4326)
  
  plot_water <- water %>% 
    st_intersection(plot_boundary)
  
  plot_streets <- streets_filtered %>% 
    st_intersection(plot_boundary) %>% 
    filter(!(osmhighway %in% c("service", "residential")))
  
  plot_speeds <- speeds_expanded_geo %>% 
    st_intersection(plot_boundary)
  
  }
  for (h in hours){
    ggplot()+
      geom_sf(data=plot_boundary, fill="#253441")+
      geom_sf(data = plot_water,
              fill = "#3C5368",
              # size = .8,
              lwd = 0) +
      geom_sf(data= plot_streets,
              col="#3B5268", 
              size=0.1) +
      geom_sf(data=plot_speeds %>% 
                filter(hour == h), 
              aes(geometry = geometry,
                  col=delay_pct), 
              size=0.4) +
      scale_colour_viridis_c(
        labels = scales::percent_format(accuracy = 1),
        breaks = c(0.25,0.5,1,2.5),
        #trans = "sqrt",
        na.value = "grey50",
        guide = "coloursteps",
        option = "magma",
        limits = c(0, 2.5), 
        oob = scales::squish,
        direction = -1) +
      labs(title = paste0("Kyiv city traffic delay, ", str_pad(h,2,"left",0),":00"),
           caption = "taraskaduk.com | @taraskaduk",
           color = "Traffic Delay, %") +
      theme(legend.text = element_text(size = 7))
    
    ggsave(paste0("output/",option,"-", str_pad(h,2,"left",0),".png"))
  }
}
# $ gifski --quality 100 --width 2595 --fps 2 -o full.gif full*.png



p <- ggplot()+
  geom_sf(data=plot_boundary, fill="#253441")+
  geom_sf(data = plot_water,
          fill = "#3C5368",
          # size = .8,
          lwd = 0) +
  geom_sf(data= plot_streets,
          col="#3B5268",
          size=0.1) +
  geom_sf(data=plot_speeds,
          aes(geometry = geometry,
              col=delay_pct, 
              group = index), 
          size=0.4) +
  scale_colour_viridis_c(
    labels = scales::percent_format(accuracy = 1),
    breaks = c(0.25,0.5,1,2.5),
    #trans = "sqrt",
    na.value = "grey50",
    guide = "coloursteps",
    option = "magma",
    limits = c(0, 2.5), 
    oob = scales::squish,
    direction = -1) +
  labs(title = paste0("Kyiv city traffic delay, ", str_pad("{current_frame}",2,"left",0),":00"),
       caption = "taraskaduk.com | @taraskaduk",
       color = "Traffic Delay, %") +
  theme(legend.text = element_text(size = 7)) +
  transition_manual(hour)

animate(p)

# animate(p, 
#         duration = 48, 
#         fps = 0.5, 
#         width = 200,
#         height = 200,
#         renderer = gifski_renderer()
#         )
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



