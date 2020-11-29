library(sf)
library(osmdata)
library(lubridate)
library(tidyverse)

kyiv <- list(
  search = "Kyiv Ukraine",
  name = "kyiv",
  admin_id = "421866",
  place_id = "26150422"
)

# seattle <- list(
#   search = "Seattle WA",
#   name = "seattle",
#   admin_id = "237385",
#   place_id = "29546940"
# )
# 
# jax <- list(
#   search = "Jacksonville FL",
#   name = "jax",
#   admin_id = "119008",
#   place_id = "154321322"
# )

search <- kyiv$search
admin_id <- kyiv$admin_id
place_id <- kyiv$place_id
name <- kyiv$name

  
boundary_osm <- opq(search) %>%
  add_osm_feature(key = "boundary", 
                  value = c("administrative")) %>%
  osmdata_sf() %>% 
  osmdata::unname_osmdata_sf()


boundary <- boundary_osm %>% 
  .$osm_multipolygons %>% 
  filter(osm_id == admin_id) %>% 
  dplyr::select(osm_id, name, 
                #name.en, 
                place, population, 
                # timezone, 
                type, wikidata, wikipedia)


coords_osm <- opq(search) %>%
  add_osm_feature(key = "place", 
                  value = c("city")) %>%
  osmdata_sf() %>% 
  osmdata::unname_osmdata_sf() 


coords <- coords_osm %>%
  .$osm_points %>% 
  dplyr::filter(osm_id == place_id) %>%
  dplyr::select(osm_id, name, 
                # name.en, 
                place, population, wikidata, wikipedia)

water_osm <- opq(search) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

river_osm <- opq(search) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank")) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

water <- c(water_osm, river_osm) %>% 
  .$osm_multipolygons %>% 
  select(osm_id, name) %>% 
  mutate(area = st_area(.)) %>% 
  filter(area >= quantile(area, probs = 0.75)) %>% 
  st_buffer(0) %>% 
  st_intersection(boundary)

railway <- opq(search) %>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf() %>% 
  .$osm_lines %>% 
  mutate(length = st_length(.)) %>% 
  st_intersection(boundary)

# landuse <- opq(search) %>%
#   add_osm_feature(key = "landuse", value="!zzz") %>%
#   osmdata_sf()

metro <- opq(search) %>%
  add_osm_feature(key = "railway", value = c("subway")) %>%
  osmdata_sf() 

metro_lines <- metro$osm_lines %>% 
  filter(is.na(service))

metro_stops <- metro$osm_points %>% 
  filter(!is.na(name))

highway_sizes <- tibble::tribble(
          ~highway, ~highway_group, ~size,
        "motorway",        "large",   0.5,
   "motorway_link",        "large",   0.3,
         "primary",        "large",   0.5,
    "primary_link",        "large",   0.3,
       "secondary",       "medium",   0.3,
  "secondary_link",       "medium",   0.3,
        "tertiary",       "medium",   0.3,
   "tertiary_link",       "medium",   0.3,
     "residential",        "small",   0.2,
   "living_street",        "small",   0.2,
    "unclassified",        "small",   0.2,
         "service",        "small",   0.2,
         "footway",        "small",   0.2
  )


streets_osm <- opq(search) %>%
  add_osm_feature(key = "highway", 
                  value = highway_sizes$highway) %>%
  osmdata_sf()

streets <- streets_osm$osm_lines %>% 
  select(osm_id, name, name.en, highway, maxspeed, oneway, surface) %>% 
  mutate(length = as.numeric(st_length(.))) %>% 
  st_intersection(boundary) %>% 
  left_join(highway_sizes, by="highway") %>% 
  filter(highway_group != "small" | length >= quantile(length, probs = 0.25))
  
rds_list <- list(
         coords = coords,
         boundary = boundary,
         water = water,
         streets = streets,
         railway = railway,
         metro_lines = metro_lines,
         metro_stops = metro_stops
       )

saveRDS(rds_list, paste0("data/",name, "_osm.RDS"))

save(
  coords,
  boundary,
  water,
  streets,
  railway,
  metro_lines,
  metro_stops,
  file = paste0("output/",name, "_osm.RData"))



ggplot()+
  geom_sf(data=coords) +
  geom_sf(data=boundary)+
  geom_sf(data=water)+
  geom_sf(data=streets)+
  geom_sf(data=railway)+
  geom_sf(data=metro_lines)+
  geom_sf(data=metro_stops)
