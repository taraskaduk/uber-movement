library(jsonlite)
library(sf)
library(tidyverse)

# files <- list.files("data/raw/monthly")
# for (i in files) {
#   temp <- read_csv(paste0("data/raw/monthly/",i)) 
#   saveRDS(temp, paste0("data/raw/monthly/rds/",i,".RDS"))
# }

# files <- list.files("data", full.names = TRUE)[1:2]
# speeds_import <- purrr::map_dfr(files, readRDS)
# 
# # mdt create-geometry-file kyiv 2020 --output="kyiv.geojson"
# kyiv <- st_read("kyiv.geojson")

# speeds <- st_read("kyiv_speeds.geojson")
# speeds_no_na <- speeds %>% filter(!is.na(speed_mean_kph))




# Kyiv geometry -----------------------------------------------------------

# kyiv_crs <- "ESRI:102013"

# This the CLI command for pulling geometries
# mdt create-geometry-file kyiv 2020 --output="kyiv.geojson"
kyiv_import <- st_read("data/kyiv.geojson")

ggplot()+
  geom_sf(data=kyiv, size=0.5, alpha = 0.1) +
  theme_void()


# Speed data --------------------------------------------------------------

speeds_import <- readRDS("data/kyiv_speeds.RDS")

ggplot()+
  geom_sf(data=speeds_grouped, aes(col=speed), size=1) +
  scale_color_continuous(type = "viridis") +
  theme_void()



