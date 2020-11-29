files <- list.files("data/raw/monthly")
for (i in files) {
  temp <- read_csv(paste0("data/raw/monthly/",i))
  saveRDS(temp, paste0("data/raw/monthly/rds/",i,".RDS"))
}

kyiv_speeds <- NULL
kyiv_speeds <- readRDS("data/kyiv_speeds.RDS")

files <- list.files("data/raw/monthly/rds", full.names = TRUE)
speeds <- NULL
for (k in files) {
  temp <- readRDS(k)
  speeds <- speeds %>%
    bind_rows(temp)
  saveRDS(speeds, paste0("data/kyiv_speeds_2020.RDS"))
  print(k)
}

kyiv_speeds %>%
  select(utc_timestamp) %>%
  distinct() %>%
  mutate(month = floor_date(utc_timestamp, unit = "months")) %>%
  select(month) %>%
  distinct() %>%
  View()

kyiv_speeds %>%
  arrange(desc(utc_timestamp)) %>%
  head(100) %>% View()
speeds_import <- purrr::map_dfr(files, readRDS)