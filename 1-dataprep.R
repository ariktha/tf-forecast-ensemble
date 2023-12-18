
## Creating a few tibbles for data:
### "prev" is all the cleaned and filtered data
### "geo_dict" is a list of all districts with shapefiles and the country and region it falls in. Essentially a geographic dictionary.
### "forecast" is prepped data for the final forecasting

if(redo_geodict == TRUE){
  centroids <- read_csv(here("data", "raw", "geo_id_centroids.csv"), show_col_types = FALSE) %>% distinct()
  all_shapefiles <- st_read(here("data", "raw", "IUs_10May2021", "IUs_10May2021.shp"), quiet = TRUE)
  crosswalk <- read_csv(here("data", "raw", "geo_id_crosswalk.csv"), show_col_types = FALSE)
  
  crosswalk <- crosswalk %>% mutate(Geoconnect = ifelse(is.na(geo_id_shapefile), geo_id, geo_id_shapefile))
  
  all_shapefiles <- all_shapefiles %>% left_join(crosswalk, by = "Geoconnect") %>%
    rename(admin0_id = ADMIN0ID, admin1_id = ADMIN1ID, admin2_id = ADMIN2ID, admin3_id = ADMIN3ID, 
           admin0_name = ADMIN0, admin1_name = ADMIN1, admin2_name = ADMIN2, admin3_name = ADMIN3)
  
  geo_dict <- all_shapefiles %>% left_join(centroids) %>% drop_na(geo_id, admin0_id, admin1_id) %>%
    dplyr::select(geo_id, admin0_id, admin0_name, admin1_id, admin1_name, admin2_id, admin2_name, 
                  admin3_id, admin3_name, Geoconnect, latitude, longitude) %>% distinct()
  
  saveRDS(geo_dict, here("data", "geo_dict.RDS"))
  rm(all_shapefiles, crosswalk, centroids)
  
} else if(file.exists(here("data", "geo_dict.RDS"))){
  geo_dict <- readRDS(here("data", "geo_dict.RDS"))
} else print("Error in geo_dict")

if(redo_prev == TRUE){
  
  prev_raw <- read_csv(here("data", "raw", "ITI_02062023", "prevalence.csv"), show_col_types = FALSE) %>% 
    distinct() %>% rename(year = survey_year) %>% mutate(year = as.numeric(year)) %>% 
    # dplyr::filter(name %in% perm_countries) %>%
    dplyr::select(all_of(prev_cols))
  dist_raw <- read_csv(here("data", "raw", "ITI_02062023", "distribution.csv"), show_col_types = FALSE) %>% distinct()
  
  prev <- full_join(prev_raw, geo_dict, by = "geo_id") %>% 
    mutate(tf_prev = as.numeric(tf_prev), tt_prev = as.numeric(tt_prev)) %>%
    filter(tf_prev >= thresh_tf)
  
  years <- min(train_years-thresh_years):max(evaluate_years)
  
  prev <- add_mda(prev, dist_raw)
  tfrc_tbl <- rc_table(prev)
  lag_tbl <- lag_table(prev)
  prev <- prev %>% left_join(tfrc_tbl) %>% left_join(lag_tbl) %>% distinct()
  
  tfrc_tbl <- tfrc_tbl %>% filter(year == max(score_years))
  lag_tbl <- lag_tbl %>% filter(year == max(score_years))
  
  forecast <- geo_dict %>% mutate(year = pred_year)
  forecast <- add_mda(forecast, dist_raw) %>% left_join(tfrc_tbl, by = join_by(admin0_id, admin1_id)) %>% 
    left_join(lag_tbl, by = join_by(geo_id)) %>% dplyr::select(-starts_with("year")) %>% ungroup()
  forecast <- forecast %>% mutate(year = pred_year) %>% distinct() %>% 
    drop_na(latitude, longitude, year) %>% filter(year >= 2004)
  
  prev <- prev %>% drop_na(latitude, longitude, year) %>% filter(year >= 2004)
  
  saveRDS(prev, here("data", "prev.RDS"))
  saveRDS(forecast, here("data", "forecast_raw.RDS"))
  rm(lag_tbl, tfrc_tbl, prev_raw, dist_raw)
  
} else if(file.exists(here("data", "prev.RDS")) & file.exists(here("data", "forecast_raw.RDS"))){
  
  prev <- readRDS(here("data", "prev.RDS"))
  forecast <- readRDS(here("data", "forecast_raw.RDS"))
  
} else print("Error in prev")
