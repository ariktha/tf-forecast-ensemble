if(fit_geospatial == TRUE){
  
  geo_linmod_train <- fitme(log(tf_prev + const_log) ~ year + Matern(1 | longitude + latitude), data = train)
  saveRDS(geo_linmod_train, here("data", "geo_linmod_train.RDS"))
  
} else if(file.exists(here("data", "geo_linmod_train.RDS"))){
  
  geo_linmod_train <- readRDS(here("data", "geo_linmod_train.RDS"))
  
} else print("Error in geospatial training")

score_newdata <- tibble(geo_id = score$geo_id, latitude = score$latitude, longitude = score$longitude, year = score$year)
score_newdata$geo_pred <- exp(as.numeric(predict(geo_linmod_train, newdata = score_newdata)))
score_newdata <- score_newdata %>% mutate(geo_rate = 1/geo_pred)

score <- score %>% left_join(score_newdata)

if(refit_geospatial == TRUE){
  
  geo_linmod_retrain <- fitme(log(tf_prev + const_log) ~ year + Matern(1 | longitude + latitude), data = re_train)
  saveRDS(geo_linmod_retrain, here("data", "geo_linmod_retrain.RDS"))
  
} else if(file.exists(here("data", "geo_linmod_retrain.RDS"))){
  
  geo_linmod_retrain <- readRDS(here("data", "geo_linmod_retrain.RDS"))
  
} else print("Error in geospatial re-training")

forecast_newdata <- tibble(geo_id = forecast$geo_id, year = forecast$year,
                           latitude = forecast$latitude, longitude = forecast$longitude)
forecast_newdata$geo_pred <- exp(as.numeric(predict(geo_linmod_retrain, newdata = forecast_newdata)))
forecast_newdata <- forecast_newdata %>% mutate(geo_rate = 1/geo_pred)

forecast <- forecast %>% left_join(forecast_newdata) %>% distinct()

eval_newdata <- tibble(geo_id = eval_data$geo_id, year = eval_data$year,
                       latitude = eval_data$latitude, longitude = eval_data$longitude)
eval_newdata$geo_pred <- exp(as.numeric(predict(geo_linmod_retrain, newdata = eval_newdata)))
eval_newdata <- eval_newdata %>% mutate(geo_rate = 1/geo_pred)

eval_data <- eval_data %>% left_join(eval_newdata) %>% distinct()

rm(geo_linmod_train, geo_linmod_retrain, score_newdata, forecast_newdata, eval_newdata)
