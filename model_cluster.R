clean_data_train <- clean_rawdata_reg (train, yrs = train_years, zero_thresh = ZERO_THRESH, key_cols = KEY_COL_NAMES)
clean_data_train <- coalesce_data(clean_data_train)
saveRDS(clean_data_train, here("data", "seth_train_data_clean.RDS"))

if(fit_seths == TRUE){
  global_fit <- mle_fit(data = clean_data_train, fits = 'renorm_exp')
  country_fits <- clean_data_train %>% group_by(country_clust) %>% 
    do({country_fit <- mle_fit(data = ., fits = 'renorm_exp')})
  cluster_fits <- clean_data_train %>% group_by(final_clust) %>% 
    do({cluster_fit <- mle_fit(data = ., fits = 'renorm_exp')})
  
  saveRDS(global_fit, here("data", "global_fit.RDS"))
  saveRDS(country_fits, here("data", "country_fit.RDS"))
  saveRDS(cluster_fits, here("data", "cluster_fit.RDS"))
} else {
  global_fit <- readRDS(here("data", "global_fit.RDS"))
  country_fits <- readRDS(here("data", "country_fit.RDS"))
  cluster_fits <- readRDS(here("data", "cluster_fit.RDS"))
}

clean_data_train <- clean_data_train %>% ungroup() %>% 
  dplyr::select(admin0_id, admin1_id, country_clust, final_clust) %>% distinct()

global_rates <- det_coef(global_fit, yrs = score_years)
global_rates <- transform_coef(global_rates,'global')

country_rates <- country_fits %>% group_by(country_clust) %>% do ({
  rate_coef <- det_coef(.,yrs = score_years)
  transform_coef(rate_coef, 'country')
}) %>% ungroup()

final_rates <- cluster_fits %>% group_by(final_clust) %>% do ({
  rate_coef <- det_coef(.,yrs = score_years)
  transform_coef(rate_coef, 'dishwash')
}) %>% ungroup()

score <- score %>% left_join(clean_data_train) %>%
  replace_na(list(country_clust = "sub_global", final_clust = "sub_global")) %>%
  left_join(global_rates) %>% left_join(country_rates) %>% left_join(final_rates) 

rm(clean_data_train, global_fit, country_fits, cluster_fits, global_rates, country_rates, final_rates)

## Refit

clean_data_retrain <- clean_rawdata_reg(re_train, yrs = retrain_years, zero_thresh = ZERO_THRESH, key_cols = KEY_COL_NAMES)
clean_data_retrain <- coalesce_data(clean_data_retrain)
saveRDS(clean_data_retrain, here("data", "seth_retrain_data_clean.RDS"))

if(refit_seths == TRUE){
  global_refit <- mle_fit(data = clean_data_retrain, fits = 'renorm_exp')
  country_refits <- clean_data_retrain %>% group_by(country_clust) %>% 
    do({country_refit <- mle_fit(data = ., fits = 'renorm_exp')})
  cluster_refits <- clean_data_retrain %>% group_by(final_clust) %>% 
    do({cluster_refit <- mle_fit(data = ., fits = 'renorm_exp')})
  
  saveRDS(global_refit, here("data", "global_refit.RDS"))
  saveRDS(country_refits, here("data", "country_refit.RDS"))
  saveRDS(cluster_refits, here("data", "cluster_refit.RDS"))
} else {
  global_refit <- readRDS(here("data", "global_refit.RDS"))
  country_refits <- readRDS(here("data", "country_refit.RDS"))
  cluster_refits <- readRDS(here("data", "cluster_refit.RDS"))
}

global_rates_refit <- det_coef(global_refit, yrs = pred_year)
wideform_global_refit <- transform_coef(global_rates_refit,'global')

country_rates_refit <- country_refits %>% group_by(country_clust) %>% do ({
  rate_coef <- det_coef(., yrs = pred_year)
  transform_coef(rate_coef, 'country')
}) %>% ungroup() %>% mutate(admin0_id = as.numeric(sub(".*_", "", country_clust)))

final_rates_refit <- cluster_refits %>% group_by(final_clust) %>% do ({
  rate_coef <- det_coef(., yrs = pred_year)
  transform_coef(rate_coef, 'dishwash')
}) %>% ungroup() %>% mutate(admin1_id = as.numeric(sub(".*_", "", final_clust)))

subglobal_country <- country_rates_refit$country_rate[which(country_rates_refit$country_clust == "sub_global")]
subglobal_dishwash <- final_rates_refit$dishwash_rate[which(final_rates_refit$final_clust == "sub_global")]

country_rates_refit <- country_rates_refit %>% dplyr::select(-country_clust)
final_rates_refit <- final_rates_refit %>% dplyr::select(-final_clust)

forecast <- forecast %>% left_join(wideform_global_refit) %>% 
  left_join(country_rates_refit) %>% left_join(final_rates_refit) %>% 
  replace_na(list(country_rate = subglobal_country, dishwash_rate = subglobal_dishwash))

## Eval dataset

global_rates_refit <- det_coef(global_refit, yrs = evaluate_years)
wideform_global_refit <- transform_coef(global_rates_refit,'global')

country_rates_refit <- country_refits %>% group_by(country_clust) %>% do ({
  rate_coef <- det_coef(., yrs = evaluate_years)
  transform_coef(rate_coef, 'country')
}) %>% ungroup() %>% mutate(admin0_id = as.numeric(sub(".*_", "", country_clust)))

final_rates_refit <- cluster_refits %>% group_by(final_clust) %>% do ({
  rate_coef <- det_coef(., yrs = evaluate_years)
  transform_coef(rate_coef, 'dishwash')
}) %>% ungroup() %>% mutate(admin1_id = as.numeric(sub(".*_", "", final_clust)))

subglobal_country <- country_rates_refit$country_rate[which(country_rates_refit$country_clust == "sub_global")]
subglobal_dishwash <- final_rates_refit$dishwash_rate[which(final_rates_refit$final_clust == "sub_global")]

country_rates_refit <- country_rates_refit %>% dplyr::select(-country_clust)
final_rates_refit <- final_rates_refit %>% dplyr::select(-final_clust)

eval_data <- eval_data %>% left_join(wideform_global_refit) %>% 
  left_join(country_rates_refit) %>% left_join(final_rates_refit) %>% 
  replace_na(list(country_rate = subglobal_country, dishwash_rate = subglobal_dishwash))

rm(clean_data_retrain, global_refit, country_refits, cluster_refits,
   wideform_global_refit, global_rates_refit, country_rates_refit, final_rates_refit)