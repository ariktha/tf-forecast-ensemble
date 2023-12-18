library(tidyverse)
library(ReIns)

pdf_val <- function(tf_val, rate) dtexp(x = tf_val, rate = rate, endpoint = 100)
pdf_lnorm <- function(tf_val, pars) dlnorm(x = tf_val, meanlog = pars[[1]][1], sdlog = pars[[1]][2])
rm_inf <- function(x) ifelse(is.finite(x), x, NA)
isect <- function(a, b, c) intersect(intersect(a, b), c)
log_pdf <- function(x) round(log(x), 2)*(-1)
round_2 <- function(x) round(x, 2)
isnta <- function(x) is.na(x) || is.nan(x)
se2sd <- function(se, samp_size) se*sqrt(samp_size)

log_score <- function(tf, cdf){
  ind <- as.integer((tf*100)+1)
  cdf_val <- as.numeric(cdf[ind])
  round(log(cdf_val), 2)*(-1)
}

crps_score <- function(tf, cdf){
  ind <- as.integer((tf*100)+1)
  cdf <- unlist(cdf)
  ar_1 <- 0
  for(i in 1:ind) ar_1 = ar_1 + cdf[i]*0.01
  ar_2_comp <- 0
  for(i in ind:length(q)) ar_2_comp = ar_2_comp + cdf[i]*0.01
  ar_2 <- 100 - tf - ar_2_comp
  ar_1 + ar_2
}

add_mda <- function(prev, dist_raw){
  mda <- dist_raw %>% rename(year = program_year) %>%
    mutate(mda_round = ifelse(total_zx_distributed > 0, 1, 0)) %>% 
    dplyr::select(geo_id, year, mda_round) %>%
    group_by(geo_id) %>% arrange(year)
    
  prev <- prev %>% rowwise() %>%
    mutate(num_MDA = sum(mda$geo_id == geo_id & between(mda$year, 1985, year)))
  
  prev
}

get_pdf <- function(pdf, tf_prev){
  ind = as.integer((tf_prev*100)+1)
  as.numeric(pdf[ind])
}

norm_pdf_val <- function(tf_val, mean_inv, norm_sd){
  dnorm(x = tf_val, mean = 1/mean_inv, sd = norm_sd) / pnorm(q = 100, mean = 1/mean_inv, sd = norm_sd)
}

simple_weights <- function(scores, models, k){
  en_weights <- scores %>% dplyr::filter(model %in% models) %>% mutate(L_k = score/k) %>% mutate(exp_L_k = exp(L_k*(-1)))
  sum_exp_L_k = sum(en_weights$exp_L_k)
  en_weights <- en_weights %>% mutate(weight = exp_L_k/sum_exp_L_k) %>% dplyr::select(model, weight)
  return(en_weights)
}

area_thresh <- function(pdfs, tf_thresh = 5){
  q <- c(seq(0, tf_thresh, by = 0.01))
  n_rows <- length(q)
  prob_sub <- pdfs %>% filter(row_number() <= n_rows)
  area = vector(length = ncol(prob_sub))
  for(i in c(1:ncol(prob_sub))){area[i] = round(DescTools::AUC(x = q, y = unlist(prob_sub[,i]), na.rm = TRUE), 2)}
  area
}

cdfify <- function(forecast_matrix){
  cdf_mat <- forecast_matrix*0.01
  cdf_mat <- cdf_mat %>% mutate_all(cumsum)
  cdf_mat
}

crps_alt <- function(tf, cdf, geo_id){
  q = seq(0, 100, 0.01)
  zero_tf = seq(0, tf, 0.01)
  tf_100 = seq(tf, 100, 0.01)
  cutpt = length(zero_tf)
  cdf_sq = cdf^2
  y_crps = 1 - (1-cdf)^2
  auc_1 = round(DescTools::AUC(x = zero_tf, y = cdf_sq[1:cutpt]), 2)
  auc_2 = round(DescTools::AUC(x = tf_100, y = y_crps[cutpt:length(q)]), 2)
  auc_1 + 100 - tf - auc_2
}

crps_score_unif <- function(tf){
  tf = ifelse(tf==0, 0.01, tf)
  q = seq(0, 100, 0.01)
  zero_tf = seq(0, tf, 0.01)
  tf_100 = seq(tf, 100, 0.01)
  cutpt = length(zero_tf)
  cdf = punif(q, min = 0, max = 100)
  cdf_sq = cdf^2
  y_crps = 1 - (1-cdf)^2
  auc_1 = round(DescTools::AUC(x = zero_tf, y = cdf_sq[1:cutpt]), 2)
  auc_2 = round(DescTools::AUC(x = tf_100, y = y_crps[cutpt:length(q)]), 2)
  auc_1 + 100 - tf - auc_2
}

pdf2cdf <- function(pdf){
  pdf <- unlist(pdf)
  cdf <- vector(mode = "numeric", length = length(pdf))
  for(i in 1:length(cdf)) cdf[i] = ifelse(i==1, pdf[i]*0.01, cdf[i-1] + pdf[i]*0.01)
  cdf
}

cdf2expval <- function(cdf){
  q <- seq(0, 100, by = 0.01)
  cdf <- unlist(cdf)
  exp_val <- sum(q*cdf)
  exp_val
}

rc_table <- function(prev){
  prev_0 <- prev %>% dplyr::select(admin0_id, year, tf_prev, tt_prev) %>% 
    group_by(admin0_id, year) %>% 
    summarise(country_tf = mean(tf_prev), country_tt = mean(tt_prev)) 
  
  admin0 <- tibble(
    admin0_id = rep(unique(geo_dict$admin0_id), each = length(years)), 
    year = rep(years, times = length(unique(geo_dict$admin0_id))))
  
  country <- left_join(admin0, prev_0, by = c("admin0_id", "year")) %>%
    group_by(admin0_id) %>% 
    mutate(lead_tf = lag(country_tf, n = 1), lead_tt = lag(country_tt, n = 1)) %>% 
    mutate(tf_country = rollapply(lead_tf, thresh_years, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    mutate(tt_country = rollapply(lead_tt, thresh_years, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    dplyr::select(-lead_tf, -lead_tt, -country_tf, -country_tt) %>%
    ungroup()
  
  global <- country %>% group_by(year) %>% summarise(tf_global = mean(tf_country, na.rm = TRUE),
                                                     tt_global = mean(tt_country, na.rm = TRUE))
  
  prev_1 <- prev %>% dplyr::select(admin1_id, year, tf_prev, tt_prev) %>% 
    group_by(admin1_id, year) %>% 
    summarise(region_tf = mean(tf_prev), region_tt = mean(tt_prev))
  
  admin1 <- tibble(admin1_id = rep(unique(geo_dict$admin1_id), each = length(years)), 
                   year = rep(years, times = length(unique(geo_dict$admin1_id))))
  
  region <- left_join(admin1, prev_1, by = c("admin1_id", "year")) %>% 
    group_by(admin1_id) %>% 
    mutate(lead_tf = lag(region_tf, n = 1), lead_tt = lag(region_tt, n = 1)) %>% 
    mutate(tf_region = rollapply(lead_tf, thresh_years, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    mutate(tt_region = rollapply(lead_tt, thresh_years, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    dplyr::select(-lead_tf, -lead_tt, -region_tf, -region_tt) %>%
    ungroup()
  
  tfrc_tbl <- tibble(admin0_id = geo_dict$admin0_id, admin1_id = geo_dict$admin1_id) %>% distinct() %>%
    left_join(country, by = "admin0_id") %>% 
    left_join(region, by = c("admin1_id", "year")) %>% 
    left_join(global, by = "year")
  
  tfrc_tbl <- tfrc_tbl %>% 
    mutate_at(c("tf_country", "tt_country", "tf_region", "tt_region", "tf_global", "tt_global"), round_2) %>% 
    rowwise() %>%
    mutate(tf_rc = ifelse(isnta(tf_region), tf_country, tf_region), 
           tt_rc = ifelse(isnta(tt_region), tt_country, tt_region),
           tf_rc = ifelse(isnta(tf_rc), tf_global, tf_rc), 
           tt_rc = ifelse(isnta(tt_rc), tt_global, tt_rc)) %>%
    filter(year >= earliest_year) %>% ungroup() %>%
    dplyr::select(admin0_id, admin1_id, year, tf_rc, tt_rc)
  
  tfrc_tbl
}

lag_table <- function(prev){
  prev_match <- prev %>% 
    group_by(geo_id) %>% arrange(year) %>%
    mutate(lag_tf = lag(tf_prev, n = 1), lag_tt = lag(tt_prev, n = 1),
           lag_year = lag(year, n = 1), year_diff = year - lag_year) %>%
    drop_na() %>%
    filter(year_diff != 0) %>% 
    dplyr::select(geo_id, year, lag_tf, lag_tt, year_diff) %>%
    ungroup()
  
  prev_0 <- prev %>% dplyr::select(admin0_id, year, tf_prev, tt_prev) %>% 
    group_by(admin0_id, year) %>% 
    summarise(country_tf = mean(tf_prev), country_tt = mean(tt_prev)) 
  
  admin0 <- tibble(
    admin0_id = rep(unique(geo_dict$admin0_id), each = length(years)), 
    year = rep(years, times = length(unique(geo_dict$admin0_id))))
  
  country_lag <- left_join(admin0, prev_0, by = c("admin0_id", "year")) %>%
    group_by(admin0_id) %>% 
    mutate(lead_tf = lag(country_tf, n = 1), lead_tt = lag(country_tt, n = 1)) %>% 
    mutate(tf_country = rollapply(lead_tf, 1, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    mutate(tt_country = rollapply(lead_tt, 1, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    dplyr::select(-lead_tf, -lead_tt, -country_tf, -country_tt) %>%
    ungroup()
  
  global_lag <- country_lag %>% group_by(year) %>% summarise(tf_global = mean(tf_country, na.rm = TRUE),
                                                             tt_global = mean(tt_country, na.rm = TRUE))
  
  prev_1 <- prev %>% dplyr::select(admin1_id, year, tf_prev, tt_prev) %>% 
    group_by(admin1_id, year) %>% 
    summarise(region_tf = mean(tf_prev), region_tt = mean(tt_prev))
  
  admin1 <- tibble(admin1_id = rep(unique(geo_dict$admin1_id), each = length(years)), 
                   year = rep(years, times = length(unique(geo_dict$admin1_id))))
  
  region_lag <- left_join(admin1, prev_1, by = c("admin1_id", "year")) %>% 
    group_by(admin1_id) %>% 
    mutate(lead_tf = lag(region_tf, n = 1), lead_tt = lag(region_tt, n = 1)) %>% 
    mutate(tf_region = rollapply(lead_tf, 1, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    mutate(tt_region = rollapply(lead_tt, 1, mean, fill = NA, na.rm = TRUE, align = "right")) %>%
    dplyr::select(-lead_tf, -lead_tt, -region_tf, -region_tt) %>%
    ungroup()
  
  rm(prev_0, admin0, prev_1, admin1)
  
  lag_tbl <- tibble(admin0_id = geo_dict$admin0_id, admin1_id = geo_dict$admin1_id, geo_id = geo_dict$geo_id) %>% distinct() %>%
    left_join(country_lag, by = "admin0_id") %>% 
    left_join(region_lag, by = c("admin1_id", "year")) %>% 
    left_join(global_lag, by = "year")
  
  lag_tbl <- lag_tbl %>% mutate_at(c("tf_country", "tt_country", "tf_region", "tt_region", "tf_global", "tt_global"), round_2) %>% 
    rowwise() %>%
    mutate(lag_tf_alt = ifelse(isnta(tf_region), tf_country, tf_region), 
           lag_tt_alt = ifelse(isnta(tt_region), tt_country, tt_region),
           lag_tf_alt = ifelse(isnta(lag_tf_alt), tf_global, lag_tf_alt), 
           lag_tt_alt = ifelse(isnta(lag_tt_alt), tt_global, lag_tt_alt)) %>%
    filter(year >= earliest_year) %>% ungroup() %>%
    dplyr::select(geo_id, year, lag_tf_alt, lag_tt_alt)
  
  lag_tbl <- lag_tbl %>% left_join(prev_match)
  
  lag_tbl <- lag_tbl %>% mutate(lag_tf = ifelse(is.na(lag_tf), lag_tf_alt, lag_tf),
                                lag_tt = ifelse(is.na(lag_tf), lag_tf_alt, lag_tf)) %>%
    dplyr::select(-c(lag_tf_alt, lag_tt_alt))
  
  lag_tbl
}