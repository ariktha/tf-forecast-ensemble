## Evaluation dataset prep

eval_long <- eval_long %>% mutate_at("model", as.character) %>% rowwise() %>%
  mutate(pdf = ifelse(model %in% unif_models, list(rep(unif_rate, times = length(q))), list(pdf_val(q, rate)))) %>% 
  mutate(cdf = list(pdf2cdf(pdf))) 

eval_long <- eval_long %>% mutate(area = last(cdf)) %>% 
  mutate(pdf = ifelse(area > 1, list(pdf/area), list(pdf))) %>% 
  mutate(cdf = ifelse(area > 1, list(pdf2cdf(pdf)), list(cdf))) %>%
  dplyr::select(- area) %>% mutate(area = round(last(cdf), 2)) 

eval_long <- eval_long %>%
  cross_join(tibble(k = k_vec)) %>%
  left_join(weights, by = c("k", "model"))

eval_long$model <- factor(eval_long$model)
weights$model <- factor(weights$model)

geo_ids <- as.character(unique(eval_long$geo_id))

eval_ens <- eval_long %>% #drop_na() %>%
  rowwise() %>% mutate(pdf_wt = list(pdf*weight)) %>% ungroup() %>% 
  group_by(geo_id, year, k, tf_prev) %>% 
  summarise(pdf = list(unlist(pmap(pdf_wt, sum))), .groups = "keep") %>% 
  ungroup() %>% rowwise() %>% mutate(cdf = list(pdf2cdf(pdf))) %>% mutate(area = round(last(cdf), 2)) %>% 
  mutate(pdf = ifelse(area != 1, list(pdf/area), list(pdf))) %>% 
  mutate(cdf = ifelse(area != 1, list(pdf2cdf(pdf)), list(cdf))) %>%
  dplyr::select(- area) %>% mutate(area = round(last(cdf), 2)) %>%
  mutate(logs = log_score(tf_prev, cdf), crps = crps_score(tf_prev, cdf))

## Evaluation of ensembles and candidate models

ens_scores <- eval_ens %>% dplyr::select(k, crps, logs) %>% group_by(k) %>%
  summarise(crps = sum(crps, na.rm = TRUE), logs = sum(logs, na.rm = TRUE)) %>% ungroup() %>%
  mutate(model = paste0("Ensemble (k = ", k, ")"))

best_ens <- ens_scores %>% arrange(logs) %>% filter(row_number() == 1)

ens_scores <- ens_scores %>% dplyr::select(-k)

eval_long <- eval_long %>% drop_na() %>% dplyr::select(-c(k, score, weight)) %>% 
  #rbind(eval_lnorm) %>% 
  rowwise() %>% 
  mutate(logs = log_score(tf_prev, cdf), crps = crps_score(tf_prev, cdf)) %>% 
  ungroup() %>% mutate_at(c("crps", "logs"), rm_inf) %>% 
  dplyr::select(c(geo_id, year, model, crps, logs)) %>% distinct() %>%
  group_by(model) %>%
  summarise(crps = sum(crps, na.rm = TRUE), logs = sum(logs, na.rm = TRUE))

eval_long <- rbind(eval_long, ens_scores)

saveRDS(eval_long, here("data", "eval_scores.RDS"))

## Extract the ensemble with the lowest score (best performing) and use it to populate "forecast"

eval_ens <- eval_ens %>% filter(k == best_ens$k)
eval_obs <- tibble(tf = eval_ens$tf_prev)
sum_ens <- eval_ens %>% dplyr::select(pdf) %>% ungroup() %>%
  mutate(q = list(q)) %>% unnest(c(q, pdf)) %>%
  group_by(q) %>% summarise(pred = sum(pdf))

sum_ens <- sum_ens %>% mutate(pred = pred/nrow(eval_ens))
pdfs <- eval_ens %>% dplyr::select(geo_id, pdf) %>% mutate(q = list(q)) %>% unnest(c(q, pdf))

forecast <- forecast %>% mutate_if(is.numeric, function(x) round(ifelse(x == 0, 0.01, x), 2))

# forecast_lnorm <- forecast %>% dplyr::select(geo_id, year, lnorm_mean, lnorm_sd) %>%
#   rowwise() %>% mutate(pdf = list(dlnorm(q, lnorm_mean, lnorm_sd))) %>%
#   mutate(cdf = list(pdf2cdf(pdf)), area = last(cdf))
# 
# forecast_lnorm <- forecast_lnorm %>% mutate(pdf = list(pdf/area)) %>% 
#   mutate(cdf = list(pdf2cdf(pdf))) %>%
#   dplyr::select(- area) %>% mutate(area = last(cdf))
# 
# forecast_lnorm <-  forecast_lnorm %>% mutate(rate = list(list(lnorm_mean, lnorm_sd)), model = "Log-normal") %>% 
#   dplyr::select(-c(lnorm_mean, lnorm_sd))

forecast_long <- forecast %>% cross_join(unif_tib) %>% dplyr::select(geo_id, ends_with("_rate")) %>% 
  pivot_longer(ends_with("_rate"), names_to = "model", names_pattern = "(.*)_rate", values_to = "rate")

forecast_long$model <- factor(forecast_long$model, labels = all_models_eval, 
                              levels = c("fe", "re", "geo", "global", "dummy", "dishwash", "country", "unif"))

final_ens <- weights %>% filter(k == best_ens$k) %>%
  dplyr::select(model, weight) %>% mutate_at("model", as.character) %>% 
  right_join(forecast_long, by = "model", multiple = "all") %>% drop_na()

final_ens <- final_ens %>% rowwise() %>% mutate(pdf = list(pdf_val(q, rate)))
final_ens <- final_ens %>% mutate(cdf = list(pdf2cdf(pdf))) %>% mutate(area = last(cdf))

final_ens <- final_ens %>% mutate(pdf = list(pdf/area)) 
final_ens <- final_ens %>% mutate(cdf = list(pdf2cdf(pdf))) %>% mutate(area = last(cdf)) %>% ungroup()

# final_ens <- final_ens %>% rbind(forecast_lnorm)

## Massive table with the pdf for each district from each candidte model

saveRDS(final_ens, here("data", "final_ens_pre.RDS")) 
# saveRDS(forecast_lnorm, here("data", "forecast_lnorm.RDS"))

eg <- final_ens %>% filter(geo_id == eg_geo)
saveRDS(eg, here("data", "eg.RDS"))

finally_ens <- final_ens %>% rowwise() %>% mutate(pdf_wt = list(pdf*weight)) %>% 
  ungroup() %>% group_by(geo_id) %>% 
  summarise(pdf = list(unlist(pmap(pdf_wt, sum))), .groups = "keep") %>% ungroup() 

finally_ens <- finally_ens %>% rowwise() %>% mutate(cdf = list(pdf2cdf(pdf))) %>%
  mutate(area = last(cdf)) %>% rowwise() %>% mutate(expval = sum(pdf*q)/100) %>%
  mutate_at("expval", function(x) round(x, 2)) %>% mutate(p_control = cdf[[501]]) %>% ungroup() 

saveRDS(finally_ens, here("data", "final_ens.RDS"))

## Summing up the PDF for each each district to create an overall PDF for all districts forecasted

len <- nrow(finally_ens)

sum_ens <- finally_ens %>% drop_na() %>% ungroup() %>% dplyr::select(pdf) %>% 
  mutate(q = list(q)) %>% unnest(c(pdf, q)) %>% group_by(q) %>%
  reframe(pdf = sum(pdf)/len) %>% 
  mutate(cdf = cumsum(pdf)*0.01)

finally_ens <- finally_ens %>% drop_na() %>% ungroup() %>% dplyr::select(pdf) %>% 
  mutate(q = list(q)) 

saveRDS(sum_ens, here("data", "sum_ens.RDS"))

## Bootstrap for the number of districts expected to fail control according to the final ensemble model

nr <- nrow(finally_ens)
area_bs <- vector(mode = "numeric", length = 500)

pb = txtProgressBar(min = 0, max = length(area_bs), initial = 0) 
for(i in 1:length(area_bs)){
  sample_ind <- sample(1:nr, nr, replace = TRUE)
  bs_list <- finally_ens[sample_ind, ]
  
  sum_ens_bs <- bs_list %>% drop_na() %>% ungroup() %>% dplyr::select(pdf) %>% 
    mutate(q = list(q)) %>% unnest(c(pdf, q)) %>% group_by(q) %>%
    reframe(pdf = sum(pdf)/len) %>% 
    mutate(cdf = cumsum(pdf)*0.01)
  
  area_bs[i] <- (1-(sum_ens_bs$cdf[501]/sum_ens_bs$cdf[10001]))*100
  
  setTxtProgressBar(pb,i)
}

summary(area_bs)

dist_bs <- area_bs * nr / 100
fail_bs <- tibble(percent_fail = area_bs, n_fail = dist_bs)
saveRDS(fail_bs, here("data", "fail_bs.RDS"))

## Check that overall PDF's AUC = 1
round(last(unlist(sum_ens$cdf)), 2)

# % of all districts in endemic countries expected to fail EPHP 2030
(1-(unlist(sum_ens$cdf)[501]/unlist(sum_ens$cdf)[10001]))*100

# Number of districts expected to fail EPHP 2030
(1-(unlist(sum_ens$cdf)[501]/unlist(sum_ens$cdf)[10001]))*len

## Creating a table with probability of failing control for each district, according to each model

# prob_tab <- score_lnorm  %>% rowwise() %>% mutate(expval = sum(pdf*q)/100) %>%
#   mutate_at("expval", function(x) round(x, 2)) %>% mutate(p_control = cdf[[501]]) %>% ungroup() 
# 
# prob_tab <- prob_tab %>% dplyr::select(geo_id, p_control, model) %>%
#   mutate(p_control = round(p_control, 2))
# 
# prob_tab_wide <- prob_tab %>% distinct() %>% 
#   pivot_wider(names_from = model, values_from = p_control, values_fn = mean)
# 
# prob_fail_wide <- prob_tab_wide %>% mutate(across(!geo_id, function(s) 1-s))
# saveRDS(prob_fail_wide, here("data", "prob_fail_wide.RDS"))

