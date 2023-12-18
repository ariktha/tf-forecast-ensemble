## Initialize test and training datasets

train <- prev %>% filter(year %in% train_years)
re_train <- prev %>% filter(year %in% retrain_years)

eval_data <- prev %>% filter(year %in% evaluate_years)
score <- prev %>% filter(year %in% score_years)

## Geospatial model

source(here("scripts", 'model_geospatial.R'))

## Cluster-based models

source(here("scripts", 'model_cluster.R'))

## Regression models

source(here("scripts", 'model_regression.R'))

## Data prep for scoring

unif_tib <- tibble(unif_rate = round(1/100, 2))

# score_lnorm <- tibble(geo_id = score$geo_id, tf_prev = score$tf_prev, year = score$year,
#                       lnorm_mean = score$lnorm_mean, lnorm_sd = score$lnorm_sd)
# score_lnorm <- score_lnorm %>% rowwise() %>% mutate(pdf = list(dlnorm(q, lnorm_mean, lnorm_sd))) %>%
#   mutate(cdf = list(pdf2cdf(pdf)), area = last(cdf))
# 
# score_lnorm <- score_lnorm %>% mutate(pdf = list(pdf/area)) %>%
#   mutate(cdf = list(pdf2cdf(pdf))) %>%
#   dplyr::select(- area) %>% mutate(area = last(cdf))
# 
# score_lnorm <- score_lnorm %>% mutate(rate = list(list(lnorm_mean, lnorm_sd)), model = "lnorm") %>%
#   dplyr::select(-c(lnorm_mean, lnorm_sd))
# 
score_long <- score %>% cross_join(unif_tib) %>% dplyr::select(geo_id, year, tf_prev, ends_with("_rate")) %>% 
  pivot_longer(ends_with("_rate"), names_to = "model", names_pattern = "(.*)_rate", values_to = "rate") %>% 
  mutate_at(c("tf_prev"), round_2)

score_long$model <- factor(score_long$model, labels = all_models_score, 
                           levels = c("fe", "re", "geo", "global", "dummy", "dishwash", "country", "unif", "lnorm"))

# eval_lnorm <- eval_data %>% dplyr::select(geo_id, year, tf_prev, lnorm_mean, lnorm_sd) %>%
#   rowwise() %>% mutate(pdf = list(dlnorm(q, lnorm_mean, lnorm_sd))) %>%
#   mutate(cdf = list(pdf2cdf(pdf)), area = last(cdf))
# 
# eval_lnorm <- eval_lnorm %>% mutate(pdf = list(pdf/area)) %>% 
#   mutate(cdf = list(pdf2cdf(pdf))) %>%
#   dplyr::select(- area) %>% mutate(area = last(cdf))

# eval_lnorm <-  eval_lnorm %>% mutate(rate = list(list(lnorm_mean, lnorm_sd)), model = "lnorm") %>% 
#   dplyr::select(-c(lnorm_mean, lnorm_sd))

eval_long <- eval_data %>% cross_join(unif_tib) %>% dplyr::select(geo_id, year, tf_prev, ends_with("_rate")) %>% 
  pivot_longer(ends_with("_rate"), names_to = "model", names_pattern = "(.*)_rate", values_to = "rate") %>% 
  mutate_at(c("tf_prev", "rate"), round_2)

eval_long$model <- factor(eval_long$model, labels = all_models_score, 
                          levels = c("fe", "re", "geo", "global", "dummy", "dishwash", "country", "unif", "lnorm"))

## Score candidate models

score_tab <- score_long %>% mutate_at("model", as.character) %>% rowwise() %>%
  mutate(pdf = ifelse(model %in% unif_models, list(rep(unif_rate, times = length(q))), list(pdf_val(q, rate)))) %>% 
  mutate(cdf = list(pdf2cdf(pdf))) %>% mutate(area = round(last(cdf), 2)) 

score_tab <- score_tab %>% rowwise() %>% mutate(pdf = ifelse(area > 1, list(pdf/area), list(pdf))) %>% 
  mutate(cdf = ifelse(area > 1, list(pdf2cdf(pdf)), list(cdf))) %>%
  dplyr::select(- area) %>% mutate(area = round(last(cdf), 2)) 

score_tab <- score_tab  %>% rowwise() %>% 
  mutate(logs = log_score(tf_prev, cdf), crps = crps_score(tf_prev, cdf)) %>% 
  ungroup()

scores_test <- score_tab %>% group_by(model) %>% 
  summarise(mean_logs = round(mean(logs, na.rm = TRUE), 2),
            mean_crps = round(mean(crps, na.rm = TRUE), 2),
            sum_logs = round(sum(logs, na.rm = TRUE), 2),
            sum_crps = round(sum(crps, na.rm = TRUE), 2))

score_long <- score_tab %>% group_by(model, year) %>% 
  summarise(mean_logs = round(mean(logs, na.rm = TRUE), 2),
            mean_crps = round(mean(crps, na.rm = TRUE), 2),
            sum_logs = round(sum(logs, na.rm = TRUE), 2),
            sum_crps = round(sum(crps, na.rm = TRUE), 2))

saveRDS(scores_test, here("data", "scores.RDS"))
saveRDS(score_long, here("data", "scores_long.RDS"))
#saveRDS(score_tab, here("data", "score_tab.RDS"))

## Calculate weight of each model

scores_for_weights <- scores_test %>% 
  mutate(unif_logs = scores_test[which(scores_test$model == "Uniform"),]$sum_logs) %>%
  mutate(sum_logs = ifelse(sum_logs <= unif_logs, sum_logs, as.numeric(NA))) %>% 
  dplyr::select(model, sum_logs) %>% drop_na()

weights <- tibble(k = k_vec)
score_var <- c("k", "model", "score", "weight")

weights <- weights %>% cross_join(scores_for_weights) %>% group_by(k) %>%
  mutate(score = sum_logs - min(sum_logs)) %>%
  mutate(exp_score = exp(-1*score/k)) %>% group_by(k) %>% 
  mutate(sum_expscore = sum(exp_score, na.rm = TRUE)) %>% ungroup() %>%
  mutate(weight = exp_score/sum_expscore) %>%
  dplyr::select(any_of(score_var)) %>% drop_na() %>%
  mutate_at(c("weight", "score"), function(x) round(x, 2))

saveRDS(weights, here("data", "weights.RDS"))
