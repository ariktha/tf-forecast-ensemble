glbl_avg_train <- round(mean(train$tf_prev[which(train$year == train_thru)], na.rm = TRUE), 2)
dummy_rate_train <- round(1/glbl_avg_train, 2)

full_femod <- lm(log(tf_prev + const_log) ~ lag_tf + tf_rc + tt_rc + year + num_MDA, data = train, na.action = na.exclude)
fe_linmod_train <- stepAIC(full_femod, direction = "both", k = log(nrow(train)), trace = FALSE)

full_remod <- lmer(log(tf_prev + const_log) ~ lag_tf + tf_rc + tt_rc + year + num_MDA + (1|admin0_id), 
                   data = train, na.action = na.exclude)
re_linmod_step <- step(full_remod)
re_linmod_train <- get_model(re_linmod_step)

score$fe_pred <- as.numeric(exp(predict(fe_linmod_train, newdata = score)))
score$re_pred <- as.numeric(exp(predict(re_linmod_train, newdata = score, allow.new.levels = TRUE)))
fe_pred_se <- predict(fe_linmod_train, newdata = score, se.fit = TRUE)
score$lnorm_mean <- as.numeric(fe_pred_se$fit)
score$lnorm_sd <- se2sd(as.numeric(fe_pred_se$se.fit), nrow(score))

score <- score %>% mutate(fe_rate = 1/fe_pred, re_rate = 1/re_pred, dummy_rate = dummy_rate_train)

glbl_avg_retrain <- round(mean(re_train$tf_prev[which(re_train$year == max(score_years))], na.rm = TRUE), 2)
dummy_rate_retrain <- round(1/glbl_avg_retrain, 2)

full_femod <- lm(log(tf_prev + const_log) ~ lag_tf + tf_rc + year + num_MDA, data = re_train, na.action = na.exclude)
fe_linmod_retrain <- stepAIC(full_femod, direction = "both", k = log(nrow(re_train)), trace = FALSE)


full_remod <- lmer(log(tf_prev + const_log) ~ lag_tf + tf_rc + year + num_MDA + (1|admin0_id), 
                   data = re_train, na.action = na.exclude)
re_linmod_step <- step(full_remod)
re_linmod_retrain <- get_model(re_linmod_step)

forecast$fe_pred <- as.numeric(exp(predict(fe_linmod_retrain, newdata = forecast)))
forecast$re_pred <- as.numeric(exp(predict(re_linmod_retrain, newdata = forecast, allow.new.levels = TRUE)))
# fe_pred_se <- predict(fe_linmod_retrain, newdata = forecast, se.fit = TRUE)
# forecast$lnorm_mean <- as.numeric(fe_pred_se$fit)
# forecast$lnorm_sd <- se2sd(as.numeric(fe_pred_se$se.fit), nrow(forecast))

forecast <- forecast %>% mutate(fe_rate = 1/fe_pred, re_rate = 1/re_pred, dummy_rate = dummy_rate_retrain)

eval_data$fe_pred <- as.numeric(exp(predict(fe_linmod_retrain, newdata = eval_data)))
eval_data$re_pred <- as.numeric(exp(predict(re_linmod_retrain, newdata = eval_data, allow.new.levels = TRUE)))
# fe_pred_se <- predict(fe_linmod_retrain, newdata = eval_data, se.fit = TRUE)
# eval_data$lnorm_mean <- as.numeric(fe_pred_se$fit)
# eval_data$lnorm_sd <- se2sd(as.numeric(fe_pred_se$se.fit), nrow(eval_data))

eval_data <- eval_data %>% mutate(fe_rate = 1/fe_pred, re_rate = 1/re_pred, dummy_rate = dummy_rate_retrain)

rm(full_femod, full_remod, fe_linmod_train, fe_linmod_retrain, 
   re_linmod_train, re_linmod_retrain, re_linmod_step)