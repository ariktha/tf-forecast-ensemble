pred_year <- 2023
score_years <- c(2019, 2020, 2021)
evaluate_years <- c(2022)
train_thru <- 2018
earliest_year <- 2004
retrain_years <- earliest_year:max(score_years)

k_vec <- c(1, 10, 100, 1000)

redo_prev <- TRUE
redo_geodict <- TRUE
fit_geospatial <- TRUE
refit_geospatial <- TRUE
fit_seths <- TRUE
refit_seths <- TRUE

# cross_validate <- TRUE

CLEAN_YRS_train <- 2004:train_thru
ZERO_THRESH <- 0.5
KEY_COL_NAMES <- c("year", "tf_prev", "admin0_id", "admin1_id", "admin2_id", "admin3_id", "geo_id")
MIN_IN_REGION <- 30
MIN_IN_COUNTRY <- 50

const_log <- 0.005

thresh_years <- 5 # region_tf/country_tf will have mean tf of all data points in the last thresh_years
thresh_tf <- 0 # Data points with TF < thresh_tf removed

train_thru <- ifelse(train_thru < pred_year, train_thru, pred_year)
train_years <- earliest_year:ifelse(pred_year > train_thru, train_thru, pred_year) # Range of years used to train model

prev_cols <- c("geo_id", "year", "tf_prev", "tt_prev")

exp_models <- c("Fixed Effects", "Random Effects", "Geospatial", "Global", "Naive Exponential",
                "Region-based clusters", "Country-based clusters")
unif_models <- c("Uniform")
lnorm_model <- c("Log-normal")
unif_rate <- 0.01
all_models_eval <- union(exp_models, unif_models)
all_models_score <- union(all_models_eval, lnorm_model)
score_types <- c("CRPS", "Log Score")

perm_countries <- c("Benin", "Burkina Faso", "Burundi", "Cameroon", "Ethiopia",
                    "Guatemala", "Guinea Bissau", "Kenya", "Kiribati", "Malawi",
                    "Mali", "Mauritania", "Mozambique", "Nauru", "Nepal", "Niger",
                    "Nigeria", "Senegal", "South Sudan", "Tanzania", "The Gambia",
                    "Yemen", "Zambia", "Zanzibar", "Zimbabwe")

eg_geo <- 6188

q <- seq(0, 100, by = 0.01)
