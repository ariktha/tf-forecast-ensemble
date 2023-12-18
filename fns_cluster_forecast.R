library("EnvStats") #for ppareto, dpareto
library("tidyverse")
IGN_STEP <- 0.5

clean_rawdata_reg <- function (raw_data, yrs = CLEAN_YRS, zero_thresh,key_cols, duplicates = FALSE) {
  # Remove duplicates.  (Some TF prevalences were assigned to groups of regions)
  if (duplicates == FALSE) {
    raw_data <- distinct(raw_data,admin0_id, admin1_id, year, tf_prev, .keep_all= TRUE)
  }
  # Remove NA
  raw_data <- raw_data[is.finite(raw_data$tf_prev),]
  # Remove unnecessary columns
  raw_data <- raw_data %>% dplyr::select(key_cols)  
  # Remove districts with essentially no TF
  raw_data <- raw_data %>% filter(tf_prev >= zero_thresh) # same as Pinset et al.
  # dplyr::select just relevant years and data values
  raw_data <- raw_data %>% filter(year %in% yrs)
  raw_data
}

coalesce_data <- function(base_data,min_in_region = MIN_IN_REGION, min_in_country = MIN_IN_COUNTRY) {
  country_clust <- base_data %>% group_by(admin0_id) %>% do({
    tibble(country_clust = ifelse(nrow(.) < min_in_country, 'sub_global', paste('Country_',.$admin0_id[1], sep = ""))) 
  })
  base_data <- left_join(base_data,country_clust)
  
  region_clust <- base_data %>% group_by(admin1_id) %>% do({
    tibble(region_clust = ifelse(nrow(.) < min_in_region, .$country_clust[1], paste('Region_',.$admin1_id[1], sep = ""))) 
  })
  base_data <- left_join(base_data,region_clust)
  
  district_clust <- base_data %>% group_by(admin2_id) %>% do({
    tibble(district_clust = ifelse(sum(!is.na(.$admin2_id)) < min_in_region, .$region_clust[1], paste('District_',.$admin2_id[1], sep = ""))) 
  })
  
  base_data <- left_join(base_data,district_clust)
  final_clust <- base_data  %>% group_by(district_clust) %>% do({
    tibble(final_clust = ifelse(nrow(.) >= min_in_region, .$district_clust[1], 'sub_global')) 
  })
  
  base_data <- left_join(base_data,final_clust, by = 'district_clust')
  
}

transform_coef <- function (idata,prefix) {
  colnames(idata)[colnames(idata) == "par_value"] <- paste0(prefix, "_rate")
  idata %>% ungroup() %>% dplyr::select(-c(fit, par_name))
}

logit <- function (p) {
  #log is taking during regression
  p/(1-p)
}

inv_logit <- function (a) {
  #exponentiation is taking post-regression
  a/(1+a)
}

det_init_par <- function (idata,fit_dist) {
  mean_data <- mean(idata$tf_prev)
  sd_data <- sd(idata$tf_prev)
  init_par <- list(rate = 1/mean_data)
  init_par
}

# Calculate PDF
pdf_fxn <- function (x,pdf_xval,fit_dist,zero_thresh) {
  if (any(is.na(x))) { 
    pdf_val <- 0
  } else if (fit_dist == "renorm_exp") {
    pdf_val <- dexp(pdf_xval,rate = x) / (pexp(100,rate = x) - pexp(zero_thresh, rate = x))
  }
}

# Dummy function to just help pass parameter functions around
setup_for_pdf <- function(par_data) {
  x<-NA
  if (par_data$fit[1] == "renorm_exp") {
    x <- as.numeric(par_data[par_data$par_name == "rate","par_value"])
  }
  x
}

# Calculate CDF
cdf_fxn <- function (x,cdf_xval,fit_dist, zero_thresh) {
  if (fit_dist == "renorm_exp") {
    cdf_val <- (pexp(cdf_xval,rate = x) - pexp(zero_thresh,rate = x)) / (pexp(100,rate = x) - pexp(zero_thresh, rate = x))
  }
  #  cdf_val[!is.finite(cdf_val) | is.na(cdf_val)] <- 1e-300
  cdf_val
}

calc_logL_pdf <- function(x,focal_data,model,weighted=FALSE,zero_thresh=ZERO_THRESH){
  num_var <- length(x)/2
  num_years <- length(unique(focal_data$year))
  x<-as.double(x)
  L_scores <- focal_data %>% group_by(year) %>% do({
    val_year <- exp(x[1:num_var] + (.$year[1]-2020)*x[(num_var+1):(2*num_var)])
    if (weighted == TRUE) {
      logL <- log(pdf_fxn(val_year,.$tf_prev,model,zero_thresh))/nrow(.)
    } else {
      logL <- log(pdf_fxn(val_year,.$tf_prev,model,zero_thresh))
    }
    data_frame(logL = logL)
  })
  logL <- sum(L_scores$logL)
}

mle_fit <- function (data,fits) {
  data_frame (fit = fits) %>% group_by(fit) %>% do({
    # Initialize optimizer
    print(.$fit[1])
    init_par <- det_init_par(data,.$fit[1])
    init_val <- c(as.double(init_par), rep(1,length(init_par)))
    # Optimize - note the log of the coefficients are passed along
    optim_res <- optim(init_val, function (x) - calc_logL_pdf(log(x),data,.$fit[1]))
    opt_par <- log(optim_res$par)
    num_val <- length(opt_par)/2
    data_frame(par_name = rep(names(init_par), 2), coef = rep(c("intercept","slope"),each = num_val), par_value = opt_par)
  })
}

det_coef <- function (mle_input, yrs = CLEAN_YRS) {
  mle_input %>% group_by(fit,par_name) %>% do({
    intercept <- .[.$coef == "intercept","par_value"]
    slope <- .[.$coef == "slope","par_value"]
    data_frame(year = yrs, par_value = exp(intercept$par_value + (yrs-2020)*slope$par_value))
  })
}

# Calculate log likelihood of a model (including time variation)

calc_pdf_bin <- function(par_val, fit, ignorance = 0, zero_thresh = ZERO_THRESH, pdf_width = PDF_WIDTH) {
#  browser()
  xvar = seq(zero_thresh + 0.5*pdf_width, 100 -0.5*pdf_width, pdf_width)
  delta = (max(xvar)-min(xvar))/(length(xvar)-1)/2
  yvar_pdf <- cdf_fxn(par_val, cdf_xval =  xvar + delta, fit_dist = fit, zero_thresh = zero_thresh) -
    cdf_fxn(par_val, cdf_xval =  xvar - delta, fit_dist = fit, zero_thresh = zero_thresh)
  data_frame(xvar = xvar, yvar_pdf = yvar_pdf)
}
