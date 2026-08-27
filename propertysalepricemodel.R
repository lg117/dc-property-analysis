# DC Property Sale Price Analysis
# Office buildings, apartment buildings, and single-family / row homes
# Methods: hedonic pricing, repeat-sales analysis,
# ward-stratified modeling, assessment gap analysis, 2027 price projections

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(randomForest)
library(caret)
library(car)
library(ggplot2)
library(scales)
library(broom)
library(purrr)
library(rsample)
library(gt)

set.seed(1)
DATA_DIR <- "data"


# -------------------------------------------------------------------------
# PART A: HELPER FUNCTIONS
# -------------------------------------------------------------------------

# Groups with fewer than min_buildings properties get collapsed into "Other"
# so every category level appears in every cross-validation fold.
collapse_thin_categories <- function(df, group_col, id_col = "SSL", min_buildings = 4) {
  counts <- df %>%
    distinct(across(all_of(c(id_col, group_col)))) %>%
    count(across(all_of(group_col)), name = "n_buildings")
  thin <- counts %>% filter(n_buildings < min_buildings) %>% pull(all_of(group_col))
  df %>% mutate(across(all_of(group_col), ~ if_else(. %in% thin, "Other", .)))
}

# Adds sale sequence and appreciation features for each property over time.
# Tracks how many times a property has sold before, how long it was held,
# and how much the price changed between sales.
add_repeat_sales_features <- function(df, price_col = "sale_price") {
  df %>%
    arrange(SSL, sale_date) %>%
    group_by(SSL) %>%
    mutate(
      sale_number       = row_number(),
      n_prior_sales     = sale_number - 1,
      prev_sale_date    = lag(sale_date),
      prev_sale_price   = lag(.data[[price_col]]),
      holding_period_yrs = as.numeric(sale_date - prev_sale_date) / 365.25,
      pct_appreciation  = (.data[[price_col]] - prev_sale_price) / prev_sale_price * 100,
      log_return        = log(.data[[price_col]] / prev_sale_price),
      annualized_return = log_return / holding_period_yrs
    ) %>%
    ungroup()
}

# Adds sale quarter and a flag for COVID years (2020-2021).
add_time_features <- function(df) {
  df %>% mutate(
    sale_quarter = quarter(sale_date),
    covid_era    = if_else(sale_year %in% 2020:2021, 1, 0)
  )
}

# Returns year-over-year median price change with sale counts per year.
yoy_price_change <- function(df) {
  df %>%
    group_by(sale_year) %>%
    summarise(median_price = median(sale_price, na.rm = TRUE), n_sales = n(), .groups = "drop") %>%
    arrange(sale_year) %>%
    mutate(yoy_pct_change = (median_price - lag(median_price)) / lag(median_price) * 100)
}

# IAAO-standard assessment ratio analysis.
# Returns overall stats (COD, PRD), breakdowns by ward and year,
# and the 25 properties with the largest dollar gap between sale price and assessed value.
assessment_ratio_analysis <- function(df, assessed_col, ward_col = NULL) {
  ratio_df <- df %>%
    mutate(
      assessed_value = .data[[assessed_col]],
      assess_ratio   = assessed_value / sale_price,
      dollar_gap     = sale_price - assessed_value,
      pct_gap        = dollar_gap / sale_price * 100
    ) %>%
    filter(!is.na(assess_ratio), is.finite(assess_ratio))
  
  median_ratio       <- median(ratio_df$assess_ratio)
  cod                <- mean(abs(ratio_df$assess_ratio - median_ratio)) / median_ratio * 100
  weighted_mean_ratio <- sum(ratio_df$assessed_value) / sum(ratio_df$sale_price)
  simple_mean_ratio  <- mean(ratio_df$assess_ratio)
  prd                <- simple_mean_ratio / weighted_mean_ratio
  
  overall <- tibble(
    n            = nrow(ratio_df),
    median_ratio = median_ratio,
    mean_ratio   = simple_mean_ratio,
    COD          = cod,
    PRD          = prd
  )
  
  by_ward <- NULL
  if (!is.null(ward_col)) {
    ratio_df <- ratio_df %>% mutate(geo = .data[[ward_col]])
    by_ward <- ratio_df %>%
      group_by(geo) %>%
      summarise(
        n                 = n(),
        median_ratio      = median(assess_ratio),
        median_dollar_gap = median(dollar_gap),
        median_pct_gap    = median(pct_gap),
        .groups = "drop"
      ) %>%
      arrange(desc(median_dollar_gap))
  }
  
  by_year <- ratio_df %>%
    group_by(sale_year) %>%
    summarise(
      n              = n(),
      median_ratio   = median(assess_ratio),
      median_pct_gap = median(pct_gap),
      .groups = "drop"
    )
  
  top_gaps <- ratio_df %>%
    arrange(desc(dollar_gap)) %>%
    select(SSL, sale_date, sale_price, assessed_value, dollar_gap, pct_gap) %>%
    head(25)
  
  list(row_level = ratio_df, overall = overall, by_ward = by_ward, by_year = by_year, top_gaps = top_gaps)
}

# Fits a hedonic price model using only physical characteristics (no assessed value)
# and scores the test set. The gap between model-predicted price and assessed value
# flags properties where assessment may not reflect market conditions.
residual_gap_model <- function(train_data, test_data, formula, assessed_col) {
  fit <- lm(formula, data = train_data)
  test_data$pred_log_price      <- predict(fit, newdata = test_data)
  test_data$pred_price          <- exp(test_data$pred_log_price)
  test_data$model_vs_assessed_gap <- test_data$pred_price - test_data[[assessed_col]]
  list(fit = fit, scored = test_data)
}

# Grouped k-fold cross-validation. Folds are split by property (SSL) so a
# building's own sales never appear on both sides of a fold -- this prevents
# data leakage and gives honest out-of-sample error estimates.
# Folds that fail (e.g. a factor level only seen in test) are skipped with a warning.
grouped_cv <- function(data, formula, v = 5, model = "lm") {
  folds       <- group_vfold_cv(data, group = SSL, v = v)
  outcome_var <- all.vars(formula)[1]
  
  rmses <- map_dbl(folds$splits, function(split) {
    train <- analysis(split)
    test  <- assessment(split)
    tryCatch({
      fit   <- if (model == "lm") lm(formula, data = train) else randomForest(formula, data = train, ntree = 500)
      preds <- predict(fit, newdata = test)
      sqrt(mean((test[[outcome_var]] - preds)^2, na.rm = TRUE))
    }, error = function(e) {
      warning("Skipped a CV fold (", model, "): ", conditionMessage(e))
      NA_real_
    })
  })
  
  n_skipped <- sum(is.na(rmses))
  if (n_skipped > 0) {
    message(n_skipped, " of ", v, " folds skipped for model = '", model, "' -- likely a rare factor level in one fold.")
  }
  
  tibble(
    model        = model,
    mean_rmse    = mean(rmses, na.rm = TRUE),
    sd_rmse      = sd(rmses, na.rm = TRUE),
    n_folds_used = v - n_skipped
  )
}

# Plots median sale price, median price per sq ft, and sale volume by year.
# Point size reflects sale count -- small points signal noisy estimates.
plot_time_trends <- function(df, title_prefix = "") {
  yearly <- df %>%
    group_by(sale_year) %>%
    summarise(
      med_price = median(sale_price, na.rm = TRUE),
      med_psft  = median(psft, na.rm = TRUE),
      n         = n(),
      .groups   = "drop"
    )
  
  p_price <- ggplot(yearly, aes(sale_year, med_price)) +
    geom_line(color = "#2c7fb8", linewidth = 1) +
    geom_point(aes(size = n), color = "#2c7fb8") +
    scale_y_continuous(labels = label_dollar()) +
    labs(
      title    = paste(title_prefix, "Median Sale Price by Year"),
      subtitle = "Point size = number of sales (small points = noisy estimate)",
      x = "Sale Year", y = "Median Price", size = "N sales"
    ) +
    theme_minimal(base_size = 13)
  
  p_psft <- ggplot(yearly, aes(sale_year, med_psft)) +
    geom_line(color = "#41ab5d", linewidth = 1) +
    geom_point(aes(size = n), color = "#41ab5d") +
    scale_y_continuous(labels = label_dollar()) +
    labs(
      title = paste(title_prefix, "Median $/SqFt by Year"),
      x = "Sale Year", y = "Median $/SqFt", size = "N sales"
    ) +
    theme_minimal(base_size = 13)
  
  p_volume <- ggplot(yearly, aes(sale_year, n)) +
    geom_col(fill = "#807dba") +
    labs(title = paste(title_prefix, "Sale Volume by Year"), x = "Sale Year", y = "Number of Sales") +
    theme_minimal(base_size = 13)
  
  list(price_trend = p_price, psft_trend = p_psft, volume_trend = p_volume, yearly_table = yearly)
}

# Plots median assessment ratio over time.
# A ratio below 1 means properties are being assessed below their actual sale price.
plot_assessment_ratio_trend <- function(ratio_by_year, title_prefix = "") {
  ggplot(ratio_by_year, aes(sale_year, median_ratio)) +
    geom_line(color = "#e6550d", linewidth = 1) +
    geom_point(color = "#e6550d") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
    labs(
      title    = paste(title_prefix, "Assessment Ratio Over Time"),
      subtitle = "Below the dashed line = properties assessed below sale price",
      x = "Sale Year", y = "Median Assessment Ratio"
    ) +
    theme_minimal(base_size = 13)
}

# Quick overview table: sale count, property count, median price, median price/sqft.
summary_table <- function(df, title) {
  df %>%
    summarise(
      `Number of sales`      = n(),
      `Number of properties` = n_distinct(SSL),
      `Median sale price`    = median(sale_price, na.rm = TRUE),
      `Median price / sq ft` = median(psft, na.rm = TRUE)
    ) %>%
    gt() %>%
    fmt_currency(columns = c(`Median sale price`, `Median price / sq ft`), decimals = 0) %>%
    tab_header(title = title)
}

# Pulls the n_prior_sales coefficient from a log-price regression and converts
# it to a plain-language percentage: "each additional prior sale is associated
# with an X% change in price." Flags whether the result is statistically significant.
quantify_resale_effect <- function(model, label = "") {
  tt    <- tidy(model)
  row   <- tt %>% filter(term == "n_prior_sales")
  n_obs <- nobs(model)
  
  if (nrow(row) == 0) {
    cat(sprintf("[%s] n_prior_sales not found in model.\n", label))
    return(invisible(NULL))
  }
  
  pct_per_sale <- (exp(row$estimate) - 1) * 100
  sig_flag <- if (row$p.value < 0.05) {
    "statistically significant (p < 0.05)"
  } else {
    "not statistically significant -- treat as inconclusive, not as evidence of no effect"
  }
  
  cat(sprintf(
    "[%s] Each additional prior sale is associated with a %.2f%% change in price (p = %.3f, n = %d) -- %s.\n",
    label, pct_per_sale, row$p.value, n_obs, sig_flag
  ))
  invisible(tibble(segment = label, pct_change_per_sale = pct_per_sale, p_value = row$p.value, n = n_obs))
}

# Pulls the sale_year coefficient from a log-price regression and converts it
# to a plain-language percentage: "holding characteristics fixed, price changes
# X% per calendar year." More reliable than raw year-over-year medians because
# it controls for differences in the mix of properties sold each year.
quantify_year_trend <- function(model, label = "") {
  tt  <- tidy(model)
  row <- tt %>% filter(term == "sale_year")
  
  if (nrow(row) == 0) {
    cat(sprintf("[%s] sale_year not found in model.\n", label))
    return(invisible(NULL))
  }
  
  pct_per_year <- (exp(row$estimate) - 1) * 100
  sig_flag <- if (row$p.value < 0.05) "statistically significant" else "not statistically significant"
  
  cat(sprintf(
    "[%s] Holding building characteristics fixed, price changes an estimated %.2f%% per year (p = %.3f) -- %s.\n",
    label, pct_per_year, row$p.value, sig_flag
  ))
  invisible(tibble(segment = label, pct_change_per_year = pct_per_year, p_value = row$p.value))
}

# Fits a separate hedonic model for each ward that has at least min_n qualified sales.
# Ward-stratified models let coefficients (size elasticity, price trend, resale effect)
# vary by sub-market rather than forcing one citywide average.
ward_stratified_models <- function(df, formula, ward_col = "PRMS_WARD", min_n = 200) {
  eligible_wards <- df %>%
    count(across(all_of(ward_col))) %>%
    filter(n >= min_n) %>%
    pull(all_of(ward_col))
  
  results <- map(eligible_wards, function(w) {
    sub        <- df %>% filter(.data[[ward_col]] == w)
    fit        <- lm(formula, data = sub)
    cv         <- grouped_cv(sub, formula, model = "lm")
    resale_sub <- add_repeat_sales_features(sub)
    resale_fit <- lm(log(sale_price) ~ n_prior_sales + sale_year, data = resale_sub)
    list(ward = w, n = nrow(sub), fit = fit, cv = cv, resale_fit = resale_fit, data = sub)
  })
  
  names(results) <- paste0("ward_", eligible_wards)
  results
}

# Summarizes ward-stratified models into one comparison table:
# size elasticity, cross-validated RMSE, resale effect, and year trend per ward.
summarize_ward_models <- function(ward_models, size_term = "log_gba") {
  map_dfr(ward_models, function(w) {
    tt         <- tidy(w$fit)
    size_row   <- tt %>% filter(term == size_term)
    resale_pct <- (exp(coef(w$resale_fit)["n_prior_sales"]) - 1) * 100
    year_pct   <- (exp(coef(w$resale_fit)["sale_year"]) - 1) * 100
    tibble(
      ward                = w$ward,
      n_sales             = w$n,
      size_elasticity     = if (nrow(size_row) > 0) size_row$estimate else NA_real_,
      cv_rmse             = w$cv$mean_rmse,
      resale_pct_per_sale = resale_pct,
      price_pct_per_year  = year_pct
    )
  }) %>% arrange(desc(n_sales))
}

# Projects expected sale price in a target year for a "typical" property
# (characteristics set to medians). Rather than a single point estimate,
# this simulates uncertainty by sampling from the model's residual spread,
# then buckets the results into price ranges with probabilities -- e.g.
# "70% chance the property sells for $2.1M-$2.6M."
# Note: this extrapolates the model's year trend -- it is not a forecast.
expected_price_projection <- function(model, typical_property, target_year,
                                      sale_year_var = "sale_year", n_sims = 5000,
                                      n_bins = 5, label = "") {
  newdata <- typical_property
  newdata[[sale_year_var]] <- target_year
  
  pred_log <- withCallingHandlers(
    predict(model, newdata = newdata),
    warning = function(w) {
      message(sprintf("[%s] predict() warning: %s", label, conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  
  if (any(is.na(pred_log))) {
    cat(sprintf("[%s] str(newdata) passed to predict():\n", label))
    str(newdata)
    cat(sprintf("[%s] model$xlevels (factor levels the model was trained on):\n", label))
    print(model$xlevels)
    stop(paste0(
      "[", label, "] predict() returned NA. Likely cause: a factor column in typical_property ",
      "(e.g. GRADE, Submarket, CNDTN_D) has a value not seen during training. ",
      "Compare the values above against model$xlevels."
    ))
  }
  
  resid_sd <- summary(model)$sigma
  if (is.na(resid_sd) || resid_sd <= 0) {
    stop(paste0(
      "[", label, "] Model has no residual degrees of freedom (too many predictors for the data). ",
      "Simplify the formula or collapse more categories before projecting."
    ))
  }
  
  sims      <- pred_log + rnorm(n_sims, mean = 0, sd = resid_sd)
  sim_price <- exp(sims)
  
  breaks         <- quantile(sim_price, probs = seq(0, 1, length.out = n_bins + 1))
  breaks[1]      <- -Inf
  breaks[length(breaks)] <- Inf
  bin_id         <- cut(sim_price, breaks = breaks, include.lowest = TRUE)
  
  branch_table <- tibble(sim_price = sim_price, branch = bin_id) %>%
    group_by(branch) %>%
    summarise(
      probability              = n() / n_sims,
      expected_price_in_branch = mean(sim_price),
      min_price                = min(sim_price),
      max_price                = max(sim_price),
      .groups = "drop"
    ) %>%
    arrange(min_price) %>%
    mutate(
      branch_label  = paste0("Branch ", row_number()),
      property_type = label,
      target_year   = target_year
    ) %>%
    select(property_type, target_year, branch_label, probability, expected_price_in_branch, min_price, max_price)
  
  overall_ev <- mean(sim_price)
  cat(sprintf(
    "[%s] Expected sale price in %d: $%s (median: $%s)\n",
    label, target_year, comma(round(overall_ev)), comma(round(median(sim_price)))
  ))
  
  list(branches = branch_table, overall_expected_value = overall_ev, simulations = sim_price)
}


# -------------------------------------------------------------------------
# PART B: OFFICE BUILDINGS
# -------------------------------------------------------------------------
# Note: this dataset has no assessed-value column, so assessment gap
# analysis is skipped for office. No ward column either -- Submarket
# is used as the geographic grouping.

raw_office <- read_excel(file.path(DATA_DIR, "OFFICE_BLDGS_MoreThan1_All510.xlsx"), sheet = "Sheet1")

OFFICE_WARD_COL <- "Submarket"

df_office <- raw_office %>%
  mutate(
    SSL             = str_squish(SSL),
    sale_date       = ymd(str_sub(SALE_DATE, 1, 10)),
    sale_price      = as.numeric(SALE_PRICE),
    qualified       = QUALIFIED,
    bldg_sf         = as.numeric(str_remove_all(as.character(`Building Size (SF)`), ",")),
    log_bldg_sf     = log(bldg_sf),  # pre-computed because randomForest can't evaluate log() inline
    pct_leased      = as.numeric(`% Leased`),
    sale_year       = year(sale_date),
    bldg_age_at_sale = sale_year - AYB,
    psft            = sale_price / bldg_sf
  ) %>%
  filter(sale_price > 0, bldg_sf > 0, !is.na(sale_date), !is.na(bldg_sf)) %>%
  add_time_features()

hedonic_office <- df_office %>%
  filter(qualified == "Q") %>%
  mutate(log_price = log(sale_price), log_psft = log(psft), GRADE = factor(GRADE)) %>%
  select(SSL, sale_date, sale_year, sale_quarter, covid_era, sale_price, log_price, psft, log_psft,
         bldg_sf, log_bldg_sf, GRADE, AYB, bldg_age_at_sale, Stories, pct_leased, Submarket) %>%
  drop_na(bldg_sf, log_bldg_sf, GRADE, bldg_age_at_sale, sale_year, Stories, pct_leased) %>%
  mutate(GRADE = as.character(GRADE)) %>%
  collapse_thin_categories(group_col = "GRADE", min_buildings = 4) %>%
  collapse_thin_categories(group_col = "Submarket", min_buildings = 4) %>%
  mutate(GRADE = factor(GRADE), Submarket = factor(Submarket))

# 80/20 train/test split by building (not by sale row)
office_bldgs     <- unique(hedonic_office$SSL)
office_train_ids <- sample(office_bldgs, floor(0.80 * length(office_bldgs)))
office_train     <- hedonic_office %>% filter(SSL %in% office_train_ids)
office_test      <- hedonic_office %>% filter(!SSL %in% office_train_ids)

office_formula <- log_price ~ log_bldg_sf + GRADE + bldg_age_at_sale + sale_year + Stories + pct_leased + Submarket

office_ols <- lm(office_formula, data = office_train)
summary(office_ols)

office_rf <- randomForest(office_formula, data = office_train, ntree = 500, importance = TRUE)
varImpPlot(office_rf)

office_cv_ols <- grouped_cv(hedonic_office, office_formula, model = "lm")
office_cv_rf  <- grouped_cv(hedonic_office, office_formula, model = "rf")
bind_rows(office_cv_ols, office_cv_rf)

# Repeat-sales model: does having sold before affect price?
office_repeat       <- add_repeat_sales_features(df_office %>% filter(qualified == "Q"))
office_resale_model <- lm(
  log(sale_price) ~ n_prior_sales + log_bldg_sf + bldg_age_at_sale + sale_year,
  data = office_repeat %>% mutate(log_bldg_sf = log(bldg_sf))
)
summary(office_resale_model)
quantify_resale_effect(office_resale_model, label = "Office")
quantify_year_trend(office_resale_model, label = "Office")

office_trends <- plot_time_trends(df_office %>% filter(qualified == "Q"), "Office --")
office_trends$price_trend
office_trends$psft_trend
office_trends$volume_trend
office_trends$yearly_table
yoy_price_change(df_office %>% filter(qualified == "Q"))
summary_table(df_office %>% filter(qualified == "Q"), "DC Office Buildings -- Overview")


# -------------------------------------------------------------------------
# PART C: APARTMENT BUILDINGS
# -------------------------------------------------------------------------
# Note: Building Size (SF) is only populated for about 1/3 of rows in this
# extract -- the hedonic model runs on that smaller subset.

raw_apt <- read_excel(file.path(DATA_DIR, "aptbuildings.xlsx"), sheet = "Sheet1")

APT_ASSESSED_COL <- "ASSESSMENT"
APT_WARD_COL     <- "PRMS_WARD"

df_apt <- raw_apt %>%
  mutate(
    SSL              = str_squish(SSL),
    sale_date        = ymd(str_sub(SALE_DATE, 1, 10)),
    sale_price       = as.numeric(SALE_PRICE),
    qualified        = QUALIFIED,
    bldg_sf          = as.numeric(str_remove_all(as.character(`Building Size (SF)`), ",")),
    log_bldg_sf      = log(bldg_sf),
    sale_year        = year(sale_date),
    bldg_age_at_sale = sale_year - AYB,
    psft             = sale_price / bldg_sf,
    PRMS_WARD        = factor(PRMS_WARD)
  ) %>%
  filter(sale_price > 0, bldg_sf > 0, !is.na(sale_date), !is.na(bldg_sf)) %>%
  add_time_features()

hedonic_apt <- df_apt %>%
  filter(qualified == "Q") %>%
  mutate(log_price = log(sale_price), log_psft = log(psft), GRADE = factor(GRADE)) %>%
  select(SSL, sale_date, sale_year, sale_quarter, covid_era, sale_price, log_price, psft, log_psft,
         bldg_sf, log_bldg_sf, GRADE, AYB, bldg_age_at_sale, PRMS_WARD, ASSESSMENT) %>%
  drop_na(bldg_sf, log_bldg_sf, GRADE, bldg_age_at_sale, sale_year) %>%
  mutate(GRADE = as.character(GRADE)) %>%
  collapse_thin_categories(group_col = "GRADE", min_buildings = 4) %>%
  mutate(GRADE = factor(GRADE))

apt_bldgs     <- unique(hedonic_apt$SSL)
apt_train_ids <- sample(apt_bldgs, floor(0.80 * length(apt_bldgs)))
apt_train     <- hedonic_apt %>% filter(SSL %in% apt_train_ids)
apt_test      <- hedonic_apt %>% filter(!SSL %in% apt_train_ids)

# OLS uses poly() to allow a curved age effect. Random Forest handles
# nonlinearity automatically so the plain column is used there instead.
apt_ols_formula <- log_price ~ log_bldg_sf + GRADE + poly(bldg_age_at_sale, 2) + sale_year
apt_rf_formula  <- log_price ~ log_bldg_sf + GRADE + bldg_age_at_sale + sale_year

apt_ols <- lm(apt_ols_formula, data = apt_train)
summary(apt_ols)
vif(apt_ols)

apt_rf <- randomForest(apt_rf_formula, data = apt_train, ntree = 1000, mtry = 2, importance = TRUE)
varImpPlot(apt_rf)

apt_cv_ols <- grouped_cv(hedonic_apt, apt_ols_formula, model = "lm")
apt_cv_rf  <- grouped_cv(hedonic_apt, apt_rf_formula, model = "rf")
bind_rows(apt_cv_ols, apt_cv_rf)

apt_repeat       <- add_repeat_sales_features(df_apt %>% filter(qualified == "Q"))
apt_resale_model <- lm(
  log(sale_price) ~ n_prior_sales + log_bldg_sf + bldg_age_at_sale + sale_year,
  data = apt_repeat %>% mutate(log_bldg_sf = log(bldg_sf))
)
summary(apt_resale_model)
quantify_resale_effect(apt_resale_model, label = "Apartment")
quantify_year_trend(apt_resale_model, label = "Apartment")

apt_gap <- assessment_ratio_analysis(
  df_apt %>% filter(qualified == "Q"),
  assessed_col = APT_ASSESSED_COL,
  ward_col     = APT_WARD_COL
)
apt_gap$overall
apt_gap$by_ward
apt_gap$by_year
apt_gap$top_gaps

apt_residual_gap <- residual_gap_model(
  apt_train, apt_test,
  formula      = log_price ~ log_bldg_sf + GRADE + poly(bldg_age_at_sale, 2),
  assessed_col = APT_ASSESSED_COL
)

apt_trends <- plot_time_trends(df_apt %>% filter(qualified == "Q"), "Apartment --")
apt_trends$price_trend
apt_trends$psft_trend
apt_trends$volume_trend
apt_trends$yearly_table
plot_assessment_ratio_trend(apt_gap$by_year, "Apartment --")
summary_table(df_apt %>% filter(qualified == "Q"), "DC Apartment Buildings -- Overview")


# -------------------------------------------------------------------------
# PART D: SINGLE-FAMILY AND ROW HOUSES -- WARD-STRATIFIED
# -------------------------------------------------------------------------
# ~48k sales across ~15k properties. Large enough that fitting a separate
# model per ward gives more reliable sub-market estimates than a single
# pooled model with ward as one predictor.

raw_sfh <- read_excel(file.path(DATA_DIR, "SINGLE_FAM_row_hstd1_resale_multis_48k.xlsx"), sheet = "Sheet1")

SFH_ASSESSED_COL <- "ASSESSMENT"
SFH_WARD_COL     <- "PRMS_WARD"

df_sfh <- raw_sfh %>%
  mutate(
    SSL        = str_squish(SSL),
    sale_date  = ymd(str_sub(SALE_DATE, 1, 10)),
    sale_price = as.numeric(SALE_PRICE),
    qualified  = QUALIFIED,
    gba        = as.numeric(GBA),
    log_gba    = log(gba),
    sale_year  = year(sale_date),
    age_at_sale = sale_year - EYB,
    psft       = sale_price / gba,
    CNDTN_D    = factor(CNDTN_D),
    PRMS_WARD  = factor(PRMS_WARD)
  ) %>%
  filter(qualified == "Q", sale_price > 0, gba > 0, !is.na(sale_date), !is.na(gba)) %>%
  add_time_features()

hedonic_sfh <- df_sfh %>%
  mutate(log_price = log(sale_price), log_psft = log(psft)) %>%
  select(SSL, sale_date, sale_year, sale_quarter, covid_era, sale_price, log_price, psft, log_psft,
         gba, log_gba, BEDRM, BATHRM, ROOMS, STORIES, age_at_sale, CNDTN_D, PRMS_WARD, ASSESSMENT) %>%
  drop_na(gba, BEDRM, BATHRM, ROOMS, STORIES, age_at_sale, CNDTN_D, PRMS_WARD) %>%
  mutate(CNDTN_D = as.character(CNDTN_D)) %>%
  collapse_thin_categories(group_col = "CNDTN_D", min_buildings = 4) %>%
  mutate(CNDTN_D = factor(CNDTN_D))

sfh_formula_within_ward <- log_price ~ log_gba + BEDRM + BATHRM + ROOMS + STORIES + CNDTN_D + poly(age_at_sale, 2) + sale_year

sfh_ward_models <- ward_stratified_models(
  hedonic_sfh, sfh_formula_within_ward, ward_col = "PRMS_WARD", min_n = 200
)

# Ward comparison table: size elasticity, RMSE, resale effect, year trend
sfh_ward_summary <- summarize_ward_models(sfh_ward_models, size_term = "log_gba")
print(sfh_ward_summary)

# Plain-language resale and year-trend statements for each ward
walk(sfh_ward_models, function(w) {
  quantify_resale_effect(w$resale_fit, label = paste0("SFH Ward ", w$ward))
  quantify_year_trend(w$resale_fit,   label = paste0("SFH Ward ", w$ward))
})

# Pooled model (all wards, ward as predictor) -- used for the Part F projection
sfh_bldgs     <- unique(hedonic_sfh$SSL)
sfh_train_ids <- sample(sfh_bldgs, floor(0.80 * length(sfh_bldgs)))
sfh_train     <- hedonic_sfh %>% filter(SSL %in% sfh_train_ids)
sfh_test      <- hedonic_sfh %>% filter(!SSL %in% sfh_train_ids)

sfh_ols_formula <- log_price ~ log_gba + BEDRM + BATHRM + ROOMS + STORIES + CNDTN_D + poly(age_at_sale, 2) + sale_year + PRMS_WARD
sfh_rf_formula  <- log_price ~ log_gba + BEDRM + BATHRM + ROOMS + STORIES + CNDTN_D + age_at_sale + sale_year + PRMS_WARD

sfh_ols <- lm(sfh_ols_formula, data = sfh_train)
summary(sfh_ols)
vif(sfh_ols)

sfh_rf <- randomForest(sfh_rf_formula, data = sfh_train, ntree = 500, importance = TRUE)
varImpPlot(sfh_rf)

sfh_cv_ols <- grouped_cv(hedonic_sfh, sfh_ols_formula, model = "lm")
sfh_cv_rf  <- grouped_cv(hedonic_sfh, sfh_rf_formula, model = "rf")
bind_rows(sfh_cv_ols, sfh_cv_rf)

sfh_repeat       <- add_repeat_sales_features(df_sfh)
sfh_resale_model <- lm(
  log(sale_price) ~ n_prior_sales + log_gba + age_at_sale + sale_year,
  data = sfh_repeat %>% mutate(log_gba = log(gba))
)
summary(sfh_resale_model)
quantify_resale_effect(sfh_resale_model, label = "Single-Family (pooled)")
quantify_year_trend(sfh_resale_model, label = "Single-Family (pooled)")

sfh_gap <- assessment_ratio_analysis(df_sfh, assessed_col = SFH_ASSESSED_COL, ward_col = SFH_WARD_COL)
sfh_gap$overall
sfh_gap$by_ward
sfh_gap$by_year
sfh_gap$top_gaps

sfh_residual_gap <- residual_gap_model(
  sfh_train, sfh_test,
  formula      = log_price ~ log_gba + BEDRM + BATHRM + ROOMS + STORIES + CNDTN_D + poly(age_at_sale, 2),
  assessed_col = SFH_ASSESSED_COL
)

sfh_trends <- plot_time_trends(df_sfh, "Single-Family --")
sfh_trends$price_trend
sfh_trends$psft_trend
sfh_trends$volume_trend
sfh_trends$yearly_table
plot_assessment_ratio_trend(sfh_gap$by_year, "Single-Family --")
summary_table(df_sfh, "DC Single-Family / Row House Sales -- Overview")

sfh_ward_trends <- df_sfh %>%
  group_by(PRMS_WARD, sale_year) %>%
  summarise(median_price = median(sale_price, na.rm = TRUE), n = n(), .groups = "drop")

ggplot(sfh_ward_trends, aes(sale_year, median_price)) +
  geom_line(color = "#2c7fb8") +
  facet_wrap(~ PRMS_WARD, scales = "free_y") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Single-Family Median Sale Price by Year, by Ward", x = "Sale Year", y = "Median Price") +
  theme_minimal(base_size = 11)


# -------------------------------------------------------------------------
# PART E: CROSS-PROPERTY-TYPE SUMMARY
# -------------------------------------------------------------------------

combined_ward_gaps <- bind_rows(
  sfh_gap$by_ward %>% mutate(property_type = "Single-Family")
  # Office excluded -- no assessed-value column in this extract
) %>% arrange(desc(median_dollar_gap))

combined_ward_gaps %>%
  gt(groupname_col = "property_type") %>%
  fmt_currency(columns = median_dollar_gap, decimals = 0) %>%
  fmt_number(columns = median_pct_gap, decimals = 1) %>%
  tab_header(title = "DC Revenue Risk -- Median Assessment Gap by Ward and Property Type")

combined_model_metrics <- bind_rows(
  office_cv_ols %>% mutate(property_type = "Office"),
  office_cv_rf  %>% mutate(property_type = "Office"),
  apt_cv_ols    %>% mutate(property_type = "Apartment"),
  apt_cv_rf     %>% mutate(property_type = "Apartment"),
  sfh_cv_ols    %>% mutate(property_type = "Single-Family (pooled)"),
  sfh_cv_rf     %>% mutate(property_type = "Single-Family (pooled)")
)
print(combined_model_metrics)

combined_resale_effects <- bind_rows(
  quantify_resale_effect(office_resale_model, label = "Office"),
  quantify_resale_effect(apt_resale_model,    label = "Apartment"),
  quantify_resale_effect(sfh_resale_model,    label = "Single-Family (pooled)")
)
print(combined_resale_effects)

write.csv(combined_ward_gaps,      "combined_ward_assessment_gaps.csv",  row.names = FALSE)
write.csv(combined_model_metrics,  "combined_model_metrics.csv",         row.names = FALSE)
write.csv(sfh_ward_summary,        "sfh_ward_summary.csv",               row.names = FALSE)
write.csv(combined_resale_effects, "combined_resale_effects.csv",        row.names = FALSE)


# -------------------------------------------------------------------------
# PART F: 2027 PRICE PROJECTIONS
# -------------------------------------------------------------------------
# For each property type, set building characteristics to medians (a "typical"
# property), push the sale year to 2027, and simulate residual uncertainty
# forward to get a probability table of price outcomes.
# This extrapolates the model's learned year trend -- it is not a true forecast.

TARGET_YEAR <- 2027

office_typical <- office_train %>%
  summarise(
    log_bldg_sf      = median(log_bldg_sf),
    bldg_age_at_sale = median(bldg_age_at_sale),
    Stories          = median(Stories),
    pct_leased       = median(pct_leased),
    GRADE            = names(sort(table(GRADE),     decreasing = TRUE))[1],
    Submarket        = names(sort(table(Submarket), decreasing = TRUE))[1]
  ) %>%
  mutate(
    GRADE     = factor(GRADE,     levels = levels(office_train$GRADE)),
    Submarket = factor(Submarket, levels = levels(office_train$Submarket))
  )

office_2027 <- expected_price_projection(office_ols, office_typical, TARGET_YEAR, label = "Office")
office_2027$branches

apt_typical <- apt_train %>%
  summarise(
    log_bldg_sf      = median(log_bldg_sf),
    bldg_age_at_sale = median(bldg_age_at_sale),
    GRADE            = names(sort(table(GRADE), decreasing = TRUE))[1]
  ) %>%
  mutate(GRADE = factor(GRADE, levels = levels(apt_train$GRADE)))

apt_2027 <- expected_price_projection(apt_ols, apt_typical, TARGET_YEAR, label = "Apartment")
apt_2027$branches

# Single-family: one projection per ward since Part D showed price levels
# and trends differ meaningfully by sub-market.
sfh_ward_2027 <- map_dfr(sfh_ward_models, function(w) {
  typical <- w$data %>%
    summarise(
      log_gba     = median(log_gba),
      BEDRM       = round(median(BEDRM)),
      BATHRM      = round(median(BATHRM)),
      ROOMS       = round(median(ROOMS)),
      STORIES     = round(median(STORIES)),
      age_at_sale = median(age_at_sale),
      CNDTN_D     = names(sort(table(CNDTN_D), decreasing = TRUE))[1]
    ) %>%
    mutate(CNDTN_D = factor(CNDTN_D, levels = levels(w$data$CNDTN_D)))
  proj <- expected_price_projection(w$fit, typical, TARGET_YEAR, label = paste0("SFH Ward ", w$ward))
  proj$branches
})
print(sfh_ward_2027)

write.csv(
  bind_rows(office_2027$branches, apt_2027$branches, sfh_ward_2027),
  "expected_price_projections_2027.csv",
  row.names = FALSE
)


# -------------------------------------------------------------------------
# CHARTS
# -------------------------------------------------------------------------

# Size elasticity by ward
ggplot(sfh_ward_summary, aes(x = reorder(ward, size_elasticity), y = size_elasticity)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title    = "Single-Family Size Elasticity by Ward",
    subtitle = "% price change per 1% increase in home size",
    x = "Ward", y = "Size elasticity"
  ) +
  theme_minimal(base_size = 13)

# Model accuracy comparison (OLS vs Random Forest)
sfh_accuracy <- combined_model_metrics %>% filter(property_type == "Single-Family (pooled)")

ggplot(sfh_accuracy, aes(x = model, y = mean_rmse, fill = model)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean_rmse - sd_rmse, ymax = mean_rmse + sd_rmse), width = 0.15) +
  scale_fill_manual(values = c("lm" = "#2c7fb8", "rf" = "#41ab5d")) +
  labs(
    title    = "Single-Family Model Accuracy",
    subtitle = "Cross-validated prediction error (lower is better)",
    x = NULL, y = "Mean RMSE", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Resale effect by ward (significant wards highlighted)
sfh_ward_summary <- sfh_ward_summary %>%
  mutate(significant = case_when(
    ward %in% c("1", "2", "4", "7") ~ "Significant",
    TRUE ~ "Not significant"
  ))

ggplot(sfh_ward_summary, aes(x = reorder(ward, resale_pct_per_sale), y = resale_pct_per_sale, fill = significant)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Significant" = "#2c7fb8", "Not significant" = "#c6c6c6")) +
  coord_flip() +
  labs(
    title    = "Single-Family Resale Effect by Ward",
    subtitle = "% price change per additional prior sale",
    x = "Ward", y = "% change per sale", fill = NULL
  ) +
  theme_minimal(base_size = 13)

# Year-over-year appreciation by ward
ggplot(sfh_ward_summary, aes(x = reorder(ward, price_pct_per_year), y = price_pct_per_year)) +
  geom_col(fill = "#41ab5d") +
  coord_flip() +
  labs(
    title    = "Single-Family Annual Appreciation by Ward",
    subtitle = "% price change per year, holding characteristics fixed",
    x = "Ward", y = "% change per year"
  ) +
  theme_minimal(base_size = 13)

# Assessment gap by ward
ward_gap_plot <- sfh_gap$by_ward %>%
  mutate(Ward = factor(geo, levels = sort(unique(geo))))

ggplot(ward_gap_plot, aes(x = Ward, y = median_dollar_gap)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"), name = "Median Sale Price - Assessed Value") +
  labs(
    title    = "Assessment Gap by Ward",
    subtitle = "Positive = property sold above its assessed value",
    x = "Ward"
  ) +
  theme_minimal(base_size = 13)

# Where the 25 largest assessment gaps fall by year
all_years <- sfh_gap$row_level %>% filter(!is.na(sale_year)) %>% distinct(sale_year)

top25_by_year <- sfh_gap$top_gaps %>%
  mutate(sale_year = year(sale_date)) %>%
  count(sale_year, name = "top25_cases")

top25_by_year <- all_years %>%
  left_join(top25_by_year, by = "sale_year") %>%
  mutate(top25_cases = replace_na(top25_cases, 0)) %>%
  arrange(sale_year)

ggplot(top25_by_year, aes(x = sale_year, y = top25_cases)) +
  geom_col() +
  scale_x_continuous(breaks = seq(min(top25_by_year$sale_year), max(top25_by_year$sale_year), by = 2)) +
  labs(
    title = "Largest Assessment Gaps Are Concentrated in Recent Sales",
    x     = "Sale Year",
    y     = "Number of Top-25 Cases"
  ) +
  theme_minimal(base_size = 13)

# Regression equation image (for presentations)
png("equation.png", width = 2600, height = 550, res = 200, bg = "white")
par(mar = c(0, 0, 0, 0))
plot.new()
text(
  0.5, 0.5,
  expression(
    log("Sale Price") ==
      beta[0] + beta[1] * log("Building Size") + beta[2] * "Grade" +
      beta[3] * "Age" + beta[4] * "Sale Year" + beta[5] * "Stories" +
      beta[6] * "% Leased" + beta[7] * "Location" + beta[8] * "Prior Sales" + epsilon
  ),
  cex = 1.7, col = "black"
)
dev.off()