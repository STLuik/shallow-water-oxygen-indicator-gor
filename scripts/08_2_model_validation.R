# scripts/08_2_model_validation.R
# This script validates the smoothed seasonal/monthly profile fields created by
# scripts/08_1_smooth_seasonal_profiles.R.
#
# It compares the input values used by the smoother with the matching smoothed
# values at the same time-depth grid points.
#
# Main outputs:
#   1. A long validation table with observed and smoothed values.
#   2. A metrics table for each variable.
#   3. Figures showing observed vs smoothed values and residual patterns.

options(project_clean_workspace = FALSE)

source("scripts/01_header.R")

if (is.null(getOption("project_assessment"))) {
  source("scripts/03_define_assessment.R")
}

assessment <- getOption("project_assessment")

if (is.null(assessment$settings_file) || !file.exists(assessment$settings_file)) {
  source("scripts/03_define_assessment.R")
  assessment <- getOption("project_assessment")
}

assessment <- readRDS(assessment$settings_file)
options(project_assessment = assessment)

if (is.null(assessment$indicator)) {
  stop("The assessment settings object does not contain indicator settings. Run scripts/03_define_assessment.R first.")
}

indicator <- assessment$indicator

# Define paths
inputPath <- assessment$master_input_dir
outputPath <- assessment$output_dir

# Remove unnecessary data/values/functions
keep <- c(
  "assessment", "indicator", "outputPath", "inputPath", "proj",
  "repo_url", "O2satFun", "auxilliaryFile"
)

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# SETTINGS FROM scripts/03_define_assessment.R

profile_smooth_months <- indicator$validation$profile_smooth_months
seasonal_average_first <- indicator$validation$seasonal_average_first
smooth_method <- indicator$validation$smooth_method
validation_variables <- indicator$validation$validation_variables

figure_width <- indicator$validation$figure_width
figure_height <- indicator$validation$figure_height
figure_dpi <- indicator$validation$figure_dpi

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS

make_months_label <- function(months) {
  months <- sort(unique(as.integer(months)))
  paste0(sprintf("%02d", months), collapse = "_")
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) {
    return(NA_real_)
  }
  if (length(unique(x[ok])) < 2 || length(unique(y[ok])) < 2) {
    return(NA_real_)
  }
  stats::cor(x[ok], y[ok])
}

safe_lm_stats <- function(observed, smoothed) {
  ok <- is.finite(observed) & is.finite(smoothed)
  if (sum(ok) < 3 || length(unique(observed[ok])) < 2) {
    return(list(intercept = NA_real_, slope = NA_real_, p_value = NA_real_))
  }
  fit <- stats::lm(smoothed[ok] ~ observed[ok])
  fit_sum <- summary(fit)
  list(
    intercept = stats::coef(fit)[1],
    slope = stats::coef(fit)[2],
    p_value = fit_sum$coefficients[2, 4]
  )
}

#-------------------------------------------------------------------------------
# READ SMOOTHED PROFILE DATA

months_label <- make_months_label(profile_smooth_months)
mode_label <- if (seasonal_average_first) "seasonal" else "monthly"
method_label <- tolower(smooth_method)

smoothed_profiles_file <- file.path(
  outputPath,
  paste0(
    "monthly_mean_profiles_1m_",
    mode_label,
    "_",
    method_label,
    "_smoothed_months_",
    months_label,
    ".csv"
  )
)

if (!file.exists(smoothed_profiles_file)) {
  stop(
    "Missing input file: ", smoothed_profiles_file, "\n",
    "Run script 08_1 first, or check that the settings in scripts/03_define_assessment.R match the available smoothed-profile file."
  )
}

smoothed_profiles <- data.table::fread(smoothed_profiles_file)
data.table::setDT(smoothed_profiles)

required_base_cols <- c("Year", "Month", "Date", "time_value", "Depth_m")
required_value_cols <- as.vector(rbind(
  validation_variables,
  paste0(validation_variables, "_smoothed")
))
required_cols <- c(required_base_cols, required_value_cols)

if (!all(required_cols %in% names(smoothed_profiles))) {
  stop(
    "Missing required columns in smoothed profile data: ",
    paste(setdiff(required_cols, names(smoothed_profiles)), collapse = ", ")
  )
}

smoothed_profiles[, Year := as.integer(Year)]
smoothed_profiles[, Depth_m := as.numeric(Depth_m)]
smoothed_profiles[, Date := as.Date(Date)]
smoothed_profiles[, time_value := as.numeric(time_value)]

#-------------------------------------------------------------------------------
# CREATE VALIDATION TABLE: OBSERVED AND MATCHING SMOOTHED VALUES

validation_table <- data.table::rbindlist(
  lapply(validation_variables, function(v) {
    smoothed_col <- paste0(v, "_smoothed")
    residual_col <- paste0(v, "_minus_smoothed")
    distance_col <- paste0(v, "_nearest_scaled_distance")
    available_cols <- c(required_base_cols, v, smoothed_col)
    if (residual_col %in% names(smoothed_profiles)) {
      available_cols <- c(available_cols, residual_col)
    }
    if (distance_col %in% names(smoothed_profiles)) {
      available_cols <- c(available_cols, distance_col)
    }
    x <- smoothed_profiles[, ..available_cols]
    x <- x[!is.na(get(v)) & !is.na(get(smoothed_col))]
    if (nrow(x) == 0) {
      return(NULL)
    }
    out <- data.table::data.table(
      variable = v,
      Year = x$Year,
      Month = x$Month,
      Date = x$Date,
      time_value = x$time_value,
      Depth_m = x$Depth_m,
      observed = x[[v]],
      smoothed = x[[smoothed_col]]
    )
    out[, residual_observed_minus_smoothed := observed - smoothed]
    out[, residual_smoothed_minus_observed := smoothed - observed]
    out[, absolute_error := abs(smoothed - observed)]
    out[, squared_error := (smoothed - observed)^2]
    if (residual_col %in% names(x)) {
      out[, residual_from_script_08_2 := x[[residual_col]]]
    }
    if (distance_col %in% names(x)) {
      out[, nearest_scaled_distance := x[[distance_col]]]
    }
    out
  }),
  use.names = TRUE,
  fill = TRUE
)

if (nrow(validation_table) == 0) {
  stop("No matching observed and smoothed values were found for validation.")
}

validation_table_file <- file.path(
  outputPath,
  paste0("model_validation_matching_points_", mode_label, "_", method_label, "_months_", months_label, ".csv")
)

data.table::fwrite(validation_table, validation_table_file)
message("Validation matching-point table saved: ", validation_table_file)

#-------------------------------------------------------------------------------
# CALCULATE VALIDATION METRICS

validation_metrics <- validation_table[
  ,
  {
    err <- smoothed - observed
    
    ok <- is.finite(observed) & is.finite(smoothed)
    
    centered_err <- (smoothed[ok] - mean(smoothed[ok], na.rm = TRUE)) -
      (observed[ok] - mean(observed[ok], na.rm = TRUE))
    
    crmse <- if (sum(ok) < 1) {
      NA_real_
    } else {
      sqrt(mean(centered_err^2, na.rm = TRUE))
    }
    
    cor_value <- safe_cor(observed, smoothed)
    lm_stats <- safe_lm_stats(observed, smoothed)
    .(
      n = .N,
      observed_min = min(observed, na.rm = TRUE),
      observed_max = max(observed, na.rm = TRUE),
      smoothed_min = min(smoothed, na.rm = TRUE),
      smoothed_max = max(smoothed, na.rm = TRUE),
      mean_observed = mean(observed, na.rm = TRUE),
      mean_smoothed = mean(smoothed, na.rm = TRUE),
      bias_smoothed_minus_observed = mean(err, na.rm = TRUE),
      mean_residual_observed_minus_smoothed = mean(observed - smoothed, na.rm = TRUE),
      median_error_smoothed_minus_observed = stats::median(err, na.rm = TRUE),
      mae = mean(abs(err), na.rm = TRUE),
      rmse = sqrt(mean(err^2, na.rm = TRUE)),
      centered_rmse = crmse,
      residual_sd = stats::sd(observed - smoothed, na.rm = TRUE),
      correlation = cor_value,
      r2_correlation = cor_value^2,
      lm_intercept_smoothed_vs_observed = lm_stats$intercept,
      lm_slope_smoothed_vs_observed = lm_stats$slope,
      lm_slope_p_value = lm_stats$p_value
    )
  },
  by = variable
]

validation_metrics_file <- file.path(
  outputPath,
  paste0("model_validation_metrics_", mode_label, "_", method_label, "_months_", months_label, ".csv")
)

data.table::fwrite(validation_metrics, validation_metrics_file)
message("Validation metrics saved: ", validation_metrics_file)

#-------------------------------------------------------------------------------
# YEARLY VALIDATION METRICS

yearly_validation_metrics <- validation_table[
  ,
  {
    err <- smoothed - observed
    
    ok <- is.finite(observed) & is.finite(smoothed)
    
    centered_err <- if (sum(ok) > 0) {
      (smoothed[ok] - mean(smoothed[ok], na.rm = TRUE)) -
        (observed[ok] - mean(observed[ok], na.rm = TRUE))
    } else {
      NA_real_
    }
    
    .(
      n = .N,
      bias_smoothed_minus_observed = mean(err, na.rm = TRUE),
      mae = mean(abs(err), na.rm = TRUE),
      rmse = sqrt(mean(err^2, na.rm = TRUE)),
      centered_rmse = if (sum(ok) > 0) sqrt(mean(centered_err^2, na.rm = TRUE)) else NA_real_,
      correlation = if (sum(ok) >= 3 &&
                        data.table::uniqueN(observed[ok]) >= 2 &&
                        data.table::uniqueN(smoothed[ok]) >= 2) {
        stats::cor(observed[ok], smoothed[ok])
      } else {
        NA_real_
      },
      mean_nearest_scaled_distance = if ("nearest_scaled_distance" %in% names(.SD)) {
        mean(nearest_scaled_distance, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_nearest_scaled_distance = if ("nearest_scaled_distance" %in% names(.SD)) {
        max(nearest_scaled_distance, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  },
  by = .(variable, Year)
]

yearly_validation_metrics[
  ,
  r2_correlation := correlation^2
]

yearly_validation_metrics_file <- file.path(
  outputPath,
  paste0("model_validation_metrics_by_year_", mode_label, "_", method_label, "_months_", months_label, ".csv")
)

data.table::fwrite(yearly_validation_metrics, yearly_validation_metrics_file)
message("Yearly validation metrics saved: ", yearly_validation_metrics_file)


#-------------------------------------------------------------------------------
# FIGURE 1: OBSERVED VS SMOOTHED VALUES

metrics_for_labels <- data.table::copy(validation_metrics)
metrics_for_labels[, label := paste0(
  "n = ", n,
  "\nRMSE = ", round(rmse, 3),
  "\nMAE = ", round(mae, 3),
  "\ncRMSE = ", round(centered_rmse, 3),
  "\nR2 = ", round(r2_correlation, 3),
  "\nBias = ", round(bias_smoothed_minus_observed, 3)
)]

p_scatter <- ggplot2::ggplot(
  validation_table,
  ggplot2::aes(x = observed, y = smoothed)
) +
  ggplot2::geom_point(alpha = 0.45, size = 0.8) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.8) +
  ggplot2::geom_text(
    data = metrics_for_labels,
    ggplot2::aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.1,
    size = 3
  ) +
  ggplot2::facet_wrap(~ variable, scales = "free") +
  ggplot2::labs(
    x = "Input value",
    y = "Smoothed value",
    title = paste0("Model validation: observed vs smoothed values, ", mode_label, ", months ", months_label)
  ) +
  ggplot2::theme_bw()

scatter_file <- file.path(
  outputPath,
  paste0("FIG_model_validation_observed_vs_smoothed_", mode_label, "_", method_label, "_months_", months_label, ".jpg")
)

ggplot2::ggsave(
  filename = scatter_file,
  plot = p_scatter,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

message("Validation scatter figure saved: ", scatter_file)

#-------------------------------------------------------------------------------
# FIGURE 2: RESIDUALS THROUGH TIME AND DEPTH

p_residual_field <- ggplot2::ggplot(
  validation_table,
  ggplot2::aes(x = time_value, y = Depth_m, fill = residual_observed_minus_smoothed)
) +
  ggplot2::geom_tile() +
  ggplot2::scale_y_reverse() +
  ggplot2::scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey90"
  ) +
  ggplot2::facet_wrap(~ variable, scales = "free") +
  ggplot2::labs(
    x = "Time",
    y = "Depth (m)",
    fill = "Observed - smoothed",
    title = paste0("Model validation residuals, ", mode_label, ", months ", months_label)
  ) +
  ggplot2::theme_bw()

residual_field_file <- file.path(
  outputPath,
  paste0("FIG_model_validation_residuals_time_depth_", mode_label, "_", method_label, "_months_", months_label, ".jpg")
)

ggplot2::ggsave(
  filename = residual_field_file,
  plot = p_residual_field,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

message("Validation residual time-depth figure saved: ", residual_field_file)

#-------------------------------------------------------------------------------
# FIGURE 3: RESIDUAL HISTOGRAMS

p_hist <- ggplot2::ggplot(
  validation_table,
  ggplot2::aes(x = residual_observed_minus_smoothed)
) +
  ggplot2::geom_histogram(bins = 40) +
  ggplot2::geom_vline(xintercept = 0, color = "red", linewidth = 0.7) +
  ggplot2::facet_wrap(~ variable, scales = "free") +
  ggplot2::labs(
    x = "Observed - smoothed",
    y = "Number of matching points",
    title = paste0("Model validation residual distributions, ", mode_label, ", months ", months_label)
  ) +
  ggplot2::theme_bw()

hist_file <- file.path(
  outputPath,
  paste0("FIG_model_validation_residual_histograms_", mode_label, "_", method_label, "_months_", months_label, ".jpg")
)

ggplot2::ggsave(
  filename = hist_file,
  plot = p_hist,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi
)

message("Validation residual histogram figure saved: ", hist_file)

#-------------------------------------------------------------------------------
# PRINT METRICS TO CONSOLE

print(validation_metrics)

message("Model validation completed.")
