# scripts/03_define_assessment.R
# This script defines the assessment period, creates matching folders, and writes
# one central settings object used by scripts 08_0 onwards.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself.

if (!isTRUE(getOption("project_header_done"))) source("scripts/01_header.R")
# This loads setup.R + header.R (packages + Utils) if it has not been run yet.

#-------------------------------------------------------------------------------
# USER-DEFINED ASSESSMENT SETTINGS
#
# Edit values in this section only. Scripts 08_0 onwards read these settings from
# the assessment object/RDS written by this script.

# Assessment identity and period.
topic <- "SWOI_GOR"
basin <- "Opensea Gulf of Riga"
start_year <- 1900
end_year <- 2025

# Input/output roots.
input_root <- "Input"
output_root <- "Output"

# Main indicator season. This month vector is reused by scripts 08_0-08_4 so the
# season is consistent across monthly plots, smoothing, deep-value selection, and
# EQRS calculations.
indicator_months <- 7:8

# Near-bottom rule used in script 08_0. This is distance from the deepest sample
# in a profile to the bathymetry depth at that station.
bottom_depth_limit_m <- 4

# Script 08_0 averaging and figure settings.
monthly_average_parameters <- c(
  "Oxygen_mll",
  "Oxygen_mgl",
  "Oxygen_debt_mgl",
  "Oxygen_debt_mgl_H2S",
  "Oxygen_debt_mgl_NH4",
  "Oxygen_debt_mgl_H2S_NH4",
  "Temperature_degreesC",
  "Salinity_psu",
  "Density_kgm3",
  "Sigma0_kgm3",
  "Hydrogen_Sulphide_umoll",
  "Ammonium_Nitrogen_umoll"
)
min_oxygen_measurements_per_profile <- 3

monthly_profile_figure_width <- 14
monthly_profile_figure_height <- 8
monthly_profile_figure_dpi <- 300
monthly_profile_figure_max_depth <- NULL
monthly_profile_colour_palette <- "jet"
monthly_profile_salinity_colour_palette <- "jet"
monthly_profile_salinity_colour_limits <- c(4, 7)
monthly_bottom_figure_width <- 12
monthly_bottom_figure_height <- 8
monthly_bottom_figure_dpi <- 300

# Deep-value rule used in script 08_3 and script 08_4. This is an absolute
# minimum measured depth for profile-deepest selected values, not distance from
# the seabed. It is kept separate from bottom_depth_limit_m even when both are 4.
deep_profile_min_depth_m <- 40

# Smoothing settings used by scripts 08_1 and 08_2.
seasonal_average_first <- TRUE
smooth_method <- "gam"
smooth_variables <- c(
  "Oxygen_mgl",
  "Oxygen_debt_mgl_H2S_NH4",
  "Temperature_degreesC",
  "Salinity_psu"
)
max_depth_for_smoothing <- NULL

gam_k_time <- 12
gam_k_depth <- 10
gam_method <- "REML"
min_observations_for_gam <- 20

apply_distance_mask <- TRUE
max_time_gap_years <- 5
max_depth_gap_m <- 10
max_scaled_distance <- 1

profile_figure_width <- 14
profile_figure_height <- 10
profile_figure_dpi <- 300
profile_colour_palette <- "jet" # Options used by script 08_1: parula, jet, viridis, heat.

# Model-validation settings used by script 08_2.
validation_variables <- smooth_variables
validation_figure_width <- 12
validation_figure_height <- 8
validation_figure_dpi <- 300

# Deep-profile and selected deep-value settings used by script 08_3.
profile_variable_observed <- "Oxygen_debt_mgl_H2S_NH4"
profile_variable_label <- "DO deficiency, H2S (mg/l)"

deepest_variables <- c(
  Oxygen_debt_mgl_H2S_NH4 = "DO deficiency, H2S",
  Oxygen_mgl = "DO",
  Temperature_degreesC = "Temperature",
  Salinity_psu = "Salinity"
)

deepest_units <- c(
  Oxygen_debt_mgl_H2S_NH4 = "mg/l",
  Oxygen_mgl = "mg/l",
  Temperature_degreesC = "degrees C",
  Salinity_psu = "psu"
)

deep_values_figure_width <- 16
deep_values_figure_height <- 9
deep_values_figure_dpi <- 300
deep_values_4panel_figure_width <- 12
deep_values_4panel_figure_height <- 8

deep_values_plot_start_year <- 1980
deep_values_percentile_baseline_start_year <- 1980
deep_values_percentile_baseline_end_year <- 2010
deep_values_percentile_probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# EQRS settings used by script 08_4.
best_baseline_start_year <- 1980
best_baseline_end_year <- 2010
ACCDEV <- 25
final_result_year <- NA_integer_
cap_eqrs_to_0_1 <- TRUE
use_continuous_upper_eqrs_class <- TRUE

eqrs_plot_start_year <- 1980
eqrs_figure_width <- 9
eqrs_figure_height <- 7
eqrs_figure_dpi <- 300
eqrs_period_averages <- data.table::data.table(
  period = c("2011-2016", "2016-2021"),
  start_year = c(2011L, 2016L),
  end_year = c(2016L, 2021L)
)

eqrs_parameters <- data.table::data.table(
  parameter = c("Oxygen", "Oxygen deficiency"),
  measured_column = c("Oxygen_mgl", "DO_debt_measured_mgl"),
  units = c("mg/l", "mg/l"),
  response = c(">=", "<="),
  best_percentile_probability = c(0.90, 0.10),
  best_percentile_label = c("p90", "p10")
)

#-------------------------------------------------------------------------------
# DERIVED SETTINGS AND VALIDATION

validate_months <- function(months, object_name = "indicator_months") {
  months <- sort(unique(as.integer(months)))
  months <- months[!is.na(months)]

  if (length(months) == 0) {
    stop(object_name, " must contain at least one month number.")
  }

  if (any(months < 1 | months > 12)) {
    stop(object_name, " must contain month numbers from 1 to 12 only.")
  }

  months
}

make_depth_limit_label <- function(x) {
  if (length(x) != 1 || is.na(x) || !is.finite(x) || x < 0) {
    stop("Depth-limit values must be one non-negative finite number.")
  }

  label <- format(x, trim = TRUE, scientific = FALSE)
  label <- sub("(\\.\\d*?)0+$", "\\1", label)
  label <- sub("\\.$", "", label)
  gsub("\\.", "p", label)
}

make_depth_limit_text <- function(x) {
  label <- format(x, trim = TRUE, scientific = FALSE)
  label <- sub("(\\.\\d*?)0+$", "\\1", label)
  label <- sub("\\.$", "", label)
  paste0(label, " m")
}

make_months_label <- function(months) {
  months <- validate_months(months)
  paste(sprintf("%02d", months), collapse = "_")
}

make_months_text <- function(months) {
  months <- validate_months(months)
  month_names <- month.name[months]

  if (length(months) == 1) {
    return(month_names)
  }

  if (all(diff(months) == 1)) {
    return(paste0(month_names[1], "-", month_names[length(month_names)]))
  }

  paste(month_names, collapse = ", ")
}

indicator_months <- validate_months(indicator_months)
months_label <- make_months_label(indicator_months)
months_text <- make_months_text(indicator_months)
months_suffix <- paste0("months_", months_label)

bottom_depth_limit_label <- make_depth_limit_label(bottom_depth_limit_m)
bottom_depth_limit_text <- make_depth_limit_text(bottom_depth_limit_m)
bottom_suffix <- paste0("bottom_", bottom_depth_limit_label, "m")

profile_variable_smoothed <- paste0(profile_variable_observed, "_smoothed")

panel_b_variable_labels <- c(
  DO_debt_measured_mgl = deepest_variables[["Oxygen_debt_mgl_H2S_NH4"]],
  Oxygen_mgl = deepest_variables[["Oxygen_mgl"]],
  Temperature_degreesC = deepest_variables[["Temperature_degreesC"]],
  Salinity_psu = deepest_variables[["Salinity_psu"]]
)

assessment_period <- paste0(start_year, "-", end_year)
subset_id <- paste0("years_", start_year, "_", end_year)

#-------------------------------------------------------------------------------
# FOLDERS AND SETTINGS OUTPUTS

dir.create(input_root, showWarnings = FALSE, recursive = TRUE)
dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

input_subset_dir <- file.path(input_root, subset_id)
output_subset_dir <- file.path(output_root, subset_id)
master_input_dir <- file.path(input_root, "master")

dir.create(input_subset_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_subset_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(master_input_dir, showWarnings = FALSE, recursive = TRUE)

assessment_settings_file <- file.path(output_subset_dir, "assessment_settings.rds")
assessment_settings_summary_file <- file.path(output_subset_dir, "assessment_settings_summary.csv")

indicator <- list(
  profile_months = indicator_months,
  profile_months_label = months_label,
  profile_months_text = months_text,
  profile_months_suffix = months_suffix,

  near_bottom = list(
    bottom_depth_limit_m = bottom_depth_limit_m,
    bottom_depth_limit_label = bottom_depth_limit_label,
    bottom_depth_limit_text = bottom_depth_limit_text,
    bottom_suffix = bottom_suffix,
    bottom_flag_col = "deepest_within_bottom_limit_bathy",
    monthly_bottom_means_filename = paste0("monthly_bottom_means_", bottom_suffix, ".csv"),
    monthly_bottom_oxygen_figure_filename = paste0(
      "FIG_Oxygen_mgl_",
      bottom_suffix,
      "_",
      months_suffix,
      ".jpg"
    ),
    monthly_average_parameters = monthly_average_parameters,
    min_oxygen_measurements_per_profile = min_oxygen_measurements_per_profile,
    monthly_profile_figure_width = monthly_profile_figure_width,
    monthly_profile_figure_height = monthly_profile_figure_height,
    monthly_profile_figure_dpi = monthly_profile_figure_dpi,
    monthly_profile_figure_max_depth = monthly_profile_figure_max_depth,
    monthly_profile_colour_palette = monthly_profile_colour_palette,
    monthly_profile_salinity_colour_palette = monthly_profile_salinity_colour_palette,
    monthly_profile_salinity_colour_limits = monthly_profile_salinity_colour_limits,
    monthly_bottom_figure_width = monthly_bottom_figure_width,
    monthly_bottom_figure_height = monthly_bottom_figure_height,
    monthly_bottom_figure_dpi = monthly_bottom_figure_dpi
  ),

  smoothing = list(
    profile_smooth_months = indicator_months,
    seasonal_average_first = seasonal_average_first,
    smooth_variables = smooth_variables,
    max_depth_for_smoothing = max_depth_for_smoothing,
    smooth_method = smooth_method,
    gam_k_time = gam_k_time,
    gam_k_depth = gam_k_depth,
    gam_method = gam_method,
    min_observations_for_gam = min_observations_for_gam,
    apply_distance_mask = apply_distance_mask,
    max_time_gap_years = max_time_gap_years,
    max_depth_gap_m = max_depth_gap_m,
    max_scaled_distance = max_scaled_distance,
    figure_width = profile_figure_width,
    figure_height = profile_figure_height,
    figure_dpi = profile_figure_dpi,
    profile_colour_palette = profile_colour_palette
  ),

  validation = list(
    profile_smooth_months = indicator_months,
    seasonal_average_first = seasonal_average_first,
    smooth_method = smooth_method,
    validation_variables = validation_variables,
    figure_width = validation_figure_width,
    figure_height = validation_figure_height,
    figure_dpi = validation_figure_dpi
  ),

  deep_values = list(
    profile_months = indicator_months,
    seasonal_average_first = seasonal_average_first,
    smooth_method = smooth_method,
    profile_variable_observed = profile_variable_observed,
    profile_variable_smoothed = profile_variable_smoothed,
    profile_variable_label = profile_variable_label,
    deepest_variables = deepest_variables,
    panel_b_variable_labels = panel_b_variable_labels,
    deepest_units = deepest_units,
    deepest_min_depth_m = deep_profile_min_depth_m,
    figure_width = deep_values_figure_width,
    figure_height = deep_values_figure_height,
    figure_dpi = deep_values_figure_dpi,
    deep_values_4panel_figure_width = deep_values_4panel_figure_width,
    deep_values_4panel_figure_height = deep_values_4panel_figure_height,
    deep_values_plot_start_year = deep_values_plot_start_year,
    deep_values_percentile_baseline_start_year = deep_values_percentile_baseline_start_year,
    deep_values_percentile_baseline_end_year = deep_values_percentile_baseline_end_year,
    deep_values_percentile_probs = deep_values_percentile_probs
  ),

  eqrs = list(
    profile_months = indicator_months,
    seasonal_average_first = seasonal_average_first,
    smooth_method = smooth_method,
    deepest_min_depth_m = deep_profile_min_depth_m,
    best_baseline_start_year = best_baseline_start_year,
    best_baseline_end_year = best_baseline_end_year,
    ACCDEV = ACCDEV,
    final_result_year = final_result_year,
    cap_eqrs_to_0_1 = cap_eqrs_to_0_1,
    use_continuous_upper_eqrs_class = use_continuous_upper_eqrs_class,
    eqrs_plot_start_year = eqrs_plot_start_year,
    figure_width = eqrs_figure_width,
    figure_height = eqrs_figure_height,
    figure_dpi = eqrs_figure_dpi,
    period_averages = eqrs_period_averages,
    parameters = eqrs_parameters
  )
)

assessment <- list(
  topic = topic,
  basin = basin,
  start_year = start_year,
  end_year = end_year,
  period_label = assessment_period,
  subset_id = subset_id,
  input_root = input_root,
  output_root = output_root,
  master_input_dir = master_input_dir,
  input_dir = input_subset_dir,
  output_dir = output_subset_dir,
  settings_file = assessment_settings_file,
  settings_summary_file = assessment_settings_summary_file,
  indicator = indicator
)

options(project_assessment = assessment)

settings_summary <- data.table::rbindlist(list(
  data.table::data.table(section = "assessment", setting = "topic", value = topic),
  data.table::data.table(section = "assessment", setting = "basin", value = basin),
  data.table::data.table(section = "assessment", setting = "period", value = assessment_period),
  data.table::data.table(section = "paths", setting = "master_input_dir", value = master_input_dir),
  data.table::data.table(section = "paths", setting = "input_dir", value = input_subset_dir),
  data.table::data.table(section = "paths", setting = "output_dir", value = output_subset_dir),
  data.table::data.table(section = "indicator", setting = "months", value = paste(indicator_months, collapse = ",")),
  data.table::data.table(section = "indicator", setting = "months_text", value = months_text),
  data.table::data.table(section = "near_bottom", setting = "bottom_depth_limit_m", value = as.character(bottom_depth_limit_m)),
  data.table::data.table(section = "near_bottom", setting = "monthly_average_parameters", value = paste(monthly_average_parameters, collapse = ",")),
  data.table::data.table(section = "near_bottom", setting = "min_oxygen_measurements_per_profile", value = as.character(min_oxygen_measurements_per_profile)),
  data.table::data.table(section = "deep_values", setting = "deep_profile_min_depth_m", value = as.character(deep_profile_min_depth_m)),
  data.table::data.table(section = "smoothing", setting = "seasonal_average_first", value = as.character(seasonal_average_first)),
  data.table::data.table(section = "smoothing", setting = "smooth_method", value = smooth_method),
  data.table::data.table(section = "smoothing", setting = "smooth_variables", value = paste(smooth_variables, collapse = ",")),
  data.table::data.table(section = "eqrs", setting = "best_baseline", value = paste0(best_baseline_start_year, "-", best_baseline_end_year)),
  data.table::data.table(section = "eqrs", setting = "ACCDEV", value = as.character(ACCDEV)),
  data.table::data.table(section = "output", setting = "assessment_settings_file", value = assessment_settings_file),
  data.table::data.table(section = "output", setting = "assessment_settings_summary_file", value = assessment_settings_summary_file)
), use.names = TRUE)

data.table::fwrite(settings_summary, assessment_settings_summary_file)
saveRDS(assessment, assessment_settings_file)

message("Assessment defined: ", assessment$topic, " (", assessment$basin, "), period ", assessment$period_label)
message("Master input:  ", assessment$master_input_dir)
message("Input folder:  ", assessment$input_dir)
message("Output folder: ", assessment$output_dir)
message("Settings RDS:   ", assessment$settings_file)
message("Settings table: ", assessment$settings_summary_file)
