# scripts/08_3_yearly_DO_debt_profiles_and_deep_profile_deepest_seasonal_averages_separate_wide_tables.R
#
# Run this after script 08_2.
#
# Purpose:
#   1. Put all yearly July-September DO debt profiles on one panel.
#      x = DO debt, y = depth, colour/Z = year.
#   2. Add a second panel with one July-September seasonal average per year
#      calculated from profile-deepest measured values. The script first finds
#      the deepest valid DO-debt record per profile, keeps only profiles whose
#      deepest selected record is >= 40 m, then averages those values by year.
#      DO, temperature, and salinity correspond to those same profile-depth
#      records.
#   3. Add additional profile-deepest temperature and salinity seasonal averages
#      where no DO value exists. These are plotted in blue.
#
# Inputs:
#   - monthly_mean_profiles_1m_<seasonal/monthly>_<GAM/gam>_smoothed_months_07_08_09.csv
#     created by script 08_1 and used by script 08_2.
#   - oxy_clean.csv created by script 07_data_preparation.R.
#
# Outputs:
#   - FIG_yearly_DO_debt_profiles_and_deep_profile_deepest_seasonal_averages_extra_TS_min40m_<mode>_<method>_months_07_08_09.jpg
#   - FIG_deep_profile_deepest_selected_values_4panel_percentiles_1980_to_end_baseline_1980_2010_min40m_<mode>_<method>_months_07_08_09.jpg
#   - smoothed_profile_values_wide_DO_debt_profiles_<mode>_<method>_months_07_08_09.csv
#   - deep_measured_profile_deepest_seasonal_average_values_wide_extra_TS_min40m_<mode>_<method>_months_07_08_09.csv
#   - deep_profile_deepest_selected_values_percentiles_1980_2010_min40m_<mode>_<method>_months_07_08_09.csv
#
# The output tables keep smoothed profile values and deep measured
# profile-deepest seasonal average values separate.

options(project_clean_workspace = FALSE)

source("scripts/01_header.R")

if (is.null(getOption("project_assessment"))) {
  source("scripts/03_define_assessment.R")
}

assessment <- getOption("project_assessment")

# Define paths.
inputPath <- "Input/master"
outputPath <- assessment$output_dir

# Remove unnecessary data/values/functions.
keep <- c(
  "assessment", "outputPath", "inputPath", "proj",
  "repo_url", "O2satFun", "auxilliaryFile"
)

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# USER SETTINGS

# July-September.
profile_months <- 7:9

# These settings should match scripts 08_1 and 08_2.
seasonal_average_first <- TRUE
smooth_method <- "gam"

# DO debt variable to use for the yearly profile panel.
profile_variable_observed <- "Oxygen_debt_mgl_H2S_NH4"
profile_variable_smoothed <- paste0(profile_variable_observed, "_smoothed")
profile_variable_label <- "DO debt, H2S + NH4 (mg/l)"

# Variables for the measured deep-layer seasonal average panel.
deepest_variables <- c(
  Oxygen_debt_mgl_H2S_NH4 = "DO debt, H2S + NH4",
  Oxygen_mgl = "DO",
  Temperature_degreesC = "Temperature",
  Salinity_psu = "Salinity"
)

# Plot labels for the wide columns used in Panel B. Keep this separate from
# deepest_variables because the measured DO-debt output column is renamed to
# DO_debt_measured_mgl after the yearly averaging step.
panel_b_variable_labels <- c(
  DO_debt_measured_mgl = deepest_variables[["Oxygen_debt_mgl_H2S_NH4"]],
  Oxygen_mgl = deepest_variables[["Oxygen_mgl"]],
  Temperature_degreesC = deepest_variables[["Temperature_degreesC"]],
  Salinity_psu = deepest_variables[["Salinity_psu"]]
)

deepest_units <- c(
  Oxygen_debt_mgl_H2S_NH4 = "mg/l",
  Oxygen_mgl = "mg/l",
  Temperature_degreesC = "degrees C",
  Salinity_psu = "psu"
)

# Panel B depth rule.
# A profile contributes to Panel B only if its selected deepest measured value
# is at this depth or deeper.
deepest_min_depth_m <- 40

# Figure settings.
figure_width <- 16
figure_height <- 9
figure_dpi <- 300

# Four-panel selected deep-values figure settings.
deep_values_plot_start_year <- 1980
deep_values_percentile_baseline_start_year <- 1980
deep_values_percentile_baseline_end_year <- 2010
deep_values_percentile_probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS

make_months_label <- function(months) {
  months <- sort(unique(as.integer(months)))
  paste0(sprintf("%02d", months), collapse = "_")
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

find_first_existing_file <- function(paths) {
  paths <- unique(paths)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) {
    return(NA_character_)
  }
  hit[1]
}

check_required_cols <- function(data, required_cols, data_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ", data_name, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

#-------------------------------------------------------------------------------
# READ SMOOTHED YEARLY PROFILE DATA

profile_months <- sort(unique(as.integer(profile_months)))
months_label <- make_months_label(profile_months)
mode_label <- if (seasonal_average_first) "seasonal" else "monthly"

# Script 08_1 saves the method as GAM, while some later scripts may use gam.
# Check both to avoid filename mismatch problems.
method_label_candidates <- unique(c(
  smooth_method,
  toupper(smooth_method),
  tolower(smooth_method)
))

smoothed_profile_file_candidates <- file.path(
  outputPath,
  paste0(
    "monthly_mean_profiles_1m_",
    mode_label,
    "_",
    method_label_candidates,
    "_smoothed_months_",
    months_label,
    ".csv"
  )
)

smoothed_profiles_file <- find_first_existing_file(smoothed_profile_file_candidates)

if (is.na(smoothed_profiles_file)) {
  stop(
    "Missing smoothed profile input file. Checked:\n  ",
    paste(smoothed_profile_file_candidates, collapse = "\n  "),
    "\nRun scripts 08_1 and 08_2 first, or check the settings in this script."
  )
}

smoothed_profiles <- data.table::fread(smoothed_profiles_file)
data.table::setDT(smoothed_profiles)

check_required_cols(
  smoothed_profiles,
  c("Year", "Depth_m", profile_variable_smoothed),
  basename(smoothed_profiles_file)
)

smoothed_profiles[, Year := as.integer(Year)]
smoothed_profiles[, Depth_m := as.numeric(Depth_m)]
smoothed_profiles[, (profile_variable_smoothed) := as.numeric(get(profile_variable_smoothed))]

profile_data <- smoothed_profiles[
  !is.na(get(profile_variable_smoothed)),
  .(
    Year,
    Depth_m,
    DO_debt_mgl = get(profile_variable_smoothed)
  )
]

data.table::setorder(profile_data, Year, Depth_m)

if (nrow(profile_data) == 0) {
  stop("No non-NA smoothed DO debt profile values were found.")
}

#-------------------------------------------------------------------------------
# READ RAW PROFILE DATA FOR DEEP PROFILE-DEEPEST SEASONAL AVERAGES

# Panel B logic:
#   1. Use the original measured profile records, not monthly mean profile bins.
#   2. Within July-September, find the deepest valid measured DO-debt record in
#      each profile.
#   3. Keep only those profile-deepest records where the selected depth is
#      >= deepest_min_depth_m.
#   4. Average those profile-deepest values by year. DO, temperature, and salinity
#      are taken from the same profile-depth records as the DO-debt values.
#   5. For the blue T/S points, repeat the same profile-deepest logic separately
#      for T and S records without a matching DO value.

measured_profiles_file <- file.path(outputPath, "oxy_clean.csv")

if (!file.exists(measured_profiles_file)) {
  stop(
    "Missing input file: ", measured_profiles_file, "\n",
    "Run script 07_data_preparation.R first."
  )
}

measured_profiles <- data.table::fread(measured_profiles_file)
data.table::setDT(measured_profiles)

required_measured_cols <- c(
  "ID",
  "Year",
  "Month",
  "Day",
  "Depth_m",
  "Oxygen_mgl",
  "Temperature_degreesC",
  "Salinity_psu"
)
check_required_cols(measured_profiles, required_measured_cols, basename(measured_profiles_file))

measured_profiles[, Year := as.integer(Year)]
measured_profiles[, Month := as.integer(Month)]
measured_profiles[, Day := as.integer(Day)]
measured_profiles[, Depth_m := as.numeric(Depth_m)]

ensure_observed_do_debt_variable <- function(data, variable_name) {
  x <- data.table::copy(data)
  
  if (variable_name %in% names(x)) {
    x[, (variable_name) := as.numeric(get(variable_name))]
    return(x)
  }
  
  if (variable_name != "Oxygen_debt_mgl_H2S_NH4") {
    stop(
      "Required observed profile variable is missing: ", variable_name,
      ". This script can only auto-create Oxygen_debt_mgl_H2S_NH4."
    )
  }
  
  needed_cols <- c(
    "Oxygen_mgl",
    "Temperature_degreesC",
    "Depth_m",
    "Hydrogen_Sulphide_umoll",
    "Ammonium_Nitrogen_umoll"
  )
  check_required_cols(x, needed_cols, basename(measured_profiles_file))
  
  # Recreate the same H2S/NH4-adjusted DO-debt variable used in script 08_0.
  # Oxygen saturation concentration in mg/l.
  local_O2satFun <- function(temp) {
    tempabs <- temp + 273.15
    exp(
      -173.4292 +
        249.6339 * (100 / tempabs) +
        143.3483 * log(tempabs / 100) -
        21.8492 * (tempabs / 100) +
        (-0.033096 +
           0.014259 * (tempabs / 100) -
           0.0017000 * (tempabs / 100)^2)
    ) * 1.428
  }
  
  x[, Oxygen_mgl := as.numeric(Oxygen_mgl)]
  x[, Temperature_degreesC := as.numeric(Temperature_degreesC)]
  x[, Hydrogen_Sulphide_umoll := as.numeric(Hydrogen_Sulphide_umoll)]
  x[, Ammonium_Nitrogen_umoll := as.numeric(Ammonium_Nitrogen_umoll)]
  
  x[, negat_DO_H2S_mll := NA_real_]
  x[!is.na(Hydrogen_Sulphide_umoll),
    negat_DO_H2S_mll := Hydrogen_Sulphide_umoll * -0.04478]
  
  x[, negat_DO_H2S_mgl := NA_real_]
  x[!is.na(negat_DO_H2S_mll),
    negat_DO_H2S_mgl := negat_DO_H2S_mll * 1.428]
  
  x[Hydrogen_Sulphide_umoll <= 4, negat_DO_H2S_mgl := NA_real_]
  
  low_h2s_with_oxygen <- which(
    !is.na(x$Oxygen_mgl) &
      !is.na(x$Hydrogen_Sulphide_umoll) &
      x$Hydrogen_Sulphide_umoll <= 4
  )
  
  if (length(low_h2s_with_oxygen) > 0) {
    x$Hydrogen_Sulphide_umoll[low_h2s_with_oxygen] <- NA_real_
    x$Oxygen_mgl[low_h2s_with_oxygen] <- 0
  }
  
  x[, Oxygen_debt_mgl := local_O2satFun(Temperature_degreesC) - Oxygen_mgl]
  
  x[, Ammonium_Nitrogen_mgl := NA_real_]
  x[!is.na(Ammonium_Nitrogen_umoll),
    Ammonium_Nitrogen_mgl := Ammonium_Nitrogen_umoll * (14.0067 / 1000)]
  
  x[, negat_DO_NH4_mgl := NA_real_]
  x[!is.na(Ammonium_Nitrogen_mgl),
    negat_DO_NH4_mgl := Ammonium_Nitrogen_mgl * -4.57]
  x[Depth_m < 65, negat_DO_NH4_mgl := NA_real_]
  
  x[, Oxygen_debt_mgl_H2S := Oxygen_debt_mgl]
  x[!is.na(negat_DO_H2S_mgl),
    Oxygen_debt_mgl_H2S := Oxygen_debt_mgl + negat_DO_H2S_mgl * -1]
  
  x[, Oxygen_debt_mgl_NH4 := Oxygen_debt_mgl]
  x[!is.na(negat_DO_NH4_mgl),
    Oxygen_debt_mgl_NH4 := Oxygen_debt_mgl + negat_DO_NH4_mgl * -1]
  
  x[, Oxygen_debt_mgl_H2S_NH4 := Oxygen_debt_mgl_H2S]
  x[!is.na(negat_DO_NH4_mgl),
    Oxygen_debt_mgl_H2S_NH4 := Oxygen_debt_mgl_H2S_NH4 + negat_DO_NH4_mgl * -1]
  
  x[, (variable_name) := as.numeric(get(variable_name))]
  x
}

measured_profiles <- ensure_observed_do_debt_variable(
  measured_profiles,
  profile_variable_observed
)

for (v in names(deepest_variables)) {
  measured_profiles[, (v) := as.numeric(get(v))]
}

measured_season <- measured_profiles[
  Month %in% profile_months &
    !is.na(Depth_m)
]

if (nrow(measured_season) == 0) {
  stop(
    "No measured profile rows found in oxy_clean.csv for selected months ",
    months_label,
    "."
  )
}

# Black points: deepest valid DO-debt record per profile, then yearly average.
corresponding_source <- measured_season[
  !is.na(get(profile_variable_observed))
]

if (nrow(corresponding_source) == 0) {
  stop(
    "No non-NA DO debt observations found for selected months ",
    months_label,
    "."
  )
}

profile_deepest_corresponding_rows <- corresponding_source[
  ,
  .SD[Depth_m == max(Depth_m, na.rm = TRUE)],
  by = ID
]

# If there are several records at the same deepest depth within a profile,
# average them first so each profile contributes only one value.
profile_deepest_corresponding <- profile_deepest_corresponding_rows[
  ,
  .(
    Year = first(Year),
    Month = first(Month),
    Day = first(Day),
    Depth_m = first(Depth_m),
    n_records_at_profile_deepest_DO_debt = .N,
    DO_debt_measured_mgl = safe_mean(get(profile_variable_observed)),
    Oxygen_mgl = safe_mean(Oxygen_mgl),
    Temperature_degreesC = safe_mean(Temperature_degreesC),
    Salinity_psu = safe_mean(Salinity_psu)
  ),
  by = ID
]

profile_deepest_corresponding <- profile_deepest_corresponding[
  Depth_m >= deepest_min_depth_m
]

if (nrow(profile_deepest_corresponding) == 0) {
  stop(
    "No profile-deepest DO debt observations were measured at depths >= ",
    deepest_min_depth_m,
    " m for selected months ",
    months_label,
    "."
  )
}

corresponding_yearly <- profile_deepest_corresponding[
  ,
  .(
    mean_depth_m_corresponding = safe_mean(Depth_m),
    min_depth_m_corresponding = min(Depth_m, na.rm = TRUE),
    max_depth_m_corresponding = max(Depth_m, na.rm = TRUE),
    n_profiles_DO_debt = data.table::uniqueN(ID),
    n_records_at_profile_deepest_DO_debt = sum(n_records_at_profile_deepest_DO_debt),
    n_profiles_Oxygen_mgl = sum(!is.na(Oxygen_mgl)),
    n_profiles_Temperature_degreesC = sum(!is.na(Temperature_degreesC)),
    n_profiles_Salinity_psu = sum(!is.na(Salinity_psu)),
    DO_debt_measured_mgl = safe_mean(DO_debt_measured_mgl),
    Oxygen_mgl = safe_mean(Oxygen_mgl),
    Temperature_degreesC = safe_mean(Temperature_degreesC),
    Salinity_psu = safe_mean(Salinity_psu)
  ),
  by = Year
]

data.table::setorder(corresponding_yearly, Year)

make_extra_profile_deepest_without_DO_yearly <- function(data,
                                                         variable_col,
                                                         value_col,
                                                         depth_prefix,
                                                         n_profile_col,
                                                         n_record_col) {
  make_empty_extra <- function() {
    out <- data.table::data.table(Year = integer())
    out[, (paste0("mean_depth_m_", depth_prefix)) := numeric()]
    out[, (paste0("min_depth_m_", depth_prefix)) := numeric()]
    out[, (paste0("max_depth_m_", depth_prefix)) := numeric()]
    out[, (n_profile_col) := integer()]
    out[, (n_record_col) := integer()]
    out[, (value_col) := numeric()]
    out
  }
  
  source <- data[
    is.na(Oxygen_mgl) &
      !is.na(get(variable_col))
  ]
  
  if (nrow(source) == 0) {
    return(make_empty_extra())
  }
  
  deepest_rows <- source[
    ,
    .SD[Depth_m == max(Depth_m, na.rm = TRUE)],
    by = ID
  ]
  
  by_profile <- deepest_rows[
    ,
    .(
      Year = first(Year),
      Depth_m = first(Depth_m),
      n_records_at_profile_deepest = .N,
      value = safe_mean(get(variable_col))
    ),
    by = ID
  ]
  
  by_profile <- by_profile[Depth_m >= deepest_min_depth_m]
  
  if (nrow(by_profile) == 0) {
    return(make_empty_extra())
  }
  
  out <- by_profile[
    ,
    .(
      mean_depth = safe_mean(Depth_m),
      min_depth = min(Depth_m, na.rm = TRUE),
      max_depth = max(Depth_m, na.rm = TRUE),
      n_profiles = data.table::uniqueN(ID),
      n_records_at_profile_deepest = sum(n_records_at_profile_deepest),
      value = safe_mean(value)
    ),
    by = Year
  ]
  
  data.table::setnames(
    out,
    old = c(
      "mean_depth",
      "min_depth",
      "max_depth",
      "n_profiles",
      "n_records_at_profile_deepest",
      "value"
    ),
    new = c(
      paste0("mean_depth_m_", depth_prefix),
      paste0("min_depth_m_", depth_prefix),
      paste0("max_depth_m_", depth_prefix),
      n_profile_col,
      n_record_col,
      value_col
    )
  )
  
  data.table::setorder(out, Year)
  out
}

# Blue points: deepest profile-level T and S records without matching DO,
# averaged separately to one value per year.
extra_temperature_yearly <- make_extra_profile_deepest_without_DO_yearly(
  data = measured_season,
  variable_col = "Temperature_degreesC",
  value_col = "Temperature_degreesC_extra_without_DO",
  depth_prefix = "extra_T_without_DO",
  n_profile_col = "n_profiles_extra_Temperature_degreesC_without_DO",
  n_record_col = "n_records_at_profile_deepest_extra_Temperature_degreesC_without_DO"
)

extra_salinity_yearly <- make_extra_profile_deepest_without_DO_yearly(
  data = measured_season,
  variable_col = "Salinity_psu",
  value_col = "Salinity_psu_extra_without_DO",
  depth_prefix = "extra_S_without_DO",
  n_profile_col = "n_profiles_extra_Salinity_psu_without_DO",
  n_record_col = "n_records_at_profile_deepest_extra_Salinity_psu_without_DO"
)

extra_ts_yearly <- merge(
  extra_temperature_yearly,
  extra_salinity_yearly,
  by = "Year",
  all = TRUE
)

data.table::setDT(extra_ts_yearly)
data.table::setorder(extra_ts_yearly, Year)

# One wide row per year for the measured Panel B values. Extra T/S values are
# kept as separate columns so the table still has one row per year.
deep_measured_seasonal_values_wide <- merge(
  corresponding_yearly,
  extra_ts_yearly,
  by = "Year",
  all = TRUE
)

data.table::setDT(deep_measured_seasonal_values_wide)
data.table::setorder(deep_measured_seasonal_values_wide, Year)

# Long plotting tables are derived from the single wide table.
corresponding_long <- data.table::melt(
  deep_measured_seasonal_values_wide,
  id.vars = c(
    "Year",
    "mean_depth_m_corresponding",
    "min_depth_m_corresponding",
    "max_depth_m_corresponding"
  ),
  measure.vars = c(
    "DO_debt_measured_mgl",
    "Oxygen_mgl",
    "Temperature_degreesC",
    "Salinity_psu"
  ),
  variable.name = "variable",
  value.name = "value"
)

corresponding_long <- corresponding_long[!is.na(value)]
corresponding_long[, parameter := unname(panel_b_variable_labels[as.character(variable)])]
corresponding_long <- corresponding_long[!is.na(parameter)]
corresponding_long[, value_source := "Profile-deepest seasonal average corresponding to DO debt"]
corresponding_long[, parameter := factor(parameter, levels = unname(panel_b_variable_labels))]
data.table::setorder(corresponding_long, parameter, Year)

extra_temperature_long <- deep_measured_seasonal_values_wide[
  !is.na(Temperature_degreesC_extra_without_DO),
  .(
    Year,
    variable = "Temperature_degreesC_extra_without_DO",
    value = Temperature_degreesC_extra_without_DO,
    parameter = deepest_variables[["Temperature_degreesC"]],
    value_source = "Additional profile-deepest seasonal average T/S without DO"
  )
]

extra_salinity_long <- deep_measured_seasonal_values_wide[
  !is.na(Salinity_psu_extra_without_DO),
  .(
    Year,
    variable = "Salinity_psu_extra_without_DO",
    value = Salinity_psu_extra_without_DO,
    parameter = deepest_variables[["Salinity_psu"]],
    value_source = "Additional profile-deepest seasonal average T/S without DO"
  )
]

extra_ts_long <- data.table::rbindlist(
  list(extra_temperature_long, extra_salinity_long),
  use.names = TRUE,
  fill = TRUE
)

extra_ts_long[, parameter := factor(parameter, levels = unname(panel_b_variable_labels))]
extra_ts_long <- extra_ts_long[!is.na(parameter)]
data.table::setorder(extra_ts_long, parameter, Year)

if (nrow(corresponding_long) == 0) {
  stop("No deep measured profile-deepest seasonal average values could be calculated for Panel B.")
}

#-------------------------------------------------------------------------------
# SEPARATE WIDE OUTPUT TABLES FOR VALUES USED IN THE FIGURE

# Table 1: Panel A values only. These are smoothed yearly DO debt profile lines.
smoothed_profile_values_wide <- profile_data[
  ,
  .(
    figure_panel = "A. Yearly July-September DO debt profiles",
    value_source = "Smoothed seasonal DO debt profile",
    Year,
    Depth_m,
    months_included = months_label,
    profile_smooth_method = tolower(smooth_method),
    seasonal_average_first = seasonal_average_first,
    DO_debt_profile_smoothed_mgl = DO_debt_mgl
  )
]

data.table::setorder(smoothed_profile_values_wide, Year, Depth_m)

smoothed_profile_values_wide_output_file <- file.path(
  outputPath,
  paste0(
    "smoothed_profile_values_wide_DO_debt_profiles_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    ".csv"
  )
)

data.table::fwrite(
  smoothed_profile_values_wide,
  smoothed_profile_values_wide_output_file
)
message("Smoothed profile values wide table saved: ", smoothed_profile_values_wide_output_file)

# Table 2: Panel B values only. This table has one row per year. The black
# figure points use DO_debt_measured_mgl, Oxygen_mgl, Temperature_degreesC, and
# Salinity_psu. The blue points use Temperature_degreesC_extra_without_DO and
# Salinity_psu_extra_without_DO.
deep_measured_seasonal_values_wide[
  ,
  `:=`(
    figure_panel = "B. Deep profile-deepest July-September measured seasonal averages",
    months_included = months_label,
    min_depth_m_for_panel_b = deepest_min_depth_m
  )
]

deep_measured_col_order <- c(
  "figure_panel",
  "Year",
  "months_included",
  "min_depth_m_for_panel_b",
  "mean_depth_m_corresponding",
  "min_depth_m_corresponding",
  "max_depth_m_corresponding",
  "n_profiles_DO_debt",
  "n_records_at_profile_deepest_DO_debt",
  "n_profiles_Oxygen_mgl",
  "n_profiles_Temperature_degreesC",
  "n_profiles_Salinity_psu",
  "DO_debt_measured_mgl",
  "Oxygen_mgl",
  "Temperature_degreesC",
  "Salinity_psu",
  "mean_depth_m_extra_T_without_DO",
  "min_depth_m_extra_T_without_DO",
  "max_depth_m_extra_T_without_DO",
  "n_profiles_extra_Temperature_degreesC_without_DO",
  "n_records_at_profile_deepest_extra_Temperature_degreesC_without_DO",
  "Temperature_degreesC_extra_without_DO",
  "mean_depth_m_extra_S_without_DO",
  "min_depth_m_extra_S_without_DO",
  "max_depth_m_extra_S_without_DO",
  "n_profiles_extra_Salinity_psu_without_DO",
  "n_records_at_profile_deepest_extra_Salinity_psu_without_DO",
  "Salinity_psu_extra_without_DO"
)

data.table::setcolorder(
  deep_measured_seasonal_values_wide,
  intersect(deep_measured_col_order, names(deep_measured_seasonal_values_wide))
)

data.table::setorder(deep_measured_seasonal_values_wide, Year)

deep_measured_values_wide_output_file <- file.path(
  outputPath,
  paste0(
    "deep_measured_profile_deepest_seasonal_average_values_wide_extra_TS_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    ".csv"
  )
)

data.table::fwrite(
  deep_measured_seasonal_values_wide,
  deep_measured_values_wide_output_file
)
message("Deep measured profile-deepest seasonal average values wide table saved: ", deep_measured_values_wide_output_file)

#-------------------------------------------------------------------------------
# FIGURE: FOUR-PANEL SELECTED DEEP VALUES WITH 1980-2010 PERCENTILES

# This figure uses the selected deep yearly values from Panel B, i.e. the
# profile-deepest July-September values averaged per year after keeping only
# profiles whose selected deepest DO-debt record was measured at >= 40 m.
deep_values_4panel_labels <- c(
  Oxygen_mgl = "DO",
  DO_debt_measured_mgl = "DO debt, H2S + NH4",
  Salinity_psu = "Salinity",
  Temperature_degreesC = "Temperature"
)

deep_values_4panel_long <- data.table::melt(
  deep_measured_seasonal_values_wide,
  id.vars = c("Year"),
  measure.vars = names(deep_values_4panel_labels),
  variable.name = "variable",
  value.name = "value"
)

deep_values_4panel_long <- deep_values_4panel_long[
  Year >= deep_values_plot_start_year &
    !is.na(value)
]

deep_values_4panel_long[, parameter := unname(deep_values_4panel_labels[as.character(variable)])]
deep_values_4panel_long <- deep_values_4panel_long[!is.na(parameter)]
deep_values_4panel_long[, parameter := factor(parameter, levels = unname(deep_values_4panel_labels))]
data.table::setorder(deep_values_4panel_long, parameter, Year)

if (nrow(deep_values_4panel_long) == 0) {
  stop(
    "No selected deep measured values available from ",
    deep_values_plot_start_year,
    " onward for the four-panel percentile figure."
  )
}

baseline_deep_values <- deep_values_4panel_long[
  Year >= deep_values_percentile_baseline_start_year &
    Year <= deep_values_percentile_baseline_end_year
]

if (nrow(baseline_deep_values) == 0) {
  stop(
    "No selected deep measured values available for percentile baseline period ",
    deep_values_percentile_baseline_start_year,
    "-",
    deep_values_percentile_baseline_end_year,
    "."
  )
}

percentile_values_long <- baseline_deep_values[
  ,
  .(
    percentile = paste0("p", deep_values_percentile_probs * 100),
    probability = deep_values_percentile_probs,
    percentile_value = as.numeric(stats::quantile(
      value,
      probs = deep_values_percentile_probs,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )),
    n_years_baseline = data.table::uniqueN(Year[!is.na(value)]),
    baseline_start_year = deep_values_percentile_baseline_start_year,
    baseline_end_year = deep_values_percentile_baseline_end_year
  ),
  by = .(variable, parameter)
]

percentile_values_long[, parameter := factor(parameter, levels = unname(deep_values_4panel_labels))]

deep_values_year_range <- range(deep_values_4panel_long$Year, na.rm = TRUE)
deep_values_x_start <- deep_values_plot_start_year
deep_values_x_end <- max(deep_values_year_range, na.rm = TRUE)

percentile_values_long[, `:=`(
  x_start = deep_values_x_start,
  x_end = deep_values_x_end
)]

percentile_values_output_file <- file.path(
  outputPath,
  paste0(
    "deep_profile_deepest_selected_values_percentiles_",
    deep_values_percentile_baseline_start_year,
    "_",
    deep_values_percentile_baseline_end_year,
    "_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    ".csv"
  )
)

data.table::fwrite(percentile_values_long, percentile_values_output_file)
message("Selected deep values percentile table saved: ", percentile_values_output_file)

p_deep_values_4panel <- ggplot2::ggplot(
  deep_values_4panel_long,
  ggplot2::aes(x = Year, y = value)
) +
  ggplot2::geom_line(linewidth = 0.45, colour = "black", na.rm = TRUE) +
  ggplot2::geom_point(size = 1.9, colour = "black", alpha = 0.85, na.rm = TRUE) +
  ggplot2::geom_segment(
    data = percentile_values_long,
    ggplot2::aes(
      x = x_start,
      xend = x_end,
      y = percentile_value,
      yend = percentile_value,
      linetype = percentile
    ),
    inherit.aes = FALSE,
    linewidth = 0.45,
    alpha = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::scale_x_continuous(
    limits = c(deep_values_x_start, deep_values_x_end),
    breaks = pretty(c(deep_values_x_start, deep_values_x_end), n = 7),
    expand = ggplot2::expansion(mult = c(0, 0.01))
  ) +
  ggplot2::scale_linetype_manual(
    name = paste0(
      deep_values_percentile_baseline_start_year,
      "-",
      deep_values_percentile_baseline_end_year,
      " percentile"
    ),
    values = c(
      p10 = "dotted",
      p25 = "dotdash",
      p50 = "solid",
      p75 = "longdash",
      p90 = "twodash"
    ),
    breaks = c("p10", "p25", "p50", "p75", "p90"),
    labels = c("10th", "25th", "50th", "75th", "90th"),
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Seasonal average value",
    title = paste0(
      "Selected deep July-September values from ",
      deep_values_plot_start_year,
      " onward"
    ),
    subtitle = paste0(
      "Percentile lines are calculated from ",
      deep_values_percentile_baseline_start_year,
      "-",
      deep_values_percentile_baseline_end_year,
      "; selected profile-deepest records at depths >= ",
      deepest_min_depth_m,
      " m"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold")
  )

deep_values_4panel_figure_file <- file.path(
  outputPath,
  paste0(
    "FIG_deep_profile_deepest_selected_values_4panel_percentiles_",
    deep_values_plot_start_year,
    "_to_end_baseline_",
    deep_values_percentile_baseline_start_year,
    "_",
    deep_values_percentile_baseline_end_year,
    "_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    ".jpg"
  )
)

ggplot2::ggsave(
  filename = deep_values_4panel_figure_file,
  plot = p_deep_values_4panel,
  width = 12,
  height = 8,
  units = "in",
  dpi = figure_dpi
)
message("Four-panel selected deep values percentile figure saved: ", deep_values_4panel_figure_file)

#-------------------------------------------------------------------------------
# FIGURE: TWO-PANEL SUMMARY

profile_year_range <- range(profile_data$Year, na.rm = TRUE)
profile_depth_range <- range(profile_data$Depth_m, na.rm = TRUE)

p_profile <- ggplot2::ggplot(
  profile_data,
  ggplot2::aes(
    x = DO_debt_mgl,
    y = Depth_m,
    group = Year,
    colour = Year
  )
) +
  ggplot2::geom_path(linewidth = 0.6, alpha = 0.85) +
  ggplot2::scale_y_reverse(limits = rev(profile_depth_range)) +
  ggplot2::scale_colour_viridis_c(
    limits = profile_year_range,
    breaks = pretty(profile_year_range, n = 6)
  ) +
  ggplot2::labs(
    x = profile_variable_label,
    y = "Depth (m)",
    colour = "Year",
    title = "A. Yearly July-September DO debt profiles"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold")
  )

panel_b_years <- range(c(corresponding_long$Year, extra_ts_long$Year), na.rm = TRUE)

p_deepest <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = corresponding_long,
    ggplot2::aes(x = Year, y = value, group = parameter),
    linewidth = 0.5,
    colour = "black",
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    data = corresponding_long,
    ggplot2::aes(x = Year, y = value, colour = value_source),
    size = 2.1,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    data = extra_ts_long,
    ggplot2::aes(x = Year, y = value, colour = value_source),
    size = 2.1,
    alpha = 0.75,
    na.rm = TRUE
  ) +
  ggplot2::facet_wrap(
    ~ parameter,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(breaks = pretty(panel_b_years, n = 6)) +
  ggplot2::scale_colour_manual(
    name = "Value type",
    values = c(
      "Profile-deepest seasonal average corresponding to DO debt" = "black",
      "Additional profile-deepest seasonal average T/S without DO" = "blue"
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Value",
    title = paste0(
      "B. July-September averages of profile-deepest records at depths >= ",
      deepest_min_depth_m,
      " m"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold")
  )

combined_figure_file <- file.path(
  outputPath,
  paste0(
    "FIG_yearly_DO_debt_profiles_and_deep_profile_deepest_seasonal_averages_extra_TS_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    ".jpg"
  )
)

grDevices::jpeg(
  filename = combined_figure_file,
  width = figure_width,
  height = figure_height,
  units = "in",
  res = figure_dpi
)

grid::grid.newpage()
combined_layout <- grid::grid.layout(
  nrow = 1,
  ncol = 2,
  widths = grid::unit(c(1.15, 1), "null")
)
combined_viewport <- grid::viewport(layout = combined_layout)
grid::pushViewport(combined_viewport)

print(
  p_profile,
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
)

print(
  p_deepest,
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
)

grid::popViewport()
grDevices::dev.off()

message("Combined yearly profile/deep-profile-deepest-seasonal-average figure saved: ", combined_figure_file)

# Also leave these objects in the session for inspection.
invisible(list(
  profile_data = profile_data,
  deep_measured_seasonal_average_values = corresponding_long,
  additional_deep_TS_seasonal_average_values_without_DO = extra_ts_long,
  smoothed_profile_values_wide = smoothed_profile_values_wide,
  deep_measured_seasonal_values_wide = deep_measured_seasonal_values_wide,
  smoothed_profile_values_wide_output_file = smoothed_profile_values_wide_output_file,
  deep_measured_values_wide_output_file = deep_measured_values_wide_output_file,
  percentile_values_long = percentile_values_long,
  percentile_values_output_file = percentile_values_output_file,
  deep_values_4panel_figure_file = deep_values_4panel_figure_file,
  combined_figure_file = combined_figure_file
))
