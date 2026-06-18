# scripts/08_4_EQRS.R
#
# Run this after script 08_3.
#
# Purpose:
#   Calculate EQR and EQRS scaled indicator values for the Gulf of Riga open sea
#   oxygen indicator using the selected deep yearly values from script 08_3.
#
# Inputs:
#   - deep_measured_profile_deepest_seasonal_average_values_wide_extra_TS_min<configured_depth>m_<mode>_<method>_months_<configured_months>.csv
#     created by script 08_3.
#
# Logic:
#   - ES is the measured yearly seasonal average value.
#   - ACCDEV is 25%.
#   - Oxygen response sign is ">=".
#   - Oxygen debt response sign is "<=".
#   - Because ET target values are not used here, BEST is taken directly from
#     the 1980-2010 baseline distribution:
#       * Oxygen BEST = 90th percentile of selected deep Oxygen_mgl.
#       * Oxygen debt BEST = 10th percentile of selected deep DO_debt_measured_mgl.
#
# Outputs:
#   - EQRS_yearly_DO_and_DO_debt_min<configured_depth>m_<mode>_<method>_months_<configured_months>.csv
#   - EQRS_final_result_DO_and_DO_debt_min<configured_depth>m_<mode>_<method>_months_<configured_months>.csv
#   - EQRS_BEST_values_DO_and_DO_debt_baseline_<baseline>_min<configured_depth>m_<mode>_<method>_months_<configured_months>.csv
#   - FIG_EQRS_yearly_DO_and_DO_debt_class_background_period_averages_min<configured_depth>m_<mode>_<method>_months_<configured_months>.jpg
#   - EQRS_period_averages_DO_and_DO_debt_min<configured_depth>m_<mode>_<method>_months_<configured_months>.csv

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

# Define paths.
inputPath <- assessment$master_input_dir
outputPath <- assessment$output_dir

# Remove unnecessary data/values/functions.
keep <- c(
  "assessment", "indicator", "outputPath", "inputPath", "proj",
  "repo_url", "O2satFun", "auxilliaryFile"
)

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# SETTINGS FROM scripts/03_define_assessment.R

profile_months <- indicator$eqrs$profile_months
seasonal_average_first <- indicator$eqrs$seasonal_average_first
smooth_method <- indicator$eqrs$smooth_method
deepest_min_depth_m <- indicator$eqrs$deepest_min_depth_m
bottom_suffix <- indicator$near_bottom$bottom_suffix

best_baseline_start_year <- indicator$eqrs$best_baseline_start_year
best_baseline_end_year <- indicator$eqrs$best_baseline_end_year
ACCDEV <- indicator$eqrs$ACCDEV
final_result_year <- indicator$eqrs$final_result_year
cap_eqrs_to_0_1 <- indicator$eqrs$cap_eqrs_to_0_1
use_continuous_upper_eqrs_class <- indicator$eqrs$use_continuous_upper_eqrs_class

eqrs_plot_start_year <- indicator$eqrs$eqrs_plot_start_year
eqrs_figure_width <- indicator$eqrs$figure_width
eqrs_figure_height <- indicator$eqrs$figure_height
eqrs_figure_dpi <- indicator$eqrs$figure_dpi
eqrs_period_averages <- data.table::as.data.table(indicator$eqrs$period_averages)
eqrs_parameters <- data.table::as.data.table(indicator$eqrs$parameters)

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS

make_months_label <- function(months) {
  months <- sort(unique(as.integer(months)))
  paste0(sprintf("%02d", months), collapse = "_")
}

find_first_existing_file <- function(paths) {
  paths <- unique(paths)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) {
    return(NA_character_)
  }
  hit[1]
}

find_latest_matching_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    return(NA_character_)
  }
  files[which.max(file.info(files)$mtime)]
}

safe_quantile <- function(x, prob) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
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

calc_boundaries <- function(response, accdev) {
  if (response == ">=") {
    eqr_gm <- 1 - accdev / 100
  } else if (response == "<=") {
    eqr_gm <- 1 / (1 + accdev / 100)
  } else {
    stop("Unknown response sign: ", response)
  }

  eqr_hg <- 0.5 * 0.95 + 0.5 * eqr_gm
  eqr_pb <- 2 * eqr_gm - 0.95
  eqr_mp <- 0.5 * eqr_gm + 0.5 * eqr_pb

  list(
    EQR_PB = eqr_pb,
    EQR_MP = eqr_mp,
    EQR_GM = eqr_gm,
    EQR_HG = eqr_hg
  )
}

calc_eqr <- function(es, best, response) {
  if (is.na(es) || is.na(best)) {
    return(NA_real_)
  }

  if (response == ">=") {
    if (best == 0) {
      return(NA_real_)
    }
    return(es / best)
  }

  if (response == "<=") {
    if (es == 0 && best >= 0) {
      return(Inf)
    }
    return(best / es)
  }

  stop("Unknown response sign: ", response)
}

calc_eqrs <- function(eqr,
                      eqr_pb,
                      eqr_mp,
                      eqr_gm,
                      eqr_hg,
                      cap_to_0_1 = TRUE,
                      continuous_upper_class = TRUE) {
  if (is.na(eqr)) {
    return(NA_real_)
  }

  if (is.infinite(eqr) && eqr > 0) {
    return(if (cap_to_0_1) 1 else Inf)
  }

  if (eqr <= eqr_pb) {
    eqrs <- (eqr - 0) * (0.2 - 0) / (eqr_pb - 0) + 0
  } else if (eqr <= eqr_mp) {
    eqrs <- (eqr - eqr_pb) * (0.4 - 0.2) / (eqr_mp - eqr_pb) + 0.2
  } else if (eqr <= eqr_gm) {
    eqrs <- (eqr - eqr_mp) * (0.6 - 0.4) / (eqr_gm - eqr_mp) + 0.4
  } else if (eqr <= eqr_hg) {
    eqrs <- (eqr - eqr_gm) * (0.8 - 0.6) / (eqr_hg - eqr_gm) + 0.6
  } else {
    if (continuous_upper_class) {
      eqrs <- (eqr - eqr_hg) * (1 - 0.8) / (1 - eqr_hg) + 0.8
    } else {
      eqrs <- (eqr - eqr_hg) * (1 - 0.8) / (1 - eqr_hg + 0.8)
    }
  }

  if (cap_to_0_1) {
    eqrs <- max(0, min(1, eqrs))
  }

  eqrs
}

assign_status_class <- function(eqrs) {
  if (is.na(eqrs)) {
    return(NA_character_)
  }
  if (eqrs < 0.2) {
    return("Bad")
  }
  if (eqrs < 0.4) {
    return("Poor")
  }
  if (eqrs < 0.6) {
    return("Moderate")
  }
  if (eqrs < 0.8) {
    return("Good")
  }
  "High"
}

#-------------------------------------------------------------------------------
# READ SELECTED DEEP YEARLY VALUES FROM SCRIPT 08_3

profile_months <- sort(unique(as.integer(profile_months)))
months_label <- make_months_label(profile_months)
mode_label <- if (seasonal_average_first) "seasonal" else "monthly"

method_label_candidates <- unique(c(
  smooth_method,
  toupper(smooth_method),
  tolower(smooth_method)
))

exact_input_candidates <- file.path(
  outputPath,
  paste0(
    "deep_measured_profile_deepest_seasonal_average_values_wide_extra_TS_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    method_label_candidates,
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

selected_deep_values_file <- find_first_existing_file(exact_input_candidates)

if (is.na(selected_deep_values_file)) {
  selected_deep_values_file <- find_latest_matching_file(
    outputPath,
    paste0(
      "^deep_measured_profile_deepest_seasonal_average_values_wide.*min",
      deepest_min_depth_m,
      "m_.*months_",
      months_label,
      "\\.csv$"
    )
  )
}

if (is.na(selected_deep_values_file)) {
  stop(
    "Missing selected deep yearly input file from script 08_3. Checked exact paths:\n  ",
    paste(exact_input_candidates, collapse = "\n  "),
    "\nRun script 08_3 first, or check the settings in this script."
  )
}

message("Reading selected deep yearly values: ", selected_deep_values_file)
selected_deep_values <- data.table::fread(selected_deep_values_file)

required_cols <- c("Year", eqrs_parameters$measured_column)
check_required_cols(selected_deep_values, required_cols, basename(selected_deep_values_file))

selected_deep_values[, Year := as.integer(Year)]

#-------------------------------------------------------------------------------
# CALCULATE BEST VALUES FROM 1980-2010 BASELINE

baseline_values <- selected_deep_values[
  Year >= best_baseline_start_year &
    Year <= best_baseline_end_year
]

if (nrow(baseline_values) == 0) {
  stop(
    "No selected deep yearly values found for BEST baseline period ",
    best_baseline_start_year,
    "-",
    best_baseline_end_year,
    "."
  )
}

best_values <- data.table::rbindlist(lapply(seq_len(nrow(eqrs_parameters)), function(i) {
  p <- eqrs_parameters[i]
  x <- baseline_values[[p$measured_column]]
  x_non_na <- x[!is.na(x)]

  data.table::data.table(
    parameter = p$parameter,
    measured_column = p$measured_column,
    units = p$units,
    response = p$response,
    ACCDEV = ACCDEV,
    best_baseline_start_year = best_baseline_start_year,
    best_baseline_end_year = best_baseline_end_year,
    best_percentile_label = p$best_percentile_label,
    best_percentile_probability = p$best_percentile_probability,
    n_years_baseline = length(x_non_na),
    BEST = safe_quantile(x, p$best_percentile_probability)
  )
}), use.names = TRUE)

if (any(is.na(best_values$BEST))) {
  stop(
    "Could not calculate BEST for: ",
    paste(best_values[is.na(BEST), parameter], collapse = ", "),
    ". Check baseline data availability."
  )
}

# Add EQR boundaries for each parameter.
boundary_values <- data.table::rbindlist(lapply(seq_len(nrow(best_values)), function(i) {
  b <- calc_boundaries(best_values$response[i], best_values$ACCDEV[i])
  data.table::data.table(
    parameter = best_values$parameter[i],
    EQR_PB = b$EQR_PB,
    EQR_MP = b$EQR_MP,
    EQR_GM = b$EQR_GM,
    EQR_HG = b$EQR_HG
  )
}), use.names = TRUE)

best_values <- merge(best_values, boundary_values, by = "parameter", all.x = TRUE)

best_values_output_file <- file.path(
  outputPath,
  paste0(
    "EQRS_BEST_values_DO_and_DO_debt_baseline_",
    best_baseline_start_year,
    "_",
    best_baseline_end_year,
    "_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

data.table::fwrite(best_values, best_values_output_file)
message("BEST and boundary values saved: ", best_values_output_file)

#-------------------------------------------------------------------------------
# CALCULATE YEARLY EQR AND EQRS

eqrs_yearly <- data.table::rbindlist(lapply(seq_len(nrow(eqrs_parameters)), function(i) {
  p <- eqrs_parameters[i]
  b <- best_values[parameter == p$parameter]

  out <- selected_deep_values[
    ,
    .(
      Year = Year,
      parameter = p$parameter,
      measured_column = p$measured_column,
      units = p$units,
      response = p$response,
      ACCDEV = ACCDEV,
      ES = get(p$measured_column),
      BEST = b$BEST,
      best_percentile_label = b$best_percentile_label,
      best_percentile_probability = b$best_percentile_probability,
      best_baseline_start_year = b$best_baseline_start_year,
      best_baseline_end_year = b$best_baseline_end_year,
      n_years_baseline = b$n_years_baseline,
      EQR_PB = b$EQR_PB,
      EQR_MP = b$EQR_MP,
      EQR_GM = b$EQR_GM,
      EQR_HG = b$EQR_HG
    )
  ]

  out[, EQR := vapply(
    ES,
    calc_eqr,
    numeric(1),
    best = b$BEST,
    response = p$response
  )]

  out[, EQRS_raw := vapply(
    EQR,
    calc_eqrs,
    numeric(1),
    eqr_pb = b$EQR_PB,
    eqr_mp = b$EQR_MP,
    eqr_gm = b$EQR_GM,
    eqr_hg = b$EQR_HG,
    cap_to_0_1 = FALSE,
    continuous_upper_class = use_continuous_upper_eqrs_class
  )]

  out[, EQRS := vapply(
    EQR,
    calc_eqrs,
    numeric(1),
    eqr_pb = b$EQR_PB,
    eqr_mp = b$EQR_MP,
    eqr_gm = b$EQR_GM,
    eqr_hg = b$EQR_HG,
    cap_to_0_1 = cap_eqrs_to_0_1,
    continuous_upper_class = use_continuous_upper_eqrs_class
  )]

  out[, status_class := vapply(EQRS, assign_status_class, character(1))]
  out[]
}), use.names = TRUE)

data.table::setorder(eqrs_yearly, parameter, Year)

eqrs_yearly_output_file <- file.path(
  outputPath,
  paste0(
    "EQRS_yearly_DO_and_DO_debt_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

data.table::fwrite(eqrs_yearly, eqrs_yearly_output_file)
message("Yearly EQR/EQRS table saved: ", eqrs_yearly_output_file)

#-------------------------------------------------------------------------------
# EQRS CLASS FIGURE WITH PERIOD AVERAGES

# Background bands for EQRS class boundaries.
eqrs_class_bands <- data.table::data.table(
  status_class = factor(
    c("Bad", "Poor", "Moderate", "Good", "High"),
    levels = c("Bad", "Poor", "Moderate", "Good", "High")
  ),
  ymin = c(0.0, 0.2, 0.4, 0.6, 0.8),
  ymax = c(0.2, 0.4, 0.6, 0.8, 1.0),
  fill_colour = c("#b2182b", "#ef8a62", "#fddbc7", "#a6dba0", "#1b7837")
)

eqrs_plot_data <- data.table::copy(eqrs_yearly)[
  Year >= eqrs_plot_start_year &
    !is.na(EQRS)
]

eqrs_period_average_values <- data.table::rbindlist(lapply(seq_len(nrow(eqrs_period_averages)), function(i) {
  this_period <- eqrs_period_averages[i]

  eqrs_plot_data[
    Year >= this_period$start_year & Year <= this_period$end_year,
    .(
      period = this_period$period,
      start_year = this_period$start_year,
      end_year = this_period$end_year,
      n_years = .N,
      EQRS_period_average = mean(EQRS, na.rm = TRUE)
    ),
    by = .(parameter, units)
  ]
}), use.names = TRUE, fill = TRUE)

eqrs_period_average_values <- eqrs_period_average_values[!is.nan(EQRS_period_average)]

eqrs_period_average_output_file <- file.path(
  outputPath,
  paste0(
    "EQRS_period_averages_DO_and_DO_debt_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

data.table::fwrite(eqrs_period_average_values, eqrs_period_average_output_file)
message("EQRS period-average table saved: ", eqrs_period_average_output_file)

if (nrow(eqrs_plot_data) > 0) {
  x_min <- min(eqrs_plot_data$Year, na.rm = TRUE)
  x_max <- max(eqrs_plot_data$Year, na.rm = TRUE)

  eqrs_class_label_data <- eqrs_class_bands[
    ,
    .(
      x = x_min,
      y = (ymin + ymax) / 2,
      label = as.character(status_class)
    )
  ]

  eqrs_figure <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = eqrs_class_bands,
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = ymin,
        ymax = ymax,
        fill = fill_colour
      ),
      alpha = 0.55,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = c(0.2, 0.4, 0.6, 0.8),
      linewidth = 0.25,
      colour = "grey35"
    ) +
    ggplot2::geom_line(
      data = eqrs_plot_data,
      ggplot2::aes(x = Year, y = EQRS),
      linewidth = 0.45,
      colour = "black"
    ) +
    ggplot2::geom_point(
      data = eqrs_plot_data,
      ggplot2::aes(x = Year, y = EQRS),
      size = 1.7,
      colour = "black"
    ) +
    ggplot2::geom_segment(
      data = eqrs_period_average_values,
      ggplot2::aes(
        x = start_year,
        xend = end_year,
        y = EQRS_period_average,
        yend = EQRS_period_average,
        linetype = period
      ),
      linewidth = 1.15,
      colour = "blue"
    ) +
    ggplot2::geom_text(
      data = eqrs_class_label_data,
      ggplot2::aes(x = x, y = y, label = label),
      hjust = 0,
      size = 3.1,
      colour = "grey15",
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~ parameter, ncol = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_linetype_manual(
      name = "Period average",
      values = c("2011-2016" = "solid", "2016-2021" = "dashed")
    ) +
    ggplot2::coord_cartesian(
      xlim = c(x_min, x_max),
      ylim = c(0, 1),
      expand = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = pretty(c(x_min, x_max), n = 8)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, by = 0.2)
    ) +
    ggplot2::labs(
      title = "Yearly EQRS for selected deep oxygen values",
      subtitle = paste0(
        "Class bands shown as EQRS background; period averages: 2011-2016 and 2016-2021"
      ),
      x = "Year",
      y = "EQRS"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.2, colour = "grey85"),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey90", colour = "grey30"),
      strip.text = ggplot2::element_text(face = "bold")
    )

  eqrs_figure_file <- file.path(
    outputPath,
    paste0(
      "FIG_EQRS_yearly_DO_and_DO_debt_class_background_period_averages_min",
      deepest_min_depth_m,
      "m_",
      mode_label,
      "_",
      tolower(smooth_method),
      "_months_",
      months_label,
      "_",
      bottom_suffix,
      ".jpg"
    )
  )

  ggplot2::ggsave(
    filename = eqrs_figure_file,
    plot = eqrs_figure,
    width = eqrs_figure_width,
    height = eqrs_figure_height,
    units = "in",
    dpi = eqrs_figure_dpi
  )

  message("EQRS class-background figure saved: ", eqrs_figure_file)
} else {
  warning("No EQRS values available from ", eqrs_plot_start_year, " onward. EQRS figure was not created.")
  eqrs_figure_file <- NA_character_
}

#-------------------------------------------------------------------------------
# FINAL RESULT TABLE

if (is.na(final_result_year)) {
  eqrs_final <- eqrs_yearly[
    !is.na(ES),
    .SD[Year == max(Year, na.rm = TRUE)],
    by = parameter
  ]
} else {
  eqrs_final <- eqrs_yearly[Year == final_result_year]
}

if (nrow(eqrs_final) == 0) {
  stop("No final-result rows could be selected. Check final_result_year and input data.")
}

eqrs_final[, final_result_selection := if (is.na(final_result_year)) {
  "Latest available year per parameter"
} else {
  paste0("Requested year ", final_result_year)
}]

final_col_order <- c(
  "final_result_selection",
  "Year",
  "parameter",
  "measured_column",
  "units",
  "response",
  "ACCDEV",
  "ES",
  "BEST",
  "best_percentile_label",
  "best_percentile_probability",
  "best_baseline_start_year",
  "best_baseline_end_year",
  "n_years_baseline",
  "EQR",
  "EQR_PB",
  "EQR_MP",
  "EQR_GM",
  "EQR_HG",
  "EQRS_raw",
  "EQRS",
  "status_class"
)

data.table::setcolorder(eqrs_final, intersect(final_col_order, names(eqrs_final)))
data.table::setorder(eqrs_final, parameter)

eqrs_final_output_file <- file.path(
  outputPath,
  paste0(
    "EQRS_final_result_DO_and_DO_debt_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

data.table::fwrite(eqrs_final, eqrs_final_output_file)
message("Final EQR/EQRS result table saved: ", eqrs_final_output_file)

# Also create a compact wide final table.
eqrs_final_wide <- data.table::dcast(
  eqrs_final,
  Year + final_result_selection ~ parameter,
  value.var = c("ES", "BEST", "EQR", "EQRS", "status_class")
)

eqrs_final_wide_output_file <- file.path(
  outputPath,
  paste0(
    "EQRS_final_result_DO_and_DO_debt_wide_min",
    deepest_min_depth_m,
    "m_",
    mode_label,
    "_",
    tolower(smooth_method),
    "_months_",
    months_label,
    "_",
    bottom_suffix,
    ".csv"
  )
)

data.table::fwrite(eqrs_final_wide, eqrs_final_wide_output_file)
message("Final EQR/EQRS wide result table saved: ", eqrs_final_wide_output_file)

#-------------------------------------------------------------------------------
# OBJECTS KEPT FOR INTERACTIVE CHECKING

result_objects <- list(
  selected_deep_values_file = selected_deep_values_file,
  best_values = best_values,
  eqrs_yearly = eqrs_yearly,
  eqrs_final = eqrs_final,
  eqrs_yearly_output_file = eqrs_yearly_output_file,
  eqrs_final_output_file = eqrs_final_output_file,
  eqrs_final_wide_output_file = eqrs_final_wide_output_file,
  best_values_output_file = best_values_output_file,
  eqrs_period_average_output_file = eqrs_period_average_output_file,
  eqrs_figure_file = eqrs_figure_file
)

message("08_4_EQRS complete.")
