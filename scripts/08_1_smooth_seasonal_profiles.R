# scripts/08_1_smooth_seasonal_profiles.R
# This script creates smoothed time-depth profile fields from monthly_mean_profiles_1m.csv.
# It is intended as a simpler DIVA-like exploratory analysis, but it is not DIVA/ODV.
#
# Main idea:
#   1. Read monthly mean profiles created by scripts/08_monthly_mean_deep_data.R.
#   2. Optionally average selected months first, e.g. July-October -> one seasonal profile per year.
#   3. Fit a smooth 2D surface through time and depth using a GAM.
#   4. Predict a smooth field for all year-depth or month-depth grid cells.
#   5. Save the smoothed dataset and a five-panel figure.

options(project_clean_workspace = FALSE)

source("scripts/01_header.R")

if (is.null(getOption("project_assessment"))) {
  source("scripts/03_define_assessment.R")
}

assessment <- getOption("project_assessment")

# Define paths
inputPath <- "Input/master"
outputPath <- assessment$output_dir

# Remove unnecessary data/values/functions
keep <- c(
  "assessment", "outputPath", "inputPath", "proj",
  "repo_url", "O2satFun", "auxilliaryFile"
)

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

#-------------------------------------------------------------------------------
# USER SETTINGS

# Months to include.
# Examples:
#   profile_smooth_months <- 7:10
#   profile_smooth_months <- c(8, 9)
#   profile_smooth_months <- 6:11
profile_smooth_months <- 7:9

# If TRUE, selected months are averaged first, giving one profile per year.
# This is recommended for a seasonal indicator such as July-October.
# If FALSE, the model uses monthly profiles directly.
seasonal_average_first <- TRUE

# Variables to smooth and plot.
smooth_variables <- c(
  "Oxygen_mgl",
  "Oxygen_debt_mgl_H2S_NH4",
  "Temperature_degreesC",
  "Salinity_psu"
)

# Optional maximum depth shown/smoothed.
# Set to NULL to use full depth range.
max_depth_for_smoothing <- NULL

# Smoothing method.
# Current implemented option: "gam".
smooth_method <- "gam"

# GAM settings.
# Smaller k values = smoother fields. Larger k values = more detail, but more risk of overfitting.
gam_k_time <- 12
gam_k_depth <- 10
gam_method <- "REML"
min_observations_for_gam <- 20

# Distance mask to avoid retaining predictions far away from observations.
apply_distance_mask <- TRUE
max_time_gap_years <- 5
max_depth_gap_m <- 10
max_scaled_distance <- 1

# Figure settings.
figure_width <- 14
figure_height <- 10
figure_dpi <- 300

# Colour palette for profile figures.
# Options: "parula", "jet", "viridis", "heat".
profile_colour_palette <- "jet"

#-------------------------------------------------------------------------------
# READ DATA

if (!requireNamespace("mgcv", quietly = TRUE)) {
  stop("Package 'mgcv' is needed for GAM smoothing. Add it to scripts/02_setup.R or install it.")
}

monthly_profiles_file <- file.path(outputPath, "monthly_mean_profiles_1m.csv")

if (!file.exists(monthly_profiles_file)) {
  stop(
    "Missing input file: ", monthly_profiles_file, "\n",
    "Run scripts/08_monthly_mean_deep_data.R first."
  )
}

monthly_profiles <- data.table::fread(monthly_profiles_file)
data.table::setDT(monthly_profiles)

required_cols <- c("Year", "Month", "Depth_m", smooth_variables)

if (!all(required_cols %in% names(monthly_profiles))) {
  stop(
    "Missing required columns in monthly_mean_profiles_1m.csv: ",
    paste(setdiff(required_cols, names(monthly_profiles)), collapse = ", ")
  )
}

monthly_profiles[, Year := as.integer(Year)]
monthly_profiles[, Month := as.integer(Month)]
monthly_profiles[, Depth_m := as.numeric(Depth_m)]

monthly_profiles[, Date := as.Date(
  paste0(Year, "-", sprintf("%02d", Month), "-01")
)]

# Fractional year for time-depth smoothing.
monthly_profiles[, time_value := Year + (Month - 0.5) / 12]

# Keep selected months.
profile_smooth_months <- sort(unique(as.integer(profile_smooth_months)))
analysis_data <- monthly_profiles[Month %in% profile_smooth_months]

if (!is.null(max_depth_for_smoothing)) {
  analysis_data <- analysis_data[Depth_m <= max_depth_for_smoothing]
}

if (nrow(analysis_data) == 0) {
  stop("No monthly profile rows found for the selected months/depth range.")
}

#-------------------------------------------------------------------------------
# HELPER FUNCTIONS

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

safe_range <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r))) {
    r <- c(0, 1)
  }
  if (r[1] == r[2]) {
    r <- r + c(-0.5, 0.5)
  }
  r
}

make_months_label <- function(months) {
  months <- sort(unique(as.integer(months)))
  paste0(sprintf("%02d", months), collapse = "_")
}

make_profile_palette <- function(palette = "parula", n = 100) {
  palette <- tolower(palette)
  
  if (palette == "jet") {
    return(grDevices::colorRampPalette(
      c("#00007F", "#0000FF", "#007FFF", "#00FFFF", "#7FFF7F",
        "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")
    )(n))
  }
  
  if (palette == "parula") {
    return(grDevices::colorRampPalette(
      c("#352A87", "#1464D2", "#06A7C6", "#38B977", "#B6C84B", "#F9FB0E")
    )(n))
  }
  
  if (palette == "viridis") {
    return(grDevices::hcl.colors(n, "viridis", rev = FALSE))
  }
  
  if (palette == "heat") {
    return(grDevices::hcl.colors(n, "YlOrRd", rev = FALSE))
  }
  
  stop("Unknown profile_colour_palette: ", palette)
}

reduce_gam_k <- function(n_obs, k_time, k_depth) {
  k_time <- max(3, as.integer(k_time))
  k_depth <- max(3, as.integer(k_depth))
  
  while ((k_time * k_depth) >= n_obs && (k_time > 3 || k_depth > 3)) {
    if (k_time >= k_depth && k_time > 3) {
      k_time <- k_time - 1L
    } else if (k_depth > 3) {
      k_depth <- k_depth - 1L
    } else {
      break
    }
  }
  
  c(k_time = k_time, k_depth = k_depth)
}

#-------------------------------------------------------------------------------
# OPTIONAL SEASONAL AVERAGING

if (seasonal_average_first) {
  # Average selected months first, producing one seasonal mean profile per year.
  # This reduces monthly patchiness and is recommended when the indicator itself is seasonal.
  analysis_data <- analysis_data[
    ,
    c(
      .(
        Month = NA_integer_,
        Date = as.Date(paste0(first(Year), "-09-15")),
        time_value = as.numeric(first(Year)),
        n_selected_month_rows = .N
      ),
      lapply(.SD, safe_mean)
    ),
    by = .(Year, Depth_m),
    .SDcols = smooth_variables
  ]
  
  grid_all <- data.table::CJ(
    Year = assessment$start_year:assessment$end_year,
    Depth_m = sort(unique(analysis_data$Depth_m))
  )
  
  grid_all[, Month := NA_integer_]
  grid_all[, Date := as.Date(paste0(Year, "-09-15"))]
  grid_all[, time_value := as.numeric(Year)]
  
  analysis_data <- merge(
    grid_all,
    analysis_data,
    by = c("Year", "Depth_m", "Month", "Date", "time_value"),
    all.x = TRUE
  )
  
} else {
  # Use monthly profiles directly.
  grid_all <- data.table::CJ(
    Year = assessment$start_year:assessment$end_year,
    Month = profile_smooth_months,
    Depth_m = sort(unique(analysis_data$Depth_m))
  )
  
  grid_all[, Date := as.Date(paste0(Year, "-", sprintf("%02d", Month), "-01"))]
  grid_all[, time_value := Year + (Month - 0.5) / 12]
  
  analysis_data <- merge(
    grid_all,
    analysis_data,
    by = c("Year", "Month", "Depth_m", "Date", "time_value"),
    all.x = TRUE
  )
}

data.table::setorder(analysis_data, time_value, Depth_m)

#-------------------------------------------------------------------------------
# SMOOTH ONE VARIABLE USING GAM

smooth_one_variable_gam <- function(data,
                                    value_col,
                                    gam_k_time = 12,
                                    gam_k_depth = 10,
                                    gam_method = "REML",
                                    min_observations_for_gam = 20,
                                    apply_distance_mask = TRUE,
                                    max_time_gap_years = 5,
                                    max_depth_gap_m = 10,
                                    max_scaled_distance = 1) {
  
  x <- data.table::copy(data)
  observed_col <- value_col
  smoothed_col <- paste0(value_col, "_smoothed")
  residual_col <- paste0(value_col, "_minus_smoothed")
  distance_col <- paste0(value_col, "_nearest_scaled_distance")
  
  x[, (smoothed_col) := NA_real_]
  x[, (residual_col) := NA_real_]
  x[, (distance_col) := NA_real_]
  
  obs <- x[!is.na(get(value_col))]
  
  if (nrow(obs) < min_observations_for_gam) {
    warning(
      "Too few observations to smooth ", value_col,
      ": n = ", nrow(obs), ". Returning observed values only."
    )
    return(x[, .SD, .SDcols = c(
      "Year", "Month", "Date", "time_value", "Depth_m",
      observed_col, smoothed_col, residual_col, distance_col
    )])
  }
  
  if (data.table::uniqueN(obs$time_value) < 3 || data.table::uniqueN(obs$Depth_m) < 3) {
    warning(
      "Too few unique time or depth values to smooth ", value_col,
      ". Returning observed values only."
    )
    return(x[, .SD, .SDcols = c(
      "Year", "Month", "Date", "time_value", "Depth_m",
      observed_col, smoothed_col, residual_col, distance_col
    )])
  }
  
  k_time_eff <- min(gam_k_time, data.table::uniqueN(obs$time_value))
  k_depth_eff <- min(gam_k_depth, data.table::uniqueN(obs$Depth_m))
  k_eff <- reduce_gam_k(
    n_obs = nrow(obs),
    k_time = k_time_eff,
    k_depth = k_depth_eff
  )
  
  model_data <- data.frame(
    value = obs[[value_col]],
    time_value = obs$time_value,
    Depth_m = obs$Depth_m
  )
  
  # mgcv smooth terms should be used as te(), not mgcv::te(), inside formulas.
  te <- mgcv::te
  
  gam_formula <- stats::as.formula(
    paste0(
      "value ~ te(time_value, Depth_m, k = c(",
      k_eff[["k_time"]], ", ",
      k_eff[["k_depth"]], "))"
    )
  )
  
  fit <- mgcv::gam(
    formula = gam_formula,
    data = model_data,
    method = gam_method
  )
  
  pred_data <- data.frame(
    time_value = x$time_value,
    Depth_m = x$Depth_m
  )
  
  predicted_value <- as.numeric(stats::predict(fit, newdata = pred_data))
  
  if (apply_distance_mask) {
    obs_time <- obs$time_value
    obs_depth <- obs$Depth_m
    
    nearest_scaled_distance <- vapply(
      seq_len(nrow(x)),
      function(i) {
        d <- sqrt(
          ((x$time_value[i] - obs_time) / max_time_gap_years)^2 +
            ((x$Depth_m[i] - obs_depth) / max_depth_gap_m)^2
        )
        min(d, na.rm = TRUE)
      },
      numeric(1)
    )
    
    x[, (distance_col) := nearest_scaled_distance]
    predicted_value[nearest_scaled_distance > max_scaled_distance] <- NA_real_
 # Even if a monitoring value exists in a year, the smoothed value can be removed if the selected time-depth cell is considered too far from the observations used for that specific variable.
}
  
  x[, (smoothed_col) := predicted_value]
  x[, (residual_col) := get(value_col) - get(smoothed_col)]
  
  x[, .SD, .SDcols = c(
    "Year", "Month", "Date", "time_value", "Depth_m",
    observed_col, smoothed_col, residual_col, distance_col
  )]
}

#-------------------------------------------------------------------------------
# SMOOTH ALL VARIABLES

smoothed_list <- lapply(
  smooth_variables,
  function(v) {
    smooth_one_variable_gam(
      data = analysis_data,
      value_col = v,
      gam_k_time = gam_k_time,
      gam_k_depth = gam_k_depth,
      gam_method = gam_method,
      min_observations_for_gam = min_observations_for_gam,
      apply_distance_mask = apply_distance_mask,
      max_time_gap_years = max_time_gap_years,
      max_depth_gap_m = max_depth_gap_m,
      max_scaled_distance = max_scaled_distance
    )
  }
)

smoothed_profiles <- smoothed_list[[1]]

if (length(smoothed_list) > 1) {
  for (i in 2:length(smoothed_list)) {
    smoothed_profiles <- merge(
      smoothed_profiles,
      smoothed_list[[i]],
      by = c("Year", "Month", "Date", "time_value", "Depth_m"),
      all = TRUE
    )
  }
}

months_label <- make_months_label(profile_smooth_months)
mode_label <- if (seasonal_average_first) "seasonal" else "monthly"

smoothed_output_file <- file.path(
  outputPath,
  paste0("monthly_mean_profiles_1m_", mode_label, "_GAM_smoothed_months_", months_label, ".csv")
)

data.table::fwrite(smoothed_profiles, smoothed_output_file)

message("Smoothed profile data saved: ", smoothed_output_file)

#-------------------------------------------------------------------------------
# FIGURE: SMOOTHED PROFILES, 4 PANELS

plot_smoothed_profile_panels <- function(data,
                                         months_label,
                                         mode_label,
                                         output_dir,
                                         width = 14,
                                         height = 8,
                                         dpi = 300,
                                         palette = "parula") {
  
  panel_specs <- list(
    list(column = "Oxygen_mgl_smoothed", title = "Oxygen", legend_title = "mg/l", reverse_palette = TRUE),
    list(column = "Oxygen_debt_mgl_H2S_NH4_smoothed", title = "Oxygen debt, H2S + NH4", legend_title = "mg/l", reverse_palette = FALSE),
    list(column = "Temperature_degreesC_smoothed", title = "Temperature", legend_title = "degrees C", reverse_palette = FALSE),
    list(column = "Salinity_psu_smoothed", title = "Salinity", legend_title = "psu", reverse_palette = FALSE)
  )
  
  required_plot_cols <- c("time_value", "Depth_m", vapply(panel_specs, function(z) z$column, character(1)))
  
  if (!all(required_plot_cols %in% names(data))) {
    stop(
      "Missing required columns for smoothed profile panel figure: ",
      paste(setdiff(required_plot_cols, names(data)), collapse = ", ")
    )
  }
  
  x <- data.table::copy(data)
  x[, time_value := as.numeric(time_value)]
  x[, Depth_m := as.numeric(Depth_m)]
  data.table::setorder(x, time_value, Depth_m)
  
  base_cols <- make_profile_palette(palette = palette, n = 100)
  
  plot_one_panel <- function(panel_data, value_col, panel_title, cols) {
    
    z_data <- panel_data[!is.na(get(value_col))]
    
    if (nrow(z_data) == 0) {
      graphics::plot.new()
      graphics::title(main = panel_title)
      graphics::text(0.5, 0.5, "No data")
      return(invisible(NULL))
    }
    
    wide <- data.table::dcast(
      z_data,
      Depth_m ~ time_value,
      value.var = value_col
    )
    
    time_cols <- setdiff(names(wide), "Depth_m")
    times <- as.numeric(time_cols)
    depths <- wide$Depth_m
    z <- as.matrix(wide[, ..time_cols])
    zlim <- safe_range(z)
    
    time_ticks <- pretty(times, n = 6)
    time_ticks <- time_ticks[time_ticks >= min(times) & time_ticks <= max(times)]
    
    graphics::image(
      x = times,
      y = depths,
      z = t(z),
      col = cols,
      zlim = zlim,
      xlab = "Year",
      ylab = "Depth (m)",
      main = panel_title,
      ylim = rev(range(depths, na.rm = TRUE)),
      axes = FALSE
    )
    
    graphics::axis(side = 1, at = time_ticks, labels = round(time_ticks, 0), las = 1)
    graphics::axis(side = 2, las = 1)
    graphics::box()
    
    invisible(zlim)
  }
  
  plot_one_legend <- function(zlim, legend_title, cols) {
    
    if (is.null(zlim) || !all(is.finite(zlim))) {
      graphics::plot.new()
      return(invisible(NULL))
    }
    
    z_seq <- seq(zlim[1], zlim[2], length.out = length(cols))
    z_breaks <- seq(zlim[1], zlim[2], length.out = length(cols) + 1)
    
    graphics::image(
      x = c(0, 1),
      y = z_breaks,
      z = matrix(z_seq, nrow = 1),
      col = cols,
      axes = FALSE,
      xlab = "",
      ylab = ""
    )
    
    graphics::axis(side = 4, las = 1, cex.axis = 0.8)
    graphics::mtext(legend_title, side = 3, line = 0.5, cex = 0.8)
    
    invisible(NULL)
  }
  
  output_file <- file.path(
    output_dir,
    paste0("FIG_monthly_mean_profiles_1m_", mode_label, "_GAM_smoothed_months_", months_label, ".jpg")
  )
  
  grDevices::jpeg(
    filename = output_file,
    width = width,
    height = height,
    units = "in",
    res = dpi
  )
  
  old_par <- graphics::par(no.readonly = TRUE)
  
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  
  n_panels <- length(panel_specs)
  n_cols <- 2
  n_rows <- ceiling(n_panels / n_cols)
  layout_ids <- matrix(0L, nrow = n_rows, ncol = n_cols * 2)
  panel_id <- 1L
  for (r in seq_len(n_rows)) {
    for (c in seq_len(n_cols)) {
      if (panel_id <= n_panels) {
        layout_ids[r, (c - 1L) * 2L + 1L] <- (panel_id - 1L) * 2L + 1L
        layout_ids[r, (c - 1L) * 2L + 2L] <- (panel_id - 1L) * 2L + 2L
        panel_id <- panel_id + 1L
      }
    }
  }
  
  graphics::layout(
    layout_ids,
    widths = rep(c(5, 0.55), n_cols),
    heights = rep(1, n_rows)
  )
  
  for (spec in panel_specs) {
    panel_cols <- if (isTRUE(spec$reverse_palette)) rev(base_cols) else base_cols
    
    graphics::par(mar = c(4, 4.5, 3, 1))
    zlim <- plot_one_panel(x, spec$column, spec$title, panel_cols)
    
    graphics::par(mar = c(4, 0.5, 3, 3.5))
    plot_one_legend(zlim, spec$legend_title, panel_cols)
  }
  
  invisible(output_file)
}

smoothed_profile_figure <- plot_smoothed_profile_panels(
  data = smoothed_profiles,
  months_label = months_label,
  mode_label = mode_label,
  output_dir = outputPath,
  width = figure_width,
  height = figure_height,
  dpi = figure_dpi,
  palette = profile_colour_palette
)

message("Smoothed profile panel figure saved: ", smoothed_profile_figure)

