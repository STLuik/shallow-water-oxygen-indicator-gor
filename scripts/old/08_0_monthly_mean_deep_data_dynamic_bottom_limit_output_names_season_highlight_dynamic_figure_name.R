# scripts/08_0_monthly_mean_deep_data.R
# This script calculates monthly near-bottom means using a user-defined maximum distance from the bottom and creates supporting profile plots.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself during testing.

source("scripts/01_header.R")
# This loads packages (via setup.R if needed) and loads Utils functions into oxydebt_funs (if you have Utils/).

if (is.null(getOption("project_assessment"))) {
  # This checks whether the assessment settings (years + folders) have been defined.
  
  source("scripts/03_define_assessment.R")
  # If not defined, this sets the period and creates Input/Output folders for that period.
}

assessment <- getOption("project_assessment")
# This reads the assessment settings into a variable we can use in this script.

# Define paths
inputPath <- "Input/master"
outputPath <- assessment$output_dir

#-------------------------------------------------------------------------------
# USER SETTINGS
#
# Maximum allowed distance between the deepest measurement in a profile and the
# bathymetry depth at that position. For example, 1 means that the profile is
# treated as near-bottom only when the deepest measurement is max 1 m from bottom.
bottom_depth_limit_m <- 4

# Months to highlight in the monthly near-bottom oxygen figure and to use in the
# monthly profile panel figure. The month selection is also included in the
# near-bottom oxygen figure filename.
# Example: profile_plot_months <- 7:10 uses July-October.
# Example: profile_plot_months <- c(8, 9) uses August-September.
profile_plot_months <- 7:8

make_depth_limit_label <- function(x) {
  if (length(x) != 1 || is.na(x) || !is.finite(x) || x < 0) {
    stop("bottom_depth_limit_m must be one non-negative finite number.")
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
  months <- sort(unique(as.integer(months)))
  months <- months[!is.na(months) & months >= 1 & months <= 12]

  if (length(months) == 0) {
    return("selected")
  }

  paste(sprintf("%02d", months), collapse = "_")
}

make_months_text <- function(months) {
  months <- sort(unique(as.integer(months)))
  months <- months[!is.na(months) & months >= 1 & months <= 12]

  if (length(months) == 0) {
    return("selected months")
  }

  month_names <- month.name[months]

  if (length(months) == 1) {
    return(month_names)
  }

  if (all(diff(months) == 1)) {
    return(paste0(month_names[1], "-", month_names[length(month_names)]))
  }

  paste(month_names, collapse = ", ")
}

bottom_depth_limit_label <- make_depth_limit_label(bottom_depth_limit_m)
bottom_depth_limit_text <- make_depth_limit_text(bottom_depth_limit_m)
bottom_suffix <- paste0("bottom_", bottom_depth_limit_label, "m")
bottom_flag_col <- "deepest_within_bottom_limit_bathy"
profile_months_suffix <- paste0("months_", make_months_label(profile_plot_months))

# Output filenames that depend on the near-bottom depth limit and highlighted season.
monthly_bottom_means_filename <- paste0("monthly_bottom_means_", bottom_suffix, ".csv")
monthly_bottom_oxygen_figure_filename <- paste0(
  "FIG_Oxygen_mgl_",
  bottom_suffix,
  "_",
  profile_months_suffix,
  ".jpg"
)

# Remove unnecessary data/values/functions
keep <- c("assessment", "end_year", "start_year", "outputPath", "inputPath", "proj", "repo_url", "O2satFun","auxilliaryFile",
          "bottom_depth_limit_m", "bottom_depth_limit_label", "bottom_depth_limit_text", "bottom_suffix", "bottom_flag_col",
          "profile_plot_months", "profile_months_suffix",
          "monthly_bottom_means_filename", "monthly_bottom_oxygen_figure_filename",
          "make_depth_limit_label", "make_depth_limit_text", "make_months_label", "make_months_text")
# List the object names you want to keep (write yours here).

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
# Removes everything in the global environment EXCEPT the objects in keep.

# Read in data
oxy <- read.csv(file.path(outputPath, "oxy_clean.csv"))
# Makes sure both are data.tables:
setDT(oxy);

# Read depth layer created in 06_make_bathymetry_layer.R
bathy <- sf::read_sf(outputPath, "oxy_bathymetry")

# Convert oxy observations to spatial points
oxy_sf <- sf::st_as_sf(
  oxy,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

# Match CRS of bathymetry layer
oxy_sf <- sf::st_transform(oxy_sf, sf::st_crs(bathy))

# Find nearest bathymetry point for each observation
nearest_bathy_id <- sf::st_nearest_feature(oxy_sf, bathy)

# Add bathymetry depth to oxy data
oxy$Bathy_depth_m <- bathy$depth[nearest_bathy_id]

# Remove unnecessary columns
oxy[, c("F2_Name", "Name", "Basin2") := NULL]


#-------------------------------------------------------------------------------
# ADD NEGATIVE OXYGEN BASED ON MEASURED H2S
# 1 H2S = 2 O2 or 1 μmol L-1 H2S = - 0.04478 mL L-1 O2 (Fonselius, 1981). H2S is in umol l-1 (mg/L = µmol/L × 0.03408)
oxy$negat_DO_H2S_mll[!is.na(oxy$Hydrogen_Sulphide_umoll)] <- oxy$Hydrogen_Sulphide_umoll[!is.na(oxy$Hydrogen_Sulphide_umoll)] * - 0.04478

# Oxygen observations are in mg/l - convert ml/l to mg/l
oxy$negat_DO_H2S_mgl[!is.na(oxy$negat_DO_H2S_mll)] <- oxy$negat_DO_H2S_mll[!is.na(oxy$negat_DO_H2S_mll)] * 1.428 # or / 0.700

# Remove  O2$negat_DO_H2S_mgl values, where O2$Hydrogen_Sulphide_umoll <= 4 (According to Rolff et al., 2022: Negative oxygen was calculated only if H2S > 4 μmol L-1, since measurements below this level were considered uncertain.)
oxy$negat_DO_H2S_mgl[oxy$Hydrogen_Sulphide_umoll <= 4] <- NA

# If O2$Oxygen_mgl != NA & O2$Hydrogen_Sulphide_umoll <= 4, then oxygen values are used, but they are assumed to be zero.
z <- which(!is.na(oxy$Oxygen_mgl) & oxy$Hydrogen_Sulphide_umoll <= 4)

if (length(z) > 0) {
  oxy$Hydrogen_Sulphide_umoll[z] <- NA
  oxy$Oxygen_mgl[z] <- 0
}
#-------------------------------------------------------------------------------
# CALCULATE DO DEFICIT AGAIN
# For correction, oxygen debt (deficit) is calculated here again, according to the previous if function the oxygen values could change (If O2$Oxygen_mgl != NA & O2$Hydrogen_Sulphide_umoll <= 4, then oxygen values are used, but they are assumed to be zero.)
# Define function for calculating oxygen saturation concentration:
O2satFun <- function(temp) {
  tempabs <- temp + 273.15
  exp(-173.4292 + 249.6339 * (100/tempabs) +
        143.3483 * log(tempabs/100) - 21.8492 * (tempabs/100) +
        (-0.033096 + 0.014259 * (tempabs/100) - 0.0017000 * (tempabs/100)^2)
  ) * 1.428  # * Oxygen saturation in mg/l
}
# Compute oxygen deficit
oxy$Oxygen_debt_mgl <- O2satFun(oxy$Temperature_degreesC) - oxy$Oxygen_mgl

#------------------------------------------------------------------------------
# ADD NEGATIVE OXYGEN BASED ON MEASURED NH4
# Convert NH4 µmol/L to mg/l
oxy$Ammonium_Nitrogen_mgl[!is.na(oxy$Ammonium_Nitrogen_umoll)] <- oxy$Ammonium_Nitrogen_umoll[!is.na(oxy$Ammonium_Nitrogen_umoll)] * (14.0067 / 1000)

# Calculate negative oxygen according to Rolff et al., 2022
oxy$negat_DO_NH4_mgl[!is.na(oxy$Ammonium_Nitrogen_mgl)] <- oxy$Ammonium_Nitrogen_mgl[!is.na(oxy$Ammonium_Nitrogen_mgl)] * -4.57

# O2$negat_DO_NH4 values that are above 65m (Rolff et al., 2022) are not considered
oxy$negat_DO_NH4_mgl[oxy$Depth_m < 65] <- NA
#-------------------------------------------------------------------------------
# Create new DO debt variables:
# Populate DO deficit H2S var with DO deficit values
oxy$Oxygen_debt_mgl_H2S <- oxy$Oxygen_debt_mgl
# Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
oxy$Oxygen_debt_mgl_H2S[!is.na(oxy$negat_DO_H2S_mgl)] <- oxy$Oxygen_debt_mgl[!is.na(oxy$negat_DO_H2S_mgl)] + oxy$negat_DO_H2S_mgl[!is.na(oxy$negat_DO_H2S_mgl)] * -1


# Populate DO deficit NH4 var with DO deficit values
oxy$Oxygen_debt_mgl_NH4 <- oxy$Oxygen_debt_mgl
# Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
oxy$Oxygen_debt_mgl_NH4[!is.na(oxy$negat_DO_NH4_mgl)] <- oxy$Oxygen_debt_mgl[!is.na(oxy$negat_DO_NH4_mgl)] + oxy$negat_DO_NH4_mgl[!is.na(oxy$negat_DO_NH4_mgl)] * -1


# Populate DO deficit H2S+NH4 var with DO deficit values
oxy$Oxygen_debt_mgl_H2S_NH4 <- oxy$Oxygen_debt_mgl_H2S
# Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
oxy$Oxygen_debt_mgl_H2S_NH4[!is.na(oxy$negat_DO_NH4_mgl)] <- oxy$Oxygen_debt_mgl_H2S_NH4[!is.na(oxy$negat_DO_NH4_mgl)] + oxy$negat_DO_NH4_mgl[!is.na(oxy$negat_DO_NH4_mgl)] * -1

#-------------------------------------------------------------------------------
# CALCULATE TEOS-10 DENSITY VARIABLES
# These are added here so that later profile-based scripts can use the same
# density values as the monthly bottom means and monthly mean profiles.
#
# Density_kgm3 is in-situ density at the observation pressure.
# Sigma0_kgm3 is potential density anomaly referenced to 0 dbar.
# For stratification work, Sigma0_kgm3 is usually the more useful variable,
# because it removes the direct pressure-compression effect.

if (!requireNamespace("gsw", quietly = TRUE)) {
  stop(
    "Package 'gsw' is needed for TEOS-10 density calculations. ",
    "Add it to scripts/02_setup.R or install it with install.packages('gsw')."
  )
}

required_density_cols <- c(
  "Salinity_psu",
  "Temperature_degreesC",
  "Depth_m",
  "Longitude",
  "Latitude"
)

if (!all(required_density_cols %in% names(oxy))) {
  stop(
    "Missing columns needed for density calculation: ",
    paste(setdiff(required_density_cols, names(oxy)), collapse = ", ")
  )
}

oxy[, Pressure_dbar := NA_real_]
oxy[, Absolute_Salinity_gkg := NA_real_]
oxy[, Conservative_Temperature_degreesC := NA_real_]
oxy[, Density_kgm3 := NA_real_]
oxy[, Sigma0_kgm3 := NA_real_]

density_rows <- which(
  !is.na(oxy$Salinity_psu) &
    !is.na(oxy$Temperature_degreesC) &
    !is.na(oxy$Depth_m) &
    !is.na(oxy$Longitude) &
    !is.na(oxy$Latitude)
)

if (length(density_rows) > 0) {
  oxy$Pressure_dbar[density_rows] <- gsw::gsw_p_from_z(
    -abs(oxy$Depth_m[density_rows]),
    oxy$Latitude[density_rows]
  )
  
  oxy$Absolute_Salinity_gkg[density_rows] <- gsw::gsw_SA_from_SP(
    oxy$Salinity_psu[density_rows],
    oxy$Pressure_dbar[density_rows],
    oxy$Longitude[density_rows],
    oxy$Latitude[density_rows]
  )
  
  oxy$Conservative_Temperature_degreesC[density_rows] <- gsw::gsw_CT_from_t(
    oxy$Absolute_Salinity_gkg[density_rows],
    oxy$Temperature_degreesC[density_rows],
    oxy$Pressure_dbar[density_rows]
  )
  
  oxy$Density_kgm3[density_rows] <- gsw::gsw_rho(
    oxy$Absolute_Salinity_gkg[density_rows],
    oxy$Conservative_Temperature_degreesC[density_rows],
    oxy$Pressure_dbar[density_rows]
  )
  
  oxy$Sigma0_kgm3[density_rows] <- gsw::gsw_sigma0(
    oxy$Absolute_Salinity_gkg[density_rows],
    oxy$Conservative_Temperature_degreesC[density_rows]
  )
}

message("TEOS-10 density calculated for ", length(density_rows), " rows.")

#-------------------------------------------------------------------------------
# Create columns where entire profiles are marked based on the following conditions:
# 1. Deepest measurement (oxy$max_depth_m) is max bottom_depth_limit_m from
#    bathymetry depth (oxy$Bathy_depth_m).
oxy[, (bottom_flag_col) :=
      !is.na(max_depth_m) &
      !is.na(Bathy_depth_m) &
      (Bathy_depth_m - max_depth_m) <= bottom_depth_limit_m]

# 2. There are at least 3 measurements per profile (oxy$n_Oxygen >= 3)
oxy[, at_least_3_oxygen_measurements := 
      !is.na(n_Oxygen) &
      n_Oxygen >= 3]


#-------------------------------------------------------------------------------
# Define parameter columns to average
# Replace / adjust these names according to your oxy column names
parameters <- c(
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

parameters <- intersect(parameters, names(oxy))

if (length(parameters) == 0) {
  stop("None of the selected parameter columns were found in oxy.")
}

# Make sure Year, Month, Day are numeric
oxy[, Year := as.integer(Year)]
oxy[, Month := as.integer(Month)]
oxy[, Day := as.integer(Day)]


# Create complete year-month frame
monthly_all <- CJ(
  Year = assessment$start_year:assessment$end_year,
  Month = 1:12
)

# Function for bottom monthly means
make_monthly_bottom_means <- function(data, bottom_flag, suffix) {
  
  safe_mean <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(mean(x, na.rm = TRUE))
    }
  }
  
  # Keep only profiles meeting the bottom criterion
  x <- data[get(bottom_flag) == TRUE]
  
  # Monthly profile/day counts based on unique profiles
  monthly_counts <- unique(
    data[get(bottom_flag) == TRUE],
    by = "ID"
  )[
    ,
    .(
      n_profiles = data.table::uniqueN(ID),
      n_days = data.table::uniqueN(as.Date(paste(Year, Month, Day, sep = "-"))),
      n_profiles_3plus_measurements = data.table::uniqueN(
        ID[at_least_3_oxygen_measurements == TRUE]
      )
    ),
    by = .(Year, Month)
  ]
  
  setnames(
    monthly_counts,
    old = c("n_profiles", "n_days", "n_profiles_3plus_measurements"),
    new = paste0(
      c("n_profiles_", "n_days_", "n_profiles_3plus_measurements_"),
      suffix
    )
  )
  
  # Keep only deepest measurement(s) per profile for averaging
  x <- x[Depth_m == max_depth_m]
  
  # 1. Mean by profile
  by_profile <- x[
    ,
    c(
      .(
        Year = first(Year),
        Month = first(Month),
        Day = first(Day)
      ),
      lapply(.SD, safe_mean)
    ),
    by = ID,
    .SDcols = parameters
  ]
  
  # 2. Mean by day
  by_day <- by_profile[
    ,
    lapply(.SD, safe_mean),
    by = .(Year, Month, Day),
    .SDcols = parameters
  ]
  
  # 3. Mean by month
  by_month <- by_day[
    ,
    lapply(.SD, safe_mean),
    by = .(Year, Month),
    .SDcols = parameters
  ]
  
  # Rename parameter columns
  setnames(
    by_month,
    old = parameters,
    new = paste0(parameters, "_", suffix)
  )
  
  # Add missing year-month combinations
  out <- merge(
    monthly_all,
    by_month,
    by = c("Year", "Month"),
    all.x = TRUE
  )
  
  # Add monthly counts
  out <- merge(
    out,
    monthly_counts,
    by = c("Year", "Month"),
    all.x = TRUE
  )
  
  # Replace missing counts with 0
  count_cols <- paste0(
    c("n_profiles_", "n_days_", "n_profiles_3plus_measurements_"),
    suffix
  )
  
  out[
    ,
    (count_cols) := lapply(.SD, function(z) {
      data.table::fifelse(is.na(z), 0L, as.integer(z))
    }),
    .SDcols = count_cols
  ]
  
  out
}

monthly_bottom <- make_monthly_bottom_means(
  data = oxy,
  bottom_flag = bottom_flag_col,
  suffix = bottom_suffix
)

monthly_bottom_means <- monthly_bottom


# Save
data.table::fwrite(
  monthly_bottom_means,
  file.path(outputPath, monthly_bottom_means_filename)
)


#-------------------------------------------------------------------------------
# CREATE MONTHLY MEAN PROFILES AT 1 M DEPTH STEPS
#
# Depth bin logic:
# depth = 5 m includes measurements where Depth_m >= 4.5 and Depth_m < 5.5.

make_monthly_mean_profiles_1m <- function(data, parameters) {
  
  safe_mean <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(mean(x, na.rm = TRUE))
    }
  }
  
  x <- data.table::copy(data)
  x <- x[!is.na(Depth_m)]
  
  if (nrow(x) == 0) {
    stop("No rows with non-missing Depth_m were found for monthly profile averaging.")
  }
  
  # Assign each measurement to the nearest 1 m depth step.
  # Example: 4.5 <= Depth_m < 5.5 is assigned to Depth_m = 5.
  x[, Depth_bin_m := floor(Depth_m + 0.5)]
  
  # Complete year-month-depth grid.
  depth_bins <- seq(
    from = 0L,
    to = max(x$Depth_bin_m, na.rm = TRUE),
    by = 1L
  )
  
  monthly_profile_all <- data.table::CJ(
    Year = assessment$start_year:assessment$end_year,
    Month = 1:12,
    Depth_m = depth_bins
  )
  
  # Monthly counts before averaging.
  monthly_counts <- x[
    ,
    .(
      n_measurements = .N,
      n_profiles = data.table::uniqueN(ID),
      n_days = data.table::uniqueN(as.Date(paste(Year, Month, Day, sep = "-")))
    ),
    by = .(Year, Month, Depth_bin_m)
  ]
  
  data.table::setnames(monthly_counts, "Depth_bin_m", "Depth_m")
  
  # 1. Mean by profile and 1 m depth bin.
  by_profile <- x[
    ,
    c(
      .(
        Year = first(Year),
        Month = first(Month),
        Day = first(Day),
        Depth_m = first(Depth_bin_m)
      ),
      lapply(.SD, safe_mean)
    ),
    by = .(ID, Depth_bin_m),
    .SDcols = parameters
  ]
  
  # 2. Mean by day and 1 m depth bin.
  by_day <- by_profile[
    ,
    lapply(.SD, safe_mean),
    by = .(Year, Month, Day, Depth_m),
    .SDcols = parameters
  ]
  
  # 3. Mean by month and 1 m depth bin.
  by_month <- by_day[
    ,
    lapply(.SD, safe_mean),
    by = .(Year, Month, Depth_m),
    .SDcols = parameters
  ]
  
  # Add missing year-month-depth combinations.
  out <- merge(
    monthly_profile_all,
    by_month,
    by = c("Year", "Month", "Depth_m"),
    all.x = TRUE
  )
  
  # Add counts.
  out <- merge(
    out,
    monthly_counts,
    by = c("Year", "Month", "Depth_m"),
    all.x = TRUE
  )
  
  count_cols <- c("n_measurements", "n_profiles", "n_days")
  
  out[
    ,
    (count_cols) := lapply(.SD, function(z) {
      data.table::fifelse(is.na(z), 0L, as.integer(z))
    }),
    .SDcols = count_cols
  ]
  
  out
}

monthly_mean_profiles_1m <- make_monthly_mean_profiles_1m(
  data = oxy,
  parameters = parameters
)

# Save monthly mean profiles.
data.table::fwrite(
  monthly_mean_profiles_1m,
  file.path(outputPath, "monthly_mean_profiles_1m.csv")
)


#-------------------------------------------------------------------------------
# FIGURE: MONTHLY MEAN PROFILES, 4 PANELS
#
# The months used here are controlled by profile_plot_months in USER SETTINGS.

make_profile_palette <- function(palette = "parula", n = 100) {
  palette <- tolower(palette)
  
  if (palette == "parula") {
    return(grDevices::colorRampPalette(
      c("#352A87", "#1464D2", "#06A7C6", "#38B977", "#B6C84B", "#F9FB0E")
    )(n))
  }
  
  if (palette == "jet") {
    return(grDevices::colorRampPalette(
      c("#00007F", "#0000FF", "#007FFF", "#00FFFF", "#7FFF7F",
        "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")
    )(n))
  }
  
  if (palette == "salinity") {
    return(grDevices::colorRampPalette(
      c("#F7FCF0", "#C7E9B4", "#7FCDBB", "#41B6C4",
        "#1D91C0", "#225EA8", "#0C2C84")
    )(n))
  }
  
  stop("Unknown profile colour palette: ", palette)
}

# Main palette used by oxygen, oxygen debt, and temperature panels.
profile_colour_palette <- "jet"

# Separate palette for salinity, using a sequential blue-green scale.
profile_salinity_colour_palette <- "jet"

# Fixed salinity colour scale for the salinity profile panel.
# Edit these values if another range is needed, e.g. c(3, 8).
profile_salinity_colour_limits <- c(4, 7)

plot_monthly_mean_profile_panels <- function(data,
                                             months = 7:9,
                                             output_dir,
                                             width = 14,
                                             height = 8,
                                             dpi = 300,
                                             max_depth = NULL) {
  
  required_cols <- c(
    "Year", "Month", "Depth_m",
    "Oxygen_mgl",
    "Oxygen_debt_mgl_H2S_NH4",
    "Temperature_degreesC",
    "Salinity_psu"
  )
  
  if (!all(required_cols %in% names(data))) {
    stop(
      "Missing required columns for profile panel figure: ",
      paste(setdiff(required_cols, names(data)), collapse = ", ")
    )
  }
  
  x <- data.table::copy(data)
  x[, Year := as.integer(Year)]
  x[, Month := as.integer(Month)]
  x[, Depth_m := as.numeric(Depth_m)]
  
  months <- sort(unique(as.integer(months)))
  x <- x[Month %in% months]
  
  if (nrow(x) == 0) {
    stop("No monthly mean profile data found for the selected months.")
  }
  
  if (!is.null(max_depth)) {
    x <- x[Depth_m <= max_depth]
  }
  
  x[, Date := as.Date(paste0(Year, "-", sprintf("%02d", Month), "-01"))]
  data.table::setorder(x, Date, Depth_m)
  
  # Use one shared x-axis definition for all profile panels so that the
  # parameter panels are directly comparable along the time axis.
  profile_x_limits <- range(x$Date, na.rm = TRUE)
  profile_x_ticks <- pretty(profile_x_limits, n = 6)
  profile_x_ticks <- profile_x_ticks[
    profile_x_ticks >= profile_x_limits[1] &
      profile_x_ticks <= profile_x_limits[2]
  ]
  
  panel_specs <- list(
    list(
      column = "Oxygen_mgl",
      title = "Oxygen",
      legend_title = "mg/l",
      palette = profile_colour_palette,
      reverse_palette = TRUE,
      colour_limits = NULL
    ),
    list(
      column = "Oxygen_debt_mgl_H2S_NH4",
      title = "Oxygen deficiency, H2S",
      legend_title = "mg/l",
      palette = profile_colour_palette,
      reverse_palette = FALSE,
      colour_limits = NULL
    ),
    list(
      column = "Temperature_degreesC",
      title = "Temperature",
      legend_title = "degrees C",
      palette = profile_colour_palette,
      reverse_palette = FALSE,
      colour_limits = NULL
    ),
    list(
      column = "Salinity_psu",
      title = "Salinity",
      legend_title = "psu",
      palette = profile_salinity_colour_palette,
      reverse_palette = FALSE,
      colour_limits = profile_salinity_colour_limits
    )
  )
  
  plot_one_panel <- function(panel_data,
                             value_col,
                             panel_title,
                             cols,
                             colour_limits = NULL,
                             x_limits,
                             x_ticks) {
    
    z_data <- panel_data[!is.na(get(value_col))]
    
    if (nrow(z_data) == 0) {
      plot(
        x = as.numeric(x_limits),
        y = c(0, 1),
        type = "n",
        xlab = "Date",
        ylab = "Depth (m)",
        main = panel_title,
        xlim = as.numeric(x_limits),
        axes = FALSE
      )
      axis(
        side = 1,
        at = as.numeric(x_ticks),
        labels = format(x_ticks, "%Y"),
        las = 1
      )
      axis(side = 2, las = 1)
      box()
      text(mean(as.numeric(x_limits)), 0.5, "No data")
      return(invisible(NULL))
    }
    
    wide <- data.table::dcast(
      z_data,
      Depth_m ~ Date,
      value.var = value_col
    )
    
    date_cols <- setdiff(names(wide), "Depth_m")
    dates <- as.Date(date_cols)
    depths <- wide$Depth_m
    z <- as.matrix(wide[, ..date_cols])
    
    if (length(dates) < 2 || length(depths) < 2) {
      plot(
        x = as.numeric(dates),
        y = depths,
        xlab = "Date",
        ylab = "Depth (m)",
        main = panel_title,
        xlim = as.numeric(x_limits),
        ylim = rev(range(depths, na.rm = TRUE)),
        axes = FALSE
      )
      axis(
        side = 1,
        at = as.numeric(x_ticks),
        labels = format(x_ticks, "%Y"),
        las = 1
      )
      axis(side = 2, las = 1)
      box()
      return(invisible(NULL))
    }
    
    if (!is.null(colour_limits)) {
      zlim <- as.numeric(colour_limits)
      if (length(zlim) != 2 || !all(is.finite(zlim)) || zlim[1] >= zlim[2]) {
        stop("colour_limits must be NULL or a numeric vector of length 2: c(min, max).")
      }
    } else {
      zlim <- range(z, na.rm = TRUE)
      
      if (!all(is.finite(zlim))) {
        zlim <- c(0, 1)
      }
      
      if (zlim[1] == zlim[2]) {
        zlim <- zlim + c(-0.5, 0.5)
      }
    }
    
    graphics::image(
      x = as.numeric(dates),
      y = depths,
      z = t(z),
      col = cols,
      zlim = zlim,
      xlab = "Date",
      ylab = "Depth (m)",
      main = panel_title,
      xlim = as.numeric(x_limits),
      ylim = rev(range(depths, na.rm = TRUE)),
      axes = FALSE
    )
    
    axis(
      side = 1,
      at = as.numeric(x_ticks),
      labels = format(x_ticks, "%Y"),
      las = 1
    )
    axis(side = 2, las = 1)
    box()
    
    invisible(zlim)
  }
  
  plot_one_legend <- function(zlim, legend_title, cols) {
    
    if (is.null(zlim) || !all(is.finite(zlim))) {
      plot.new()
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
    axis(side = 4, las = 1, cex.axis = 0.8)
    mtext(legend_title, side = 3, line = 0.5, cex = 0.8)
    
    invisible(NULL)
  }
  
  months_label <- make_months_label(months)
  output_file <- file.path(
    output_dir,
    paste0("FIG_monthly_mean_profiles_1m_months_", months_label, ".jpg")
  )
  
  grDevices::jpeg(
    filename = output_file,
    width = width,
    height = height,
    units = "in",
    res = dpi
  )
  
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  
  layout(
    matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE),
    widths = c(5, 0.55, 5, 0.55),
    heights = c(1, 1)
  )
  
  for (spec in panel_specs) {
    panel_cols <- make_profile_palette(palette = spec$palette, n = 100)
    panel_cols <- if (isTRUE(spec$reverse_palette)) rev(panel_cols) else panel_cols
    
    par(mar = c(4, 4.5, 3, 1))
    zlim <- plot_one_panel(
      panel_data = x,
      value_col = spec$column,
      panel_title = spec$title,
      cols = panel_cols,
      colour_limits = spec$colour_limits,
      x_limits = profile_x_limits,
      x_ticks = profile_x_ticks
    )
    par(mar = c(4, 0.5, 3, 3.5))
    plot_one_legend(zlim, spec$legend_title, panel_cols)
  }
  
  invisible(output_file)
}

monthly_profile_panel_figure <- plot_monthly_mean_profile_panels(
  data = monthly_mean_profiles_1m,
  months = profile_plot_months,
  output_dir = outputPath
)

message("Monthly mean profile panel figure saved: ", monthly_profile_panel_figure)



# Function for figure
plot_monthly_scatter <- function(data,
                                 year_col = "Year",
                                 month_col = "Month",
                                 y_col = "Oxygen_mgl",
                                 y_limits = NULL,
                                 plot_title = NULL,
                                 plot_subtitle = NULL,
                                 season_months = NULL,
                                 units = "mg/l",
                                 output_dir,
                                 output_filename = NULL,
                                 width = 12,
                                 height = 8,
                                 dpi = 300) {
  
  # Check required columns
  required_cols <- c(year_col, month_col, y_col)
  
  if (!all(required_cols %in% names(data))) {
    stop(
      "Missing required columns: ",
      paste(setdiff(required_cols, names(data)), collapse = ", ")
    )
  }
  
  # Copy data
  x <- data.table::copy(data)
  
  # Create date from year and month
  x[, plot_date := as.Date(
    paste0(get(year_col), "-", sprintf("%02d", get(month_col)), "-01")
  )]

  if (is.null(season_months)) {
    season_months <- sort(unique(x[[month_col]]))
  }

  season_months <- sort(unique(as.integer(season_months)))
  season_months <- season_months[!is.na(season_months) & season_months >= 1 & season_months <= 12]
  x[, is_season_month := get(month_col) %in% season_months]

  if (is.null(plot_subtitle)) {
    plot_subtitle <- paste0(
      "Filled black points = ", make_months_text(season_months),
      "; gray-outline points = other months"
    )
  }
  
  # Draw plot. All months are shown; selected-season months are highlighted.
  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(x = plot_date, y = .data[[y_col]])
  ) +
    ggplot2::geom_point(
      data = x[is_season_month == FALSE],
      shape = 21,
      fill = "white",
      colour = "gray60",
      stroke = 0.8,
      size = 2.2
    ) +
    ggplot2::geom_point(
      data = x[is_season_month == TRUE],
      shape = 21,
      fill = "black",
      colour = "black",
      stroke = 0.4,
      size = 2.2
    ) +
    ggplot2::labs(
      x = "Date",
      y = paste0(y_col, " (", units, ")"),
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    ggplot2::theme_bw()
  
  # Add y limits if supplied
  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits)
  }
  
  # Save plot using a dynamic filename when supplied.
  if (is.null(output_filename)) {
    output_filename <- paste0("FIG_", y_col, ".jpg")
  }

  plot_file <- file.path(
    output_dir,
    output_filename
  )
  
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )
  
  return(p)
}

# Write out monthly data
data.table::fwrite(
  monthly_bottom_means,
  file.path(outputPath, monthly_bottom_means_filename)
)

# Plot figures using the function above
plot_monthly_scatter(
  data = monthly_bottom_means,
  y_col = paste0("Oxygen_mgl_", bottom_suffix),
  y_limits = c(0, 15),
  plot_title = paste0(
    "Monthly mean near-bottom oxygen, max ",
    bottom_depth_limit_text,
    " from bottom"
  ),
  plot_subtitle = paste0(
    "Filled black points = ",
    make_months_text(profile_plot_months),
    "; gray-outline points = other months"
  ),
  season_months = profile_plot_months,
  units = "mg/l",
  output_dir = outputPath,
  output_filename = monthly_bottom_oxygen_figure_filename
)
