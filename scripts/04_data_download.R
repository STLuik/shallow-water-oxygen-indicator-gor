# scripts/04_data_download.R
# This script downloads (or prepares access to) the "master" dataset and extra files used by the assessment.

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

message("Running data download step for period: ", assessment$period_label)
# This prints the current assessment period (example: 2005-2007).

master_dir <- file.path("Input", "master")
# This is a single place where big shared datasets live (download once, reuse for all periods).

dir.create(master_dir, showWarnings = FALSE, recursive = TRUE)
# This creates Input/master if it does not exist.

master_BOT <- file.path(master_dir, "BOT.csv")
# This is where the master bottle file would live (if you download it).

master_CTD <- file.path(master_dir, "CTD.csv")
# This is where the master CTD file would live (if you download it).

download_and_unzip_if_needed <- function(url, refetch = FALSE, path = ".") {
  # This defines a helper function that downloads a file and unzips it if it is a .zip.
  
  dest <- file.path(path, sub("\\?.+", "", basename(url)))
  # This builds the destination filename from the URL (and removes query text after ? if present).
  
  if (refetch || !file.exists(dest)) {
    # This runs if we want to re-download, or if the file is not already there.
    
    message("Downloading: ", url)
    # This prints which URL is being downloaded.
    
    download.file(url, dest, mode = "wb")
    # This downloads the file; mode="wb" is important on Windows to avoid corrupted binary files.
  } else {
    # This runs if the file already exists and we are not forcing a re-download.
    
    message("Already downloaded (skipping): ", dest)
    # This prints a message so the user knows we skipped downloading.
  }
  
  if (tools::file_ext(dest) == "zip") {
    # This checks if the downloaded file is a zip archive.
    
    message("Unzipping: ", dest)
    # This prints a message before unzipping.
    
    unzip(dest, exdir = path)
    # This extracts the zip contents into the target folder.
  }
  
  return(dest)
  # This returns the downloaded file path (useful for logging).
}

urls <- c(
  "https://icesoceanography.blob.core.windows.net/heat/OxygenDebt/BALTIC_BATHY_BALTSEM.zip"
)
# These are the URLs where the supporting files are downloaded from.

downloaded_files <- vapply(
  urls,
  download_and_unzip_if_needed,
  character(1),
  refetch = FALSE,
  path = master_dir
)
# This downloads each URL (if needed) into Input/master and unzips if it is a .zip.
# vapply(...) is like sapply(...), but a bit safer because we say the output is character.

baltsemBathymetricFile <- file.path(master_dir, "BALTIC_BATHY_BALTSEM.csv")
# This is the expected bathymetry CSV file.

unitsFile <- file.path(master_dir, "HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro.shp")
# This is the expected assessment unit shape file.

missing_expected <- c(
  baltsemBathymetricFile[!file.exists(baltsemBathymetricFile)]
)
# This checks whether the expected extracted files exist after download/unzip.

if (length(missing_expected) > 0) {
  # This runs if any expected file is still missing.
  
  stop("Some expected files were not found after download/unzip: ",
       paste(missing_expected, collapse = ", "))
  # This stops with a clear message, so you immediately know what is missing.
}

download_log <- file.path(assessment$output_dir, "01_data_download_log.txt")
# This is the log file for this assessment period (saved inside Output/years_XXXX_YYYY/).

writeLines(
  c(
    paste0("Assessment period: ", assessment$period_label),
    paste0("Master directory:  ", master_dir),
    paste0("Downloaded files:  ", paste(downloaded_files, collapse = ", ")),
    paste0("Bathymetry file:   ", baltsemBathymetricFile, " (exists=", file.exists(baltsemBathymetricFile), ")"),
    paste0("HELCOM units shapefile:   ", unitsFile, " (exists=", file.exists(unitsFile), ")"),
    paste0("BOT path (planned): ", master_BOT),
    paste0("CTD path (planned): ", master_CTD)
 ),
  con = download_log
)
# This writes a simple text log so you can see what happened, per assessment period.

message("Download step finished. Log saved to: ", download_log)
# This prints a friendly final message.


#----------------------------------------------------------
# Description of downloaded files and data:

# BALTIC_BATHY_BALTSEM.csv - Bathymetry data. 404263 X 7
# 404263 bathymetry points.
# Each point has x and y coordinates, and data on SEGMENT, BALTSEM_ID, Bo_Basin, dybyde (depth in m?), area

# HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro.shp - Assessment area shape file. Open sea basins used in eutrophication assessments.

# BOT.csv
# Bottle data, pre-downloaded subset of ICES data.

# CTD.csv
# CTD data, pre-downloaded subset of ICES data.

# AssessmentUnits.shp - HELCOM subbasin with coastal WFD waterbodies or watertypes 2022 level 4a (4b is the division used in the HOLAS3 eutrophication assessment, I guess just not for this indicator).
