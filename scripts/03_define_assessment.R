# scripts/03_define_assessment.R
# This script defines the assessment period (time subset) and creates the matching folders.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself.

if (!isTRUE(getOption("project_header_done"))) source("scripts/01_header.R")
# This loads setup.R + header.R (packages + Utils) if it has not been run yet.

start_year <- 1900
# This is the first year included in the assessment period.

end_year <- 2025
# This is the last year included in the assessment period.

assessment_period <- paste0(start_year, "-", end_year)
# This creates a human-readable period label like "2005-2007".

subset_id <- paste0("years_", start_year, "_", end_year)
# This creates the folder name like "years_2005_2007".

input_root <- "Input"
# This is the main folder that will contain input data (including subsets).

output_root <- "Output"
# This is the main folder that will contain outputs for each assessment run.

dir.create(input_root, showWarnings = FALSE, recursive = TRUE)
# This creates the Input folder if it does not exist.

dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
# This creates the Output folder if it does not exist.

input_subset_dir <- file.path(input_root, subset_id)
# This builds the path "Input/years_2005_2007" (portable across operating systems).

output_subset_dir <- file.path(output_root, subset_id)
# This builds the path "Output/years_2005_2007".

dir.create(input_subset_dir, showWarnings = FALSE, recursive = TRUE)
# This creates the subset input folder.

dir.create(output_subset_dir, showWarnings = FALSE, recursive = TRUE)
# This creates the subset output folder.

assessment <- list(
  topic = "SWOI_GOR",
  # This stores what you are assessing (oxygen conditions in what area as an abbreviation).
  
  basin = "Opensea Gulf of Riga",
  # Assessment area name according to HELCOM 4b divsion (used in eutrophiaton assessment)
  
  start_year = start_year,
  # This stores the numeric start year.
  
  end_year = end_year,
  # This stores the numeric end year.
  
  period_label = assessment_period,
  # This stores the label e.g., "2005-2007".
  
  subset_id = subset_id,
  # This stores the folder-friendly id, e.g., "years_2005_2007".
  
  input_dir = input_subset_dir,
  # This stores the input subset folder path.
  
  output_dir = output_subset_dir
  # This stores the output subset folder path.
)
# This collects all key settings for the run into one object.

options(project_assessment = assessment)
# This stores the assessment settings so any script can access them using getOption("project_assessment").

message("Assessment defined: ", assessment$topic, " (", assessment$basin, "), period ", assessment$period_label)
# This prints a friendly summary of what you just defined.

message("Input folder:  ", assessment$input_dir)
# This prints where the subset input folder is.

message("Output folder: ", assessment$output_dir)
# This prints where results for this period should be written.