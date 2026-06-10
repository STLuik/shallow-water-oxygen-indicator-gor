# scripts/02_setup.R
# This script installs (if needed) and loads the packages this project uses.

required_packages <- c(
  "sp",
  "sf",
  "dplyr",
  "ggplot2",
  "data.table",
  "R.utils",
  "lubridate",
  "gsw",
  "tidyr",
  "survival",
  "mgcv",
  "gstat",
  "strucchange"
)
# This is the list of packages the project needs.

required_packages <- unique(required_packages)
# This removes duplicates from the list.

base_packages <- c("stats")
# These come with R, so we do not install them.

required_packages <- setdiff(required_packages, base_packages)
# This removes base packages from the list of things to install/load.

is_installed <- vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
# This checks whether each package is installed (quietly = TRUE keeps output minimal).

missing_packages <- required_packages[!is_installed]
# This creates a list of packages that are missing.

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  # This prints which packages will be installed.
  
  install.packages(missing_packages)
  # This installs any missing packages from CRAN.
} else {
  message("All required packages are already installed.")
  # This prints a friendly message if nothing needs installing.
}

is_available <- vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
# This checks again after installation to confirm packages are now available.

if (!all(is_available)) {
  not_available <- required_packages[!is_available]
  # This lists packages that still aren’t available.
  
  stop("These packages could not be loaded after installation: ", paste(not_available, collapse = ", "))
  # This stops with a clear message so the user knows what to fix.
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, library, character.only = TRUE))
)
# This loads all packages but hides the usual “Attaching package…” startup messages.

message("Setup complete. Packages installed/loaded successfully: ",
        paste(required_packages, collapse = ", "))
# This prints a short “done” message and lists the packages.

options(project_setup_done = TRUE)
# This stores a simple note inside R: “setup has already run in this session.”