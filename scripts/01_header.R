# scripts/01_header.R
# This script prepares the session for this project (packages + helper functions).

if (!isTRUE(getOption("project_setup_done"))) source("scripts/02_setup.R")
# If setup has not run yet in this R session, run it now (install/load packages).

if (isTRUE(getOption("project_clean_workspace"))) {
  # This block runs only if we asked for a "clean start" (usually for run_project.R).
  
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # This removes everything from the global workspace (the Environment tab).
  
  if (!isTRUE(getOption("project_setup_done"))) source("scripts/02_setup.R")
  # After clearing, run setup again so packages are loaded.
}

# utils_dir <- "scripts/Utils"
# # This is the folder where your helper .R files live.
# 
# if (!dir.exists(utils_dir)) {
#   # This runs if the Utils folder does not exist.
#   
#   message("No Utils folder found. Skipping loading Utils/*.R files.")
#   # This prints a friendly message and continues.
#   
# } else {
#   # This runs if the Utils folder exists.
#   
#   utils_files <- list.files(utils_dir, pattern = "\\.R$", full.names = TRUE)
#   # This lists all files ending in .R inside Utils/ (full.names = TRUE keeps full paths).
#   
#   if (!exists("oxydebt_funs", envir = .GlobalEnv, inherits = FALSE)) {
#     # This checks whether oxydebt_funs already exists in the global workspace.
#     
#     oxydebt_funs <- new.env(parent = emptyenv())
#     # This creates a clean “container” (environment) to hold your helper functions.
#     
#     assign("oxydebt_funs", oxydebt_funs, envir = .GlobalEnv)
#     # This saves that container into the global workspace so other scripts can use it.
#   }
#   
#   oxydebt_funs <- get("oxydebt_funs", envir = .GlobalEnv)
#   # This retrieves the oxydebt_funs container (so we can load files into it).
#   
#   invisible(lapply(utils_files, sys.source, envir = oxydebt_funs))
#   # This loads each Utils/*.R file into oxydebt_funs.
#   # invisible(...) keeps output quieter.
# }

options(project_header_done = TRUE)
# This stores a simple note inside R: “header has run in this session.”