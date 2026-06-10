# Define paths
inputPath <- "Input/master"
outputPath <- assessment$output_dir

# Read HELCOM assessment unit data
helcom <- sf::st_read(
  inputPath,
  "HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro"
)

# Keep only open sea areas
helcom <- helcom[grepl("^SEA-", helcom$HELCOM_ID), ]

# Keep only the assessment basin
helcom <- helcom[trimws(helcom$Name) == trimws(assessment$basin), ]

if (nrow(helcom) == 0) {
  stop(
    "No HELCOM open sea area matched assessment$basin = '",
    assessment$basin,
    "'. Check spelling against helcom$Name."
  )
}

# Transform to UTM zone 34
helcom <- sf::st_transform(
  helcom,
  "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
)

# Clean names if needed
helcom$Name <- gsub("Å", "A", helcom$Name)

# Write out assessment area shapefile
sf::write_sf(
  helcom[c("Name", "F2_Name")],
  dsn = outputPath,
  layer = "oxy_areas",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

# Zip shapefile components
oxy_area_zip <- file.path(outputPath, "oxy_areas.zip")
unlink(oxy_area_zip)

oxy_area_files <- file.path(
  outputPath,
  dir(outputPath, pattern = "^oxy_areas\\.")
)

zip(oxy_area_zip, oxy_area_files)