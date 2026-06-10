# Define paths
inputPath <- "Input/master"
outputPath <- assessment$output_dir

# Read assessment area created by 05_make_assessment_area.R
helcom <- sf::st_read(
  dsn = outputPath,
  layer = "oxy_areas",
  quiet = TRUE
)

# Function to clean column names
cleanColumnNames <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII", sub = "")
  x <- gsub("\\[[^\\]]*\\]|\\:.*$|\\.", "", x, perl = TRUE)
  x <- gsub("[ ]+$", "", x)
  x <- gsub("[ ]+", "_", x)
  x
}

# Read raw depth points
bathy <- read.csv(file.path(inputPath, "BALTIC_BATHY_BALTSEM.csv"))

names(bathy) <- cleanColumnNames(names(bathy))

bathy <- dplyr::rename(bathy, depth = dybde)
bathy <- bathy[c("x", "y", "depth")]

# Convert to spatial points
bathy <- sf::st_as_sf(
  bathy,
  coords = c("x", "y"),
  crs = "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
)

# Make sure assessment area uses same CRS
helcom <- sf::st_transform(helcom, sf::st_crs(bathy))

# Trim bathymetry to assessment area
bathy_filtered <- bathy |>
  sf::st_filter(sf::st_union(helcom))

# Join points with assessment area attributes
bathy <- sf::st_join(
  bathy_filtered,
  helcom[, c("Name", "F2_Name")]
)

# Rename columns to be shapefile-safe
bathy_clean <- bathy
names(bathy_clean)[names(bathy_clean) == "Name"] <- "Basin"

# Write out bathymetry shapefile
sf::write_sf(
  bathy_clean[c("depth", "Basin")],
  dsn = outputPath,
  layer = "oxy_bathymetry",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

# Zip shapefile components
oxy_bathy_zip <- file.path(outputPath, "oxy_bathymetry.zip")
unlink(oxy_bathy_zip)

oxy_bathy_files <- file.path(
  outputPath,
  dir(outputPath, pattern = "^oxy_bathymetry\\.")
)

zip(oxy_bathy_zip, oxy_bathy_files)