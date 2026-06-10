# scripts/07_data_preparation.R
# This script prepares raw data for analysis. Removes duplicates etc.

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

# Load data. This is the entire master data set.
master_ctd <- fread(input = file.path(inputPath, "CTD.csv"), sep = ",", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
master_bot <- fread(input = file.path(inputPath, "BOT.csv"), sep = ",", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)


########### ADD DATETIME VARIABLES AND GET ONLY ASSESSMENT PERIOD DATA
# CTD data:
# CTD column names:
# [1] "Cruise"                                      "Station"                                    
# [3] "Type"                                        "yyyy.mm.ddThh.mm.ss.sss"                    
# [5] "Longitude..degrees_east."                    "Latitude..degrees_north."                   
# [7] "Bot..Depth..m."                              "Secchi.Depth..m."                           
# [9] "Device.Category.Code..L05..METAVAR.TEXT"     "Platform.Code..C17..METAVAR.TEXT"           
# [11] "Custodian.Code..EDMO..METAVAR.TEXT"          "Originator.Code..EDMO..METAVAR.TEXT"        
# [13] "Distributor.Code..EDMO..METAVAR.TEXT"        "Project.Code..EDMERP..METAVAR.TEXT"         
# [15] "Modified.METAVAR.TEXT"                       "GUID.METAVAR.TEXT"                          
# [17] "Depth..ADEPZZ01_ULAA...m."                   "QV.ODV.Depth..ADEPZZ01_ULAA...m."           
# [19] "Temperature..TEMPPR01_UPAA...degC."          "QV.ODV.Temperature..TEMPPR01_UPAA...degC."  
# [21] "Salinity..PSALPR01_UUUU...dmnless."          "QV.ODV.Salinity..PSALPR01_UUUU...dmnless."  
# [23] "Oxygen..DOXYZZXX_UMLL...ml.l."               "QV.ODV.Oxygen..DOXYZZXX_UMLL...ml.l."       
# [25] "pH..PHXXZZXX_UUPH...pH.units."               "QV.ODV.pH..PHXXZZXX_UUPH...pH.units."       
# [27] "Chlorophyll.a..CPHLZZXX_UGPL...ug.l."        "QV.ODV.Chlorophyll.a..CPHLZZXX_UGPL...ug.l."
# [29] "Conductivity..CNDCZZ01_UECA...S.m."          "QV.ODV.Conductivity..CNDCZZ01_UECA...S.m."  

# Create date variables (this is done because sometimes the date in the original data is not as described - yyyy.mm.ddThh.mm.ss.sss; sometimes e.g., time is missing):
Z <- master_ctd[,.(yyyy.mm.ddThh.mm.ss.sss)]

# Convert the datetime values to POSIXct format:
Z[, datetime := as.POSIXct(yyyy.mm.ddThh.mm.ss.sss, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")]

# Fill NA for dates without time component:
Z[is.na(datetime), datetime := as.POSIXct(yyyy.mm.ddThh.mm.ss.sss, format = "%Y-%m-%d", tz = "UTC")]

# Extract year, month, day, hour, and minute:
Z[, `:=`(
  Year = format(datetime, "%Y"),
  Month = format(datetime, "%m"),
  Day = format(datetime, "%d"),
  Hour = format(datetime, "%H"),
  Minute = format(datetime, "%M")
)]

# Adding Year, Month, and Day from Z to bot
master_ctd[, `:=`(Year = Z$Year,
           Month = Z$Month,
           Day = Z$Day,
           Hour = Z$Hour,
           Minute = Z$Minute)]

# This part keeps only the rows where quality flags (likely from ODV = Ocean Data View) are less than or equal to 1, which usually means good or acceptable quality.
# Also, only the assessment period data is selected here:
ctd <- master_ctd[Year >= assessment$start_year & Year <= assessment$end_year &
             QV.ODV.Depth..ADEPZZ01_ULAA...m. <= 1 & 
             QV.ODV.Temperature..TEMPPR01_UPAA...degC. <=1 & 
             QV.ODV.Salinity..PSALPR01_UUUU...dmnless. <=1 & 
             QV.ODV.Oxygen..DOXYZZXX_UMLL...ml.l. <= 1,.
           (Cruise, Year, Month, Day, Hour, Minute, 
             Latitude = Latitude..degrees_north., 
             Longitude = Longitude..degrees_east., 
             Depth_m = Depth..ADEPZZ01_ULAA...m., 
             Temperature_degreesC = Temperature..TEMPPR01_UPAA...degC., 
             Salinity_psu = Salinity..PSALPR01_UUUU...dmnless., 
             Oxygen_mll = Oxygen..DOXYZZXX_UMLL...ml.l.)]

# Remove master_ctd
rm(master_ctd, inherits = TRUE)


# BOT data:
# BOT  column names:	
# [1] "Cruise"                                                       	
# [2] "Station"                                                      	
# [3] "Type"                                                         	
# [4] "yyyy.mm.ddThh.mm.ss.sss"                                      	
# [5] "Longitude..degrees_east."                                     	
# [6] "Latitude..degrees_north."                                     	
# [7] "Bot..Depth..m."                                               	
# [8] "Secchi.Depth..m."                                             	
# [9] "Device.Category.Code..L05..METAVAR.TEXT"                      	
# [10] "Platform.Code..C17..METAVAR.TEXT"                             	
# [11] "Custodian.Code..EDMO..METAVAR.TEXT"                           	
# [12] "Originator.Code..EDMO..METAVAR.TEXT"                          	
# [13] "Distributor.Code..EDMO..METAVAR.TEXT"                         	
# [14] "Project.Code..EDMERP..METAVAR.TEXT"                           	
# [15] "Modified.METAVAR.TEXT"                                        	
# [16] "GUID.METAVAR.TEXT"                                            	
# [17] "Depth..ADEPZZ01_ULAA...m."                                    	# [18] "QV.ODV.Depth..ADEPZZ01_ULAA...m."                             
# [19] "Temperature..TEMPPR01_UPAA...degC."                           	# [20] "QV.ODV.Temperature..TEMPPR01_UPAA...degC."                    
# [21] "Salinity..PSALPR01_UUUU...dmnless."                           	# [22] "QV.ODV.Salinity..PSALPR01_UUUU...dmnless."                    
# [23] "Oxygen..DOXYZZXX_UMLL...ml.l."                                	# [24] "QV.ODV.Oxygen..DOXYZZXX_UMLL...ml.l."                         
# [25] "Phosphate..PHOSZZXX_UPOX...umol.l."                           	# [26] "QV.ODV.Phosphate..PHOSZZXX_UPOX...umol.l."                    
# [27] "Total.Phosphorus..TPHSZZXX_UPOX...umol.l."                    	# [28] "QV.ODV.Total.Phosphorus..TPHSZZXX_UPOX...umol.l."             
# [29] "Silicate..SLCAZZXX_UPOX...umol.l."                            	# [30] "QV.ODV.Silicate..SLCAZZXX_UPOX...umol.l."                     
# [31] "Nitrate...Nitrite..NTRZZZXX_UPOX...umol.l."                   	# [32] "QV.ODV.Nitrate...Nitrite..NTRZZZXX_UPOX...umol.l."            
# [33] "Nitrate..NTRAZZXX_UPOX...umol.l."                             	# [34] "QV.ODV.Nitrate..NTRAZZXX_UPOX...umol.l."                      
# [35] "Nitrite..NTRIZZXX_UPOX...umol.l."                             	# [36] "QV.ODV.Nitrite..NTRIZZXX_UPOX...umol.l."                      
# [37] "Ammonium..AMONZZXX_UPOX...umol.l."                            	# [38] "QV.ODV.Ammonium..AMONZZXX_UPOX...umol.l."                     
# [39] "Total.Nitrogen..NTOTZZXX_UPOX...umol.l."                      	# [40] "QV.ODV.Total.Nitrogen..NTOTZZXX_UPOX...umol.l."               
# [41] "Hydrogen.Sulphide..H2SXZZXX_UPOX...umol.l."                   	# [42] "QV.ODV.Hydrogen.Sulphide..H2SXZZXX_UPOX...umol.l."            
# [43] "pH..PHXXZZXX_UUPH...pH.units."                                	# [44] "QV.ODV.pH..PHXXZZXX_UUPH...pH.units."                         
# [45] "Total.Alkalinity..ALKYZZXX_MEQL...mEq.l."                     	# [46] "QV.ODV.Total.Alkalinity..ALKYZZXX_MEQL...mEq.l."              
# [47] "Chlorophyll.a..CPHLZZXX_UGPL...ug.l."                         	# [48] "QV.ODV.Chlorophyll.a..CPHLZZXX_UGPL...ug.l."                  
# [49] "Turbidity..TURBXXXX_USTU...NTU."                              	# [50] "QV.ODV.Turbidity..TURBXXXX_USTU...NTU."                       
# [51] "Temperature.of.pH.determination..PHTXPR01_UPAA...degC."       	# [52] "QV.ODV.Temperature.of.pH.determination..PHTXPR01_UPAA...degC."
# [53] "Organic.Carbon..CORGZZZX_UPOX...umol.l."                      	# [54] "QV.ODV.Organic.Carbon..CORGZZZX_UPOX...umol.l."  

# Create date variables (this is done because sometimes the date in the original data is not as described - yyyy.mm.ddThh.mm.ss.sss; sometimes e.g., time is missing):
Z <- master_bot[,.(yyyy.mm.ddThh.mm.ss.sss)]

# Convert the datetime values to POSIXct format
Z[, datetime := as.POSIXct(yyyy.mm.ddThh.mm.ss.sss, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")]

# Fill NA for dates without time component
Z[is.na(datetime), datetime := as.POSIXct(yyyy.mm.ddThh.mm.ss.sss, format = "%Y-%m-%d", tz = "UTC")]

# Extract year, month, day, hour, and minute
Z[, `:=`(
  Year = format(datetime, "%Y"),
  Month = format(datetime, "%m"),
  Day = format(datetime, "%d"),
  Hour = format(datetime, "%H"),
  Minute = format(datetime, "%M")
)]

# Adding Year, Month, and Day from Z to bot
master_bot[, `:=`(Year = Z$Year,
           Month = Z$Month,
           Day = Z$Day,
           Hour = Z$Hour,
           Minute = Z$Minute)]

# This part keeps only the rows where quality flags (likely from ODV = Ocean Data View) are less than or equal to 1, which usually means good or acceptable quality.
# Also, only the assessment period data is selected here:
bot <- master_bot[Year >= assessment$start_year & Year <= assessment$end_year &
             QV.ODV.Depth..ADEPZZ01_ULAA...m. <= 1 & 
             QV.ODV.Temperature..TEMPPR01_UPAA...degC. <=1 & 
             QV.ODV.Salinity..PSALPR01_UUUU...dmnless. <=1 & 
             QV.ODV.Oxygen..DOXYZZXX_UMLL...ml.l. <= 1,.
           (Cruise, Year, Month, Day, Hour, Minute, 
             Latitude = Latitude..degrees_north., 
             Longitude = Longitude..degrees_east., 
             Depth_m = Depth..ADEPZZ01_ULAA...m., 
             Temperature_degreesC = Temperature..TEMPPR01_UPAA...degC., 
             Salinity_psu = Salinity..PSALPR01_UUUU...dmnless., 
             Oxygen_mll = Oxygen..DOXYZZXX_UMLL...ml.l., 
             Hydrogen_Sulphide_umoll = Hydrogen.Sulphide..H2SXZZXX_UPOX...umol.l., 
             Ammonium_Nitrogen_umoll = Ammonium..AMONZZXX_UPOX...umol.l.)]

# Remove master_bot
rm(master_bot, inherits = TRUE)


########### REMOVE DUPLICATES

# Set allowed ranges for parameter duplicates:
# Temperature is in degrees C and the allowed difference is set to:
set_temp_range = 0.1
# Salinity is in psu and the allowed difference is set to:
set_sal_range = 0.1
# Oxygen is in ml l-1 (mg/L = ml/L* 1.428) and the allowed difference is set to:
set_oxy_range = 0.1
# H2S is in umol l-1 (mg/L = µmol/L × 0.03408) and the allowed difference is set to:
set_h2s_range = 0.1
# NH4 is in umol l-1 and the allowed difference is set to:
set_nh4_range = 0.1

# For CTD data:
# Remove rows where salinity data is missing:
ctd_orig <- ctd
ctd <- ctd_orig[!is.na(ctd_orig$Salinity_psu), ]

# Extract unique rows based on specified columns and get their indices:
idx_ctd <- ctd %>%
  mutate(row_id = row_number()) %>%
  distinct(Year, Month, Day, Hour, Minute, Longitude, Latitude, .keep_all = TRUE) %>%
  pull(row_id)

# Convert the vector to a data frame:
idx_ctd_df <- data.frame(row_id = idx_ctd)

# Use mutate to create a new id column:
idx_ctd_df <- idx_ctd_df %>%
  mutate(id = 1:n())

# Get only metadata columns:
ctd_unique <- ctd[idx_ctd, .(Cruise, Year, Month, Day, Hour, Minute, Latitude, Longitude)]

# Add id column to unique metadata:
ctd_unique$id <- idx_ctd_df$id

# Move 'id' to be the first column:
ctd_unique <- ctd_unique %>%
  select(id, everything())

# Create a mapping between the key columns and the id:
id_mapping <- ctd_unique %>%
  select(id, Year, Month, Day, Hour, Minute, Longitude, Latitude)

# Join this mapping back to the original data frame
ctd <- ctd %>%
  left_join(id_mapping, by = c("Year", "Month", "Day", "Hour", "Minute", "Longitude", "Latitude")) %>%
  select(id, everything())  # Move id to first column

# Get consolidated data - average duplicates if appropriate (depends on the set ranges for variables)
ctd$consolidated <- NA_character_
ctd$allmissing <- NA_character_

# Initialize empty result data frame (used in loop below): 
T_ctd <- data.frame()

# Safe range calculation function
safe_range <- function(x) {
  if(all(is.na(x))) return(NA)
  max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
}

# Safe mean function
safe_mean <- function(x) {
  if(all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}

# Loop through unique 'stations'
for(i in 1:nrow(ctd_unique)) {
  # Find matching rows based on datetime and location:
  fD <- which(ctd$id == ctd_unique$id[i])
  
  # Subset the data:
  t <- ctd[fD, ]
  ud <- unique(t$Depth_m)
  
  # Check if all salinity or temperature values are missing:
  f_S <- sum(is.na(t$Salinity_psu))
  f_T <- sum(is.na(t$Temperature_degreesC))
  
  if(f_S == nrow(t) || f_T == nrow(t)) {
    t$Salinity_psu <- NA
    t$Temperature_degreesC <- NA
    t$Oxygen_mll <- NA
    t$allmissing <- "TRUE"
  }
  
  # If there are duplicate depth values per ID:
  if(nrow(t) > length(ud)) {
    for(j in 1:length(ud)) {
      f <- which(t$Depth_m == ud[j])
      if(length(f) > 1) {
        # Calculate variable ranges at duplicate depth values:
        temp_range <- safe_range(t$Temperature_degreesC[f])
        sal_range <- safe_range(t$Salinity_psu[f])
        oxy_range <- safe_range(t$Oxygen_mll[f])
        
        
        # Get mean values of variables if ranges are <= set ranges:
        if(!is.na(temp_range) && temp_range <= set_temp_range) {
          t$Temperature_degreesC[f] <- safe_mean(t$Temperature_degreesC[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Temperature_degreesC[f] <- NA
        }
        
        
        if(!is.na(sal_range) && sal_range <= set_sal_range) {
          t$Salinity_psu[f] <- safe_mean(t$Salinity_psu[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Salinity_psu[f] <- NA
        }
        
        if(!is.na(oxy_range) && oxy_range <= set_oxy_range) {
          t$Oxygen_mll[f] <- safe_mean(t$Oxygen_mll[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Oxygen_mll[f] <- NA
        }
      }
    }
  }
 
  if(i == 1) {
    T_ctd <- t
  } else {
    T_ctd <- rbind(T_ctd, t)
  }
}

#  Write out data
fwrite(T_ctd, file.path(outputPath, "T_ctd.csv"))


# For BOT data:
# Remove rows where salinity data is missing:
bot_orig <- bot
bot <- bot_orig[!is.na(bot_orig$Salinity_psu), ]

# BOT data is missing hour and minute data sometimes - add 12:00 to replace the NA values.
bot$Hour[is.na(bot$Hour)] <- "12"
bot$Minute[is.na(bot$Minute)] <- "00"

# Extract unique rows based on specified columns and get their indices:
idx_bot <- bot %>%
  mutate(row_id = row_number()) %>%
  distinct(Year, Month, Day, Hour, Minute, Longitude, Latitude, .keep_all = TRUE) %>%
  pull(row_id)

# Convert the vector to a data frame:
idx_bot_df <- data.frame(row_id = idx_bot)

# Use mutate to create a new id column:
idx_bot_df <- idx_bot_df %>%
  mutate(id = 1:n())

# Get only metadata columns:
bot_unique <- bot[idx_bot, .(Cruise, Year, Month, Day, Hour, Minute, Latitude, Longitude)]

# Add id column to unique metadata:
bot_unique$id <- idx_bot_df$id

# Move 'id' to be the first column:
bot_unique <- bot_unique %>%
  select(id, everything())

# Create a mapping between the key columns and the id:
id_mapping <- bot_unique %>%
  select(id, Year, Month, Day, Hour, Minute, Longitude, Latitude)

# Join this mapping back to the original data frame:
bot <- bot %>%
  left_join(id_mapping, by = c("Year", "Month", "Day", "Hour", "Minute", "Longitude", "Latitude")) %>%
  select(id, everything())  # Move id to first column

# Get consolidated data - average duplicates if appropriate (depends on the set ranges for variables)
bot$consolidated <- NA_character_
bot$allmissing <- NA_character_

# Initialize empty result data frame:
T_bot <- data.frame()

# Loop through unique 'stations'
for(i in 1:nrow(bot_unique)) {
  # Find matching rows based on datetime and location:
  fD <- which(bot$id == bot_unique$id[i])
  
  # Subset the data:
  t <- bot[fD, ]
  ud <- unique(t$Depth_m)
  
  # Check if all salinity or temperature values are missing:
  f_S <- sum(is.na(t$Salinity_psu))
  f_T <- sum(is.na(t$Temperature_degreesC))
  
  if(f_S == nrow(t) || f_T == nrow(t)) {
    t$Salinity_psu <- NA
    t$Temperature_degreesC <- NA
    t$Oxygen_mll <- NA
    t$allmissing <- "TRUE"
  }
  
  # If there are duplicate depth values per ID:
  if(nrow(t) > length(ud)) {
    for(j in 1:length(ud)) {
      f <- which(t$Depth_m == ud[j])
      if(length(f) > 1) {
        # Calculate variable ranges at duplicate depth values:
        temp_range <- safe_range(t$Temperature_degreesC[f])
        sal_range <- safe_range(t$Salinity_psu[f])
        oxy_range <- safe_range(t$Oxygen_mll[f])
        h2s_range <- safe_range(t$Hydrogen_Sulphide_umoll[f])
        nh4_range <- safe_range(t$Ammonium_Nitrogen_umoll[f])
        
        # Get mean values of variables if ranges are <= set ranges:
        if(!is.na(temp_range) && temp_range <= set_temp_range) {
          t$Temperature_degreesC[f] <- safe_mean(t$Temperature_degreesC[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Temperature_degreesC[f] <- NA
        }
        
        
        if(!is.na(sal_range) && sal_range <= set_sal_range) {
          t$Salinity_psu[f] <- safe_mean(t$Salinity_psu[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Salinity_psu[f] <- NA
        }
        
        if(!is.na(oxy_range) && oxy_range <= set_oxy_range) {
          t$Oxygen_mll[f] <- safe_mean(t$Oxygen_mll[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Oxygen_mll[f] <- NA
        }
        
        if(!is.na(h2s_range) && h2s_range <= set_h2s_range) {
          t$Hydrogen_Sulphide_umoll[f] <- safe_mean(t$Hydrogen_Sulphide_umoll[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Hydrogen_Sulphide_umoll[f] <- NA
        }
        
        if(!is.na(nh4_range) && nh4_range <= set_nh4_range) {
          t$Ammonium_Nitrogen_umoll[f] <- safe_mean(t$Ammonium_Nitrogen_umoll[f])
          t$consolidated[f[1]] <- "TRUE"
          t$consolidated[f[2:length(f)]] <- "REMOVE"
        } else {
          t$Ammonium_Nitrogen_umoll[f] <- NA
        }
      }
    }
  }
 
  if(i == 1) {
    T_bot <- t
  } else {
    T_bot <- rbind(T_bot, t)
  }
}

#  Write out data
fwrite(T_bot, file.path(outputPath, "T_bot.csv"))

# Remove data marked 'REMOVE' in the duplicate process
T_ctd_2 <- T_ctd %>%
  filter(consolidated != 'REMOVE' | is.na(consolidated))
T_ctd_2 <- T_ctd_2 %>%
  filter(!is.na(Salinity_psu))
T_ctd_2$source <- "CTD"

T_bot_2 <- T_bot %>%
  filter(consolidated != 'REMOVE' | is.na(consolidated))
T_bot_2 <- T_bot_2 %>%
  filter(!is.na(Salinity_psu))
T_bot_2$source <- "BOT"

# Check which bot data have match in ctd data
bot_ismatch <-  character(1)

# Perform a full join to add data from T_bot_2 to T_ctd_2
# If no ctd data is available for said bot data, then bot_ismatch <- "KEEP". This means that, bot data is kept only when ctd is missing.
result_full <- T_ctd_2 %>%
  full_join(T_bot_2, by = c("Year", "Month", "Day", "Hour", "Minute", "Longitude", "Latitude", "Depth_m")) %>%
  mutate(bot_ismatch = ifelse(is.na(id.x), "KEEP", ""))

# Add BOT data to CTD columns
# For rows marked "KEEP", make id.x equal to id.y; other rows stay unchanged.
result_full$id.x[result_full$bot_ismatch == "KEEP"] <- result_full$id.y[result_full$bot_ismatch == "KEEP"]
result_full$Cruise.x[result_full$bot_ismatch == "KEEP"] <- result_full$Cruise.y[result_full$bot_ismatch == "KEEP"]
result_full$Temperature_degreesC.x[result_full$bot_ismatch == "KEEP"] <- result_full$Temperature_degreesC.y[result_full$bot_ismatch == "KEEP"]
result_full$Salinity_psu.x[result_full$bot_ismatch == "KEEP"] <- result_full$Salinity_psu.y[result_full$bot_ismatch == "KEEP"]
result_full$Oxygen_mll.x[result_full$bot_ismatch == "KEEP"] <- result_full$Oxygen_mll.y[result_full$bot_ismatch == "KEEP"]
result_full$consolidated.x[result_full$bot_ismatch == "KEEP"] <- result_full$consolidated.y[result_full$bot_ismatch == "KEEP"]
result_full$allmissing.x[result_full$bot_ismatch == "KEEP"] <- result_full$allmissing.y[result_full$bot_ismatch == "KEEP"]
result_full$source.x[result_full$bot_ismatch == "KEEP"] <- result_full$source.y[result_full$bot_ismatch == "KEEP"]

# Remove BOT columns
result_full$id.y <- NULL
result_full$Cruise.y <- NULL
result_full$Temperature_degreesC.y <- NULL
result_full$Salinity_psu.y <- NULL
result_full$Oxygen_mll.y <- NULL
result_full$consolidated.y <- NULL
result_full$allmissing.y <- NULL
result_full$source.y <- NULL
result_full$id.x <- NULL
result_full$bot_ismatch <- NULL

# Remove the suffixes from column names
result_full <- result_full %>%
  rename_with(~ gsub("\\.x$", "", .))

# Create new ID-s
# Extract unique rows based on specified columns and get their indices
idx <- result_full %>%
  mutate(row_id = row_number()) %>%
  distinct(Year, Month, Day, Hour, Minute, Longitude, Latitude, .keep_all = TRUE) %>%
  pull(row_id)

# Convert the vector to a data frame:
idx_df <- data.frame(row_id = idx)

# Use mutate to create a new id column:
idx_df <- idx_df %>%
  mutate(ID = 1:n())

# Get only metadata columns
result_unique <- result_full[idx, .(Year, Month, Day, Hour, Minute, Longitude, Latitude)]

# Add id column to unique metadata:
result_unique$ID <- idx_df$ID

# Move 'id' to be the first column:
result_unique <- result_unique %>%
  select(ID, everything())

# Create a mapping between the key columns and the id:
id_mapping <- result_unique %>%
  select(ID, Year, Month, Day, Hour, Minute, Longitude, Latitude)

# Join this mapping back to the original data frame:
result_full <- result_full %>%
  left_join(id_mapping, by = c("Year", "Month", "Day", "Hour", "Minute", "Longitude", "Latitude"))

# Move 'id' to be the first column: 
result_full <- dplyr::select(result_full, ID, everything())

# Sort by ID + Retain only columns needed:
oxy <- result_full[order(ID), .(ID, Cruise, Year, Month, Day, Latitude, Longitude, Depth_m, Temperature_degreesC, Salinity_psu, Oxygen_mll, Hydrogen_Sulphide_umoll, Ammonium_Nitrogen_umoll, source)]


########## MAKE STATIONS SPATIAL
# Classify oxy stations into oxy areas
# Extract unique stations i.e. longitude/latitude pairs
stations <- unique(oxy[, .(Longitude, Latitude)])

# Make stations spatial keeping original latitude/longitude
# st_as_sf(...): Converts a data frame into an sf (spatial features) object.
# coords = c("Longitude", "Latitude"): Specifies which columns contain the spatial coordinates. These will be used to create point geometries.
# remove = FALSE: Keeps the original Longitude and Latitude columns in the data frame (instead of removing them after converting to geometry).
# crs = 4326: Sets the Coordinate Reference System to EPSG:4326, which is the standard for GPS coordinates (WGS 84 — latitude and longitude in degrees).
stations <- st_as_sf(stations, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# Transform projection into UTM zone 34N:
stations <- st_transform(stations, crs = 32634)

# Read indicator modelling areas:
oxy_areas <- st_read(file.path(outputPath, "oxy_areas.shp"))

# Classify stations into oxy areas
stations <- st_join(stations, oxy_areas, join = st_intersects)

# Delete stations not classified
stations <- na.omit(stations)

# Create x y columns with projected coordinates for later
# Defines a function called sfc_as_cols.
# Takes two arguments:
# x: an sf object (spatial data frame).
# names: a character vector of column names for the coordinates (default: "x" and "y").
sfc_as_cols <- function(x, names = c("x","y")) {
  # Checks that:
  # x is an sf object.
  # Its geometry column contains point geometries (sfc_POINT).
  # If not, it stops with an error.
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  # Extracts the coordinates from the geometry column of x.
  # Returns a matrix with columns like X and Y.
  ret <- sf::st_coordinates(x)
  # Converts the coordinate matrix into a tibble (a modern data frame).
  ret <- tibble::as_tibble(ret)
  # Ensures that the number of names provided (e.g., "x" and "y") matches the number of coordinate columns (usually 2 for points).
  stopifnot(length(names) == ncol(ret))
  # Removes any existing columns in x that have the same names as the ones you're about to add (e.g., "x" or "y"), to avoid name conflicts.
  x <- x[ , !names(x) %in% names]
  # Renames the coordinate columns to the user-specified names (e.g., "x" and "y").
  ret <- setNames(ret,names)
  # Combines the original data frame x (without geometry) and the new coordinate columns side by side.
  # Returns a tidy data frame with coordinates as regular columns.
  dplyr::bind_cols(x,ret)
}

# Convert the spatial sf object stations into a regular tibble by:
# 1. Extracting the geometry (point coordinates) from the sf object.
# 2. Adding those coordinates as regular columns named "x" and "y" (by default).
# 3. Removing the geometry column, so the result is no longer an sf object but a tidy data frame with coordinate columns.
stations <- sfc_as_cols(stations)

# Remove spatial column and make into data table
# Remove the spatial geometry from the stations sf object.
# After this, stations becomes a regular data frame (no longer spatial).
# The pipe (%>%) passes the result to as.data.table().
# This converts the data frame into a data.table, which is a high-performance version of a data frame from the data.table package.
stations <- st_set_geometry(stations, NULL) %>% as.data.table()

# Merge stations back into station samples - getting rid of station samples not classified into assessment units
oxy <- stations[oxy, on = .(Longitude, Latitude), nomatch = 0]

#  Write out data
fwrite(oxy, file.path(outputPath, "oxy.csv"))


########## DATA CLEANING
# Tidy up data and form some new variables for further analysis
# Compute number of oxygen observations per station:
oxy$n_Oxygen <- c(with(oxy, tapply(Oxygen_mll, ID, function(x) sum(!is.na(x))))[paste(oxy$ID)])

# Compute max depth per station:
oxy$max_depth_m <- c(with(oxy, tapply(Depth_m, ID, max))[paste(oxy$ID)])

# Oxygen observations are in ml/l - convert to mg/l:
oxy$Oxygen_mgl <- oxy$Oxygen_mll * 1.428 # or / 0.700

# Define function for calculating oxygen saturation concentration:
O2satFun <- function(temp) {
  tempabs <- temp + 273.15
  exp(-173.4292 + 249.6339 * (100/tempabs) +
        143.3483 * log(tempabs/100) - 21.8492 * (tempabs/100) +
        (-0.033096 + 0.014259 * (tempabs/100) - 0.0017000 * (tempabs/100)^2)
  ) * 1.428  # * Oxygen saturation in mg/l
}

# Compute oxygen deficit (called here and afterwards 'oxygen debt')
oxy$Oxygen_debt_mgl <- O2satFun(oxy$Temperature_degreesC) - oxy$Oxygen_mgl

# Supersaturation is not realistic below 30 m:
oxy$Oxygen_debt_mgl[which(oxy$Oxygen_debt_mgl < 0 & oxy$Depth_m > 30)] <- NA

# Set up covariates for modelling
oxy$date <- lubridate::ymd(with(oxy, paste(Year, Month, Day, sep = "-")))
oxy$yday <- lubridate::yday(oxy$date)
oxy$Basin <- factor(oxy$Name)    # 5 basins
oxy$Basin2 <- factor(oxy$F2_Name)    # 2 areas (1 and 4 basins)

# Set up censoring rules:
oxy$censor <- 0
oxy$censor[oxy$source == 'CTD' & oxy$Oxygen_mgl < 1] <- 1 # Do not use CTD data close to zero as they go constant;
oxy$censor[is.na(oxy$Hydrogen_Sulphide_umoll) & oxy$Oxygen_mgl == 0] <- 1 # Censoring and no measurement of H2S;

# Oxygen deficit should not be negative (this is a double check?):
oxy$Oxygen_debt_mgl[oxy$Oxygen_debt_mgl < 0] <- 0

# Write data
fwrite(oxy, file.path(outputPath, "oxy_clean.csv"))
