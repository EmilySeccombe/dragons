# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
# This file imports riverine Odonata presence-only data from the British 
# Dragonfly Society Recording Scheme dataset on NBN Atlas.
# It also imports water quality data available open access online from
# the Environment Agency and Earthwatch. 
# Data is compared to assess correlation between water quality and dragonfly
# presence.

# Install packages ----
desired_packages <- c("tidyverse", "rnrfa", "sp", "readr", "sf", "dplyr", "anytime")

for (pkg in desired_packages) {
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}


# Long lat function ----
# Taken from https://rdrr.io/github/l-hodge/ukgeog/src/R/latlng_eastnorth.R
convert_lnglat <- function(df, # Dataframe
                           easting, # Column containing Easting
                           northing # Column containing Northing
){
  
  # Create a SpatialPointsDataFrame
  spdf <- SpatialPointsDataFrame(df %>% select(!! easting, !! northing),
                                 data = df,
                                 proj4string = CRS("+init=epsg:27700"))
  
  # Convert to lat/lng
  conv <- spTransform(spdf, CRS("+init=epsg:4326"))
  
  # Back to dataframe
  new_df <- as.data.frame(conv)
  
  colnames(new_df)[ncol(new_df)-1] <- "Long"
  colnames(new_df)[ncol(new_df)] <- "Lat"
  
  return(new_df)
  
}



# Determine whether in viewing mode ----
viewing_mode_answer <- readline(prompt = "Do you want to view outputs such as viewing tables and graphs? If yes, enter 1, else enter anything else: \n\n")

# Handle non-numeric input (e.g., user enters text)
if (is.na(viewing_mode_answer)) {
  viewing_mode <- FALSE
  cat("Invalid input. Defaulting to not viewing outputs (FALSE).\n")
} else {
  viewing_mode <- viewing_mode_answer == 1
}


# Citations ----
# Data source:
# Citation for NBN Atlas: 
# NBN Atlas occurrence download at https://nbnatlas.org accessed on 28 May 2025.
# Citation for dataset:
# Copyright British Dragonfly Society Recording Scheme 2025. Dragonfly records from the British Dragonfly Society Recording Scheme. Occurrence dataset on the NBN Atlas.

# Citations for R and packages used:
# R:
# R Core Team (2025). _R: A Language and Environment for Statistical
# Computing_. R Foundation for Statistical Computing, Vienna,
# Austria. <https://www.R-project.org/>.
# R version 4.5.0 (2025-04-11 ucrt)

# Tidyverse:
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,
# Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL,
# Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu
# V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019).
# “Welcome to the tidyverse.” _Journal of Open Source Software_,
# *4*(43), 1686. doi:10.21105/joss.01686
# <https://doi.org/10.21105/joss.01686>.

# Import Dragonfly Data ----
riverine_odonata <- read_csv("G:/My Drive/Research project/riverine_odonata.csv")

# Clean Dragonfly Data ----
# Select columns required
colnames(riverine_odonata)[122] <- "longitude"
colnames(riverine_odonata)[133] <- "latitude"
r_o_selected_headers <- select(riverine_odonata, lifeStage, eventDate, gridReference, scientificName, longitude, latitude)


# Look at life stage variety:
lifeStageVariety <- table(r_o_selected_headers$lifeStage)
if(viewing_mode){
  view(lifeStageVariety)
}


# Convert to R Date format
#TODO check the next step deals with all eventualities correctly!
r_o_selected_headers$eventDate <- as.Date(anydate(r_o_selected_headers$eventDate))
# Remove anything before 2010
r_o_since2010 <- r_o_selected_headers %>% filter(eventDate > anydate('01-01-2010'))

# Look at species variety:
spVariety <- table(r_o_since2010$scientificName)
if (viewing_mode){
  view(spVariety)
}

# Look at grid refs and their precision:
grVariety <- table(r_o_since2010$gridReference)
if (viewing_mode){
  view(grVariety)
}
r_o_since2010$grPrecision <- nchar(r_o_since2010$gridReference)
grPresVariety <- table(r_o_since2010$grPrecision)
if (viewing_mode){
  view(grPresVariety)
}

# Remove entries where precision is less than 6 figures.
r_o_since2010_more_precise <- r_o_since2010 %>% filter(grPrecision > 7)

# Convert 

# Import water quality data ----
ea_water_quality <- read_csv("G:/My Drive/Research project/ea_water_quality.csv")

# Filter results just to ammonia:
ea_water_quality <- ea_water_quality %>% filter(str_detect(determinand.label, "Ammonia\\(N\\)"))


# If have missing values of location, remove.
if (viewing_mode) {
  print(summary(ea_water_quality$sample.samplingPoint.northing))
  print(summary(ea_water_quality$sample.samplingPoint.easting))
}
# This ^ shows 1 NA.
ea_water_quality <- ea_water_quality[!is.na(ea_water_quality$sample.samplingPoint.easting),]
if (viewing_mode) {
  print(summary(ea_water_quality$sample.samplingPoint.northing))
  print(summary(ea_water_quality$sample.samplingPoint.easting))
}
# All good, now removed.

# Convert eastings and northings to longitude and latitude
ea_water_quality <- convert_lnglat(ea_water_quality, "sample.samplingPoint.easting", "sample.samplingPoint.northing")

# Notes on location:
# BDS dragonfly data is given as a longitude and latidude and grid reference
# EA data is given as Eastings and Northings.
# Earthwatch data is given as longitude and latitude. 

# Import earthwater wq
##earthwater_water_quality <- read_csv("G:/My Drive/Research project/earthwater_water_quality.csv")


# Compare data to find points close together ----
#Convert dataframes to spatial objects
# Select just one species:
run_analysis_per_species <- function(species_name){
  print("Results for")
  print(species_name)
  r_o_since2010_more_precise <- r_o_since2010_more_precise %>% filter(scientificName == species_name)
  
  species_sf <- st_as_sf(r_o_since2010_more_precise, coords = c("longitude", "latitude"), crs = 4326)
  water_sf   <- st_as_sf(ea_water_quality, coords = c("Long", "Lat"), crs = 4326)
  
  #Transform date into date format
  species_sf$eventDate_parsed <- as.Date(species_sf$eventDate, format = "%Y-%m-%d")
  
  #Convert to a flat co-ord system measured in metres.
  sf_wq_proj <- st_transform(water_sf, 32630)  # Change EPSG to appropriate UTM zone
  sf_dragonfly_proj <- st_transform(species_sf, 32630)
  
  # For each dragonfly record, find index of nearest water quality record
  nearest_idx <- st_nearest_feature(sf_dragonfly_proj, sf_wq_proj)
  
  # Calculate distance to nearest water quality record
  distances <- st_distance(sf_dragonfly_proj, sf_wq_proj[nearest_idx, ], by_element = TRUE)
  
  # Filter to only include dragonfly records within 500m of a water quality record
  within_500m <- as.numeric(distances) <= 500  # logical vector
  sf_dragonfly_filtered <- sf_dragonfly_proj[within_500m, ]
  nearest_idx_filtered <- nearest_idx[within_500m]
  sf_dragonfly_filtered$nearest_wq_result <- sf_wq_proj$result[nearest_idx_filtered]  # assuming you have an ID column
  # This is assuming the wq data was all for the same thing - its not!
  sf_dragonfly_filtered <- st_transform(sf_dragonfly_filtered, 4326)
  
  # For each water quality record, identify whether there is a dragonfly record
  # within 500m within 1 year
  within_500m_list <- st_is_within_distance(sf_wq_proj, sf_dragonfly_proj, dist = 500)
  
  wq_dragonfly_pairs <- lapply(seq_along(within_500m_list), function(i) {
    if (length(within_500m_list[[i]]) == 0) return(NULL)
    data.frame(
      wq_index = i,
      dragonfly_index = within_500m_list[[i]]
    )
  }) %>% bind_rows()
  
  # Step 3: Add date columns for filtering
  
  
  wq_dragonfly_pairs <- wq_dragonfly_pairs %>%
    mutate(
      wq_date = sf_wq_proj$sample.sampleDateTime[wq_index],
      dragonfly_date = sf_dragonfly_proj$eventDate_parsed[dragonfly_index],
      date_diff = abs(as.numeric(difftime(wq_date, dragonfly_date, units = "days")))
    )
  
  # Step 4: Filter pairs to those within 365 days
  wq_dragonfly_pairs_filtered <- wq_dragonfly_pairs %>%
    filter(date_diff <= 365)
  
  # Step 5: Mark water quality records that have at least one dragonfly nearby in space AND time
  sf_wq_proj$dragonfly_within_500m_1yr <- FALSE
  sf_wq_proj$dragonfly_within_500m_1yr[unique(wq_dragonfly_pairs_filtered$wq_index)] <- TRUE
  
  # See how many records are left:
  if(viewing_mode){
    table(sf_wq_proj$dragonfly_within_500m_1yr)
  }
  
  # See if there's a relation between species presence and Ammonia(N)
  correlation_test_results <- cor.test(as.numeric(sf_wq_proj$dragonfly_within_500m_1yr), sf_wq_proj$result)
  
  if(viewing_mode){
    print("Correlation test results: \n\n")
    print(correlation_test_results)
    table_header <- paste("Ammonia(N) and Presence of", species_name, sep=" ")
    boxplot(sf_wq_proj$result~sf_wq_proj$dragonfly_within_500m_1yr,
            data=sf_wq_proj,
            main=table_header,
            xlab="Dragonfly presence",
            ylab="Water Quality Result",
            col="orange",
            border="brown"
      )
  }
  
  # Try with glms:
  model <- glm(sf_wq_proj$dragonfly_within_500m_1yr ~ sf_wq_proj$result,family=binomial)
  if(viewing_mode){
    print("GLM output: \n\n")
    summary(model)$coefficients
  }
}
  
if(viewing_mode){
  cat("C. virgo results:")
}
run_analysis_per_species("Calopteryx virgo")
if(viewing_mode){
  cat("C. splendens results:")
}
run_analysis_per_species("Calopteryx splendens")
if(viewing_mode){
  cat("Libellula fulva results:")
}
run_analysis_per_species("Libellula fulva")
if(viewing_mode){
  cat("Cordulegaster boltonii results:")
}
run_analysis_per_species("Cordulegaster boltonii")
if(viewing_mode){
  cat("Platycnemis pennipes results:")
}
run_analysis_per_species("Platycnemis pennipes")
if(viewing_mode){
  cat("Gomphus vulgatissimus results:")
}
run_analysis_per_species("Gomphus vulgatissimus")




#Decide which ones to look at:
# Step 1: Create the table of chemical counts
chemical_counts <- table(ea_water_quality$determinand.label)

# Step 2: Convert the table to a data frame
chemical_df <- as.data.frame(chemical_counts)

# Step 3: Install and load the 'writexl' package (if not already installed)
install.packages("writexl")      # Run only once
library(writexl)

# Step 4: Write the data frame to an Excel file
write_xlsx(chemical_df, "chemical_counts.xlsx")

