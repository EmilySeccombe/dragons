# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
# This file imports riverine Odonata presence-only data from the British 
# Dragonfly Society Recording Scheme dataset on NBN Atlas. The data is then
# sorted for use in analysis.

# Install packages ----
desired_packages <- c("tidyverse", "rnrfa", "sp", "readr")

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
r_o_selected_headers <- select(riverine_odonata, lifeStage, eventDate, gridReference, scientificName)

# Look at life stage variety:
lifeStageVariety <- table(r_o_selected_headers$lifeStage)
if(viewing_mode){
  view(lifeStageVariety)
}


# Convert to R Date format
#TODO check the next step deals with all eventualities correctly!
r_o_selected_headers$eventDate <- as.Date(r_o_selected_headers$eventDate)
# Remove anything before 2010
r_o_since2010 <- r_o_selected_headers %>% filter(year(eventDate) > "2010")

# Look at species variety:
spVariety <- table(r_o_since2010$scientificName)
if (viewing_mode){
  view(spVariety)
}

# Maybe not enough of some species - to check later 

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
r_o_since2010_more_precise <- r_o_since2010 %>% filter(grPrecision > 5)

# Import water quality data ----
ea_water_quality <- read_csv("G:/My Drive/Research project/ea_water_quality.csv")

# If have missing values of location, remove.
#WHy does this only show one of thetwo summaries?? TODO
if (viewing_mode) {
  summary(ea_water_quality$sample.samplingPoint.northing)
  summary(ea_water_quality$sample.samplingPoint.easting)
}
# This ^ shows 1 NA.
ea_water_quality <- ea_water_quality[!is.na(ea_water_quality$sample.samplingPoint.easting),]
if (viewing_mode) {
  summary(ea_water_quality$sample.samplingPoint.northing)
  summary(ea_water_quality$sample.samplingPoint.easting)
}
# All good, now removed.

# Convert to eastings and northings
ea_water_quality <- convert_lnglat(ea_water_quality, "sample.samplingPoint.easting", "sample.samplingPoint.northing")

# dfly data has long lat and gr
# ea has e and n TURN INTO LONG LAT
# earthwater has long lat 

# Import earthwater wq
##earthwater_water_quality <- read_csv("G:/My Drive/Research project/earthwater_water_quality.csv")


# Compare data to find points close together?
# see notes