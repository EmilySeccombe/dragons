# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
# This file imports riverine Odonata presence-only data from the British 
# Dragonfly Society Recording Scheme dataset on NBN Atlas.
# It also imports water quality data available open access online from
# the Environment Agency. 
# Data is compared to assess correlation between water quality and dragonfly
# presence.

# Define output file
output_file <- "G:/My Drive/Research project/dragons/odonata_analysis_results.txt"

# Clear or create the file
writeLines(c("Riverine Odonata Analysis Results\n", 
             paste("Generated on:", Sys.time()), 
             "\n-----------------------------------\n"), 
           output_file)


# Install packages ----
desired_packages <- c("tidyverse", "rnrfa", "sp", "readr", "sf", "dplyr", "anytime", "logistf")

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
riverine_odonata <- read_csv("G:/My Drive/Research project/dragons/riverine_odonata.csv", show_col_types = FALSE)

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

# Import water quality data ----

years <- 2010:2025
list_of_dfs <- list()

for (year in years){
  csv_name <- paste0("G:/My Drive/Research project/dragons/", year, ".csv")
  
  df <- read_csv(csv_name, show_col_types = FALSE)
  #Select rows of interest
  df <- df[df$determinand.label %in% c("Ammonia(N)", "Temp Water", "BOD ATU", "pH", "O Diss %sat",
                                                                           "Orthophospht", "N Oxidised", "Cond @ 25C", "Sld Sus@105C"),]
  # Remove all columns not needed 
  df <- df %>%
    select(sample.sampleDateTime, determinand.label, result, sample.samplingPoint.easting, sample.samplingPoint.northing)
  
  list_of_dfs[[as.character((year))]] <- df
}

ea_water_quality <- bind_rows(list_of_dfs)

# If have missing values of location, remove.
if (viewing_mode) {
  print(summary(ea_water_quality$sample.samplingPoint.northing))
  print(summary(ea_water_quality$sample.samplingPoint.easting))
}
# Remove NAs if required:
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

# Compare data to find points close together ----
run_analysis_per_species <- function(species_name, chemical){
  cat(paste("\nAnalyzing ", species_name, 
            "and ", chemical, "\n"), 
      file = output_file, append = TRUE)

  chosen_sp <- r_o_since2010_more_precise %>% filter(scientificName == species_name)
  chosen_chem <- ea_water_quality %>% filter(str_detect(determinand.label, chemical))
  
  species_sf <- st_as_sf(chosen_sp, coords = c("longitude", "latitude"), crs = 4326)
  water_sf   <- st_as_sf(chosen_chem, coords = c("Long", "Lat"), crs = 4326)
  
  #Transform date into date format
  species_sf$eventDate_parsed <- as.Date(species_sf$eventDate, format = "%Y-%m-%d")
  
  #Convert to a flat co-ord system measured in metres.
  sf_wq_proj <- st_transform(water_sf, 32630)  # Change EPSG to appropriate UTM zone
  sf_dragonfly_proj <- st_transform(species_sf, 32630)
  
  sf_wq_proj$scaled_result <- scale(sf_wq_proj$result)
  
  # For each dragonfly record, find index of nearest water quality record
  nearest_idx <- st_nearest_feature(sf_dragonfly_proj, sf_wq_proj)
  
  # Calculate distance to nearest water quality record
  distances <- st_distance(sf_dragonfly_proj, sf_wq_proj[nearest_idx, ], by_element = TRUE)
    
  # Filter to only include dragonfly records within 500m of a water quality record
  within_500m <- as.numeric(distances) <= 500  # logical vector
  sf_dragonfly_filtered <- sf_dragonfly_proj[within_500m, ]
  nearest_idx_filtered <- nearest_idx[within_500m]
  sf_dragonfly_filtered$nearest_wq_result <- sf_wq_proj$scaled_result[nearest_idx_filtered]  # assuming you have an ID column
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
  
  # Check if there are dragonfly-water quality record pairs within 500m.
  if (nrow(wq_dragonfly_pairs) == 0) {
    cat(paste(paste("No matching dragonfly-water quality pairs within 500m for ", species_name, " and ", chemical, ".")), file = output_file, append = TRUE)
    return(NULL)
  }
  
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

  valid_rows <- complete.cases(sf_wq_proj$dragonfly_within_500m_1yr, sf_wq_proj$scaled_result)
  
  if (sum(valid_rows) < 10 || length(unique(sf_wq_proj$dragonfly_within_500m_1yr[valid_rows])) < 2) {
    cat("skipping as not enough data or only one class in outcome:\n", file = output_file, append = TRUE)
    next  # Skip model if not enough data or only one class in outcome
  }
  
  # Check predictor variation
  low_var_predictor <- var(sf_wq_proj$scaled_result, na.rm = TRUE) < 1e-6
  
  if (low_var_predictor) {
    cat("Low variation in predictor variable", file=output_file, append = TRUE)
  }
  
  sp_name <- gsub(" ", "_", species_name)
  chem_name <- gsub(" ", "_", chemical)
  filename <- paste0(sp_name, "_and_", chem_name, ".png")
  filename <- gsub("%", "pc", filename)
  png(filename, width = 800, height = 600)
  plot_title <- paste(species_name, "and", chemical)
  # plot(sf_wq_proj$scaled_result, sf_wq_proj$dragonfly_within_500m_1yr, 
  #      xlab = "Water quality result (scaled)", ylab = "Presence", 
  #      main = plot_title)
  
  boxplot <- ggplot(data=sf_wq_proj, aes(x=dragonfly_within_500m_1yr, y=result, fill=dragonfly_within_500m_1yr)) +
    geom_boxplot() +
    coord_flip() +
    scale_fill_viridis_d(alpha=0.6) +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(plot_title) +
    xlab("Dragonfly presence") +
    ylab("Water quality result")
  print(boxplot)
  dev.off()
  
  # Check if outcome is imbalanced
  outcome_table <- table(sf_wq_proj$dragonfly_within_500m_1yr)
  imbalance <- any(outcome_table / sum(outcome_table) < 0.05)
  
  if (imbalance) {
    cat("Highly imbalanced outcome variable (<5% presence). \n", file = output_file, append = TRUE)
  }
  
  pairs_present <- sum(sf_wq_proj$dragonfly_within_500m_1yr == TRUE & !is.na(sf_wq_proj$scaled_result))
  pairs_absent <- sum(sf_wq_proj$dragonfly_within_500m_1yr == FALSE & !is.na(sf_wq_proj$scaled_result))
  cat("Pairs of dragonfly-wq data where dragonfly is present:", pairs_present, "\n", file = output_file, append = TRUE)
  cat("Pairs of dragonfly-wq data where dragonfly is absent:", pairs_absent, "\n", file = output_file, append = TRUE)
  total <- pairs_absent + pairs_present
  cat("Total pairs of dragonfly-wq data:", total, "\n", file = output_file, append = TRUE)
  firth_model <- logistf(dragonfly_within_500m_1yr ~ scaled_result,
                         data = sf_wq_proj)
  
  ci_failed <- any(is.infinite(firth_model$ci.lower) | is.infinite(firth_model$ci.upper) | 
                     is.na(firth_model$ci.lower) | is.na(firth_model$ci.upper))
  
  if (ci_failed) {
    writeLines("CI couldn't be calculated", output_file, sep = "\n", append = TRUE)
  }
  
  p_value <- firth_model$prob[-1]
  ci_lower <- firth_model$ci.lower[-1]
  ci_upper <- firth_model$ci.upper[-1]
  coef <- firth_model$coefficients[-1]
  p_sig <- FALSE
  if (p_value < 0.05){
    p_sig <-  TRUE
  }
  sig <- FALSE
  ci_crosses_zero <- (ci_lower * ci_upper) < 0
  if (p_sig && !ci_crosses_zero){
    sig <- TRUE
  }
  cat("Results:\n Coefficients: ", coef, "\n", 
      "Exponent of coefficient: ", exp(coef), "\n",
      "As a percentage change: ", (100 * ((exp(coef)) -1)), "\n",
      "CIs: lower: ", ci_lower, "upper: ", ci_upper, "\n",
      "p value: ", p_value, "\n",
      "Significance*: ", sig, "\n", 
      "* Considered significant if p <0.05 and CIs do not cross 0. \n",
      file = output_file, append = TRUE)
}
  
list_of_species <- list("Calopteryx virgo", "Calopteryx splendens",
                        "Libellula fulva", "Cordulegaster boltonii",
                        "Platycnemis pennipes", "Gomphus vulgatissimus")

list_of_chems <- list("Temp Water", "Ammonia(N)", "BOD ATU", "pH", "O Diss %sat",
                      "Orthophospht", "N Oxidised", "Cond @ 25C", "Sld Sus@105C")

for (species in list_of_species) {
 for (chem in list_of_chems) {
   run_analysis_per_species(species, chem)
 }
}

short_list_of_species <- list("Calopteryx virgo", "Calopteryx splendens")

short_list_of_chems <- list("Temp Water")

for (species in short_list_of_species) {
  for (chem in short_list_of_chems) {
    run_analysis_per_species(species, chem)
  }
}
