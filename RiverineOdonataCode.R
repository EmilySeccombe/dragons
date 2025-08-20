# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
# This file imports riverine Odonata presence-only data from the British 
# Dragonfly Society Recording Scheme dataset on NBN Atlas.
# It also imports water quality data available open access online from
# the Environment Agency. 
# Data is compared to assess correlation between water quality and dragonfly
# presence.

# Define output file
output_file <- "G:/My Drive/Research project/odonata_analysis_results.txt"

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
riverine_odonata <- read_csv("G:/My Drive/Research project/dragons/riverine_odonata.csv")

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
r_o_since2010 <- r_o_selected_headers %>% filter(eventDate > anydate('01-01-2024'))

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
ea_water_quality <- read_csv("G:/My Drive/Research project/dragons/2024.csv")
#TODO can try this with 2024 dataset instead; and ultimately combine all data from 20?? onwards

#Select rows of interest
ea_water_quality <- ea_water_quality[ea_water_quality$determinand.label %in% c("Ammonia(N)", "Temp Water", "BOD ATU", "pH", "O Diss %sat",
                      "Orthophospht", "N Oxidised", "Cond @ 25C", "Sld Sus@105C"),]

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
#Convert dataframes to spatial objects
# Select just one species:
run_analysis_per_species <- function(species_name, chemical){
  cat(paste("\nAnalyzing species:", species_name, 
            "with chemical:", chemical, "\n"), 
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
  
  # # Include water quality sample type
  # sf_wq_proj$sample_type <- as.factor(sf_wq_proj$sample.sampledMaterialType.label)
  # # Include water quality sample purpose
  # sf_wq_proj$sample_purpose <- as.factor(sf_wq_proj$sample.purpose.label)
  # # Include water quality year and month
  # sf_wq_proj$sample.sampleDateTime <- as.POSIXct(sf_wq_proj$sample.sampleDateTime)
  # sf_wq_proj$sample_year  <- year(sf_wq_proj$sample.sampleDateTime)
  # sf_wq_proj$sample_month <- month(sf_wq_proj$sample.sampleDateTime, label = TRUE, abbr = TRUE)
  # sf_wq_proj$sample_year <- as.factor(sf_wq_proj$sample_year)
  # sf_wq_proj$sample_month <- as.factor(sf_wq_proj$sample_month)
  #print("Tabels:")
  #print(table(sf_wq_proj$sample_year))
  #print(table(sf_wq_proj$sample_month))
  #print(table(sf_wq_proj$sample_type))
  #print(table(sf_wq_proj$sample_purpose))
  sf_wq_proj$scaled_result <- scale(sf_wq_proj$result)
  
  
  # For each dragonfly record, find index of nearest water quality record
  nearest_idx <- st_nearest_feature(sf_dragonfly_proj, sf_wq_proj)
  
  # Calculate distance to nearest water quality record
  distances <- st_distance(sf_dragonfly_proj, sf_wq_proj[nearest_idx, ], by_element = TRUE)
  
    
  cat("step1:\n", file = output_file, append = TRUE)
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
  
  cat("step2:\n", file = output_file, append = TRUE)
  # Check if there are dragonfly-water quality record pairs within 500m.
  if (nrow(wq_dragonfly_pairs) == 0) {
    cat(paste(paste("No matching dragonfly-water quality pairs within 500m for ", species_name, " and ", chemical, ".")), file = output_file, append = TRUE)
    message("No matching dragonfly-water quality pairs within 500m for ", species_name, " and ", chemical, ".")
    return(NULL)
  }
  
    # Step 3: Add date columns for filtering
  wq_dragonfly_pairs <- wq_dragonfly_pairs %>%
    mutate(
      wq_date = sf_wq_proj$sample.sampleDateTime[wq_index],
      dragonfly_date = sf_dragonfly_proj$eventDate_parsed[dragonfly_index],
      date_diff = abs(as.numeric(difftime(wq_date, dragonfly_date, units = "days")))
    )
  cat("step3:\n", file = output_file, append = TRUE)
  # Step 4: Filter pairs to those within 365 days
  wq_dragonfly_pairs_filtered <- wq_dragonfly_pairs %>%
    filter(date_diff <= 365)
  cat("step4:\n", file = output_file, append = TRUE)
  # Step 5: Mark water quality records that have at least one dragonfly nearby in space AND time
  sf_wq_proj$dragonfly_within_500m_1yr <- FALSE
  sf_wq_proj$dragonfly_within_500m_1yr[unique(wq_dragonfly_pairs_filtered$wq_index)] <- TRUE
  cat("step5:\n", file = output_file, append = TRUE)
  #Following plots should only be used out of for loop unless you want to save them as they're created before the next one!
  #if(viewing_mode){
  #  table_header <- paste("Result for", chemical, "and Presence of", species_name, sep=" ")
  #  boxplot(sf_wq_proj$result~sf_wq_proj$dragonfly_within_500m_1yr,
  #          data=sf_wq_proj,
  #          main=table_header,
  #          xlab="Dragonfly presence",
  #          ylab="Water Quality Result",
  #          col="orange",
  #          border="brown"
  #    )
  #}
  # 
  # 
  # # Base model with just water quality result
  # model <- glm(sf_wq_proj$dragonfly_within_500m_1yr ~ sf_wq_proj$scaled_result,family=binomial)
  # if(viewing_mode){
  #   cat("GLM output:")
  #   print(summary(model)$coefficients)
  # }
  # #Put output into file:
  # glm_summary <- capture.output(summary(model))
  # cat("GLM Output:\n", file = output_file, append = TRUE)
  # cat(paste(glm_summary, collapse = "\n"), file = output_file, append = TRUE)
  # cat("\n---------------------------\n", file = output_file, append = TRUE)
  # 
  # # Full model with all other variables
  # full_model <- glm(
  #   dragonfly_within_500m_1yr ~ scaled_result + sample_type + sample_purpose + sample_month,
  #   data = sf_wq_proj,
  #   family = binomial
  # )
  # if(viewing_mode){
  #   cat("GLM output from full model:")
  #   print(summary(full_model)$coefficients)
  # }
  # #Put output into file:
  # full_glm_summary <- capture.output(summary(full_model))
  # cat("Full GLM Output:\n", file = output_file, append = TRUE)
  # cat(paste(full_glm_summary, collapse = "\n"), file = output_file, append = TRUE)
  # cat("\n---------------------------\n", file = output_file, append = TRUE)
  # 
  # # Do stepwise comparison to get best model
  # best_model <- step(full_model, direction = "both")
  # if(viewing_mode){
  #   cat("GLM output from best model:")
  #   print(summary(best_model)$coefficients)
  # }
  # #Put output into file:
  # best_model_summary <- capture.output(summary(best_model))
  # cat("Full GLM Output:\n", file = output_file, append = TRUE)
  # cat(paste(best_model_summary, collapse = "\n"), file = output_file, append = TRUE)
  # cat("\n---------------------------\n", file = output_file, append = TRUE)
  # 
  # # Compare the models
  # aic_output <- AIC(model, full_model, best_model)
  # 
  # # Create a nicely formatted table
  # aic_table <- capture.output(print(aic_output))
  # 
  # # Write the AIC results to file
  # cat(paste("\nAIC Output for ", species_name, 
  #           " with chemical: ", chemical, "\n"), 
  #     file = output_file, append = TRUE)
  # cat(paste(aic_table, collapse = "\n"), file = output_file, append = TRUE)
  # cat("\n\n", file = output_file, append = TRUE)  # Add spacing
  # 
  # print("here")
  # print(chem)
  # print(species)
  # print(table(sf_wq_proj$dragonfly_within_500m_1yr))
  # print(ggplot(sf_wq_proj, aes(x = scaled_result, y = dragonfly_within_500m_1yr)) +
  #       geom_jitter(alpha = 0.3) +
  #       geom_smooth(method = "glm", method.args = list(family = "binomial")))
  # 
  # GLM identified a full model is best, but for accurate coefficients we need to use the FIrth model due to inbalanced data
  cat("step6:\n", file = output_file, append = TRUE)
  valid_rows <- complete.cases(sf_wq_proj$dragonfly_within_500m_1yr, sf_wq_proj$scaled_result)
  
  if (sum(valid_rows) < 10 || length(unique(sf_wq_proj$dragonfly_within_500m_1yr[valid_rows])) < 2) {
    cat("skipping as not enough data or only one class in outcome:\n", file = output_file, append = TRUE)
    next  # Skip model if not enough data or only one class in outcome
  }
  valid_count <- sum(sf_wq_proj$dragonfly_within_500m_1yr == TRUE & !is.na(sf_wq_proj$scaled_result))
  cat("Valid observations for this test:", valid_count, "\n", file = output_file, append = TRUE)
  firth_model <- logistf(dragonfly_within_500m_1yr ~ scaled_result,
                         data = sf_wq_proj)
  cat("step7:\n", file = output_file, append = TRUE)
  firth_summary <- capture.output(summary(firth_model))
  cat("step8:\n", file = output_file, append = TRUE)
  cat("Firth Output:\n", file = output_file, append = TRUE)
  cat(paste(firth_summary, collapse = "\n"), file = output_file, append = TRUE)
  cat("\n---------------------------\n", file = output_file, append = TRUE)
  cat("step9:\n", file = output_file, append = TRUE)
  exp_coef <- exp(coef(firth_model))
  conf_int <- exp(confint(firth_model))
  #TODO
}
  
list_of_species <- list("Calopteryx virgo", "Calopteryx splendens",
                        "Libellula fulva", "Cordulegaster boltonii",
                        "Platycnemis pennipes", "Gomphus vulgatissimus")

list_of_chems <- list("Ammonia(N)", "Temp Water", "BOD ATU", "pH", "O Diss %sat",
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

# Based on results:
# p values with < 0.05 and CIs do not cross 0:# 

#Calopteryx virgo with chemical: pH
# 1464
# Model fitted by Penalized ML
# Coefficients:
#                 coef   se(coef) lower 0.95  upper 0.95    Chisq
# (Intercept)   -3.8172153 0.02662170 -3.8698372 -3.76547174      Inf
# scaled_result -0.1310987 0.02326493 -0.1759429 -0.08464531 28.90203
# p method
# (Intercept)   0.000000e+00      2
# scaled_result 7.613308e-08      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=28.90203 on 1 df, p=7.613308e-08, n=67501
# Wald test = 20704.51 on 1 df, p = 0


# Calopteryx splendens with chemical: pH
# 3040
# Model fitted by Penalized ML
# Coefficients:
#                coef   se(coef) lower 0.95 upper 0.95 Chisq p method
# (Intercept)   -3.1038319 0.01942006 -3.1421333  -3.066001   Inf 0      2
# scaled_result  0.3516652 0.02069077  0.3111765   0.392325   Inf 0      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=300.3209 on 1 df, p=0, n=67501
# Wald test = 26450.77 on 1 df, p = 0

#Calopteryx splendens with chemical: N Oxidised
# 2640
# Model fitted by Penalized ML
# Coefficients:
#                   coef   se(coef) lower 0.95 upper 0.95 Chisq p method
# (Intercept)   -3.0737396 0.02014987 -3.1134741 -3.0344833   Inf 0      2
# scaled_result  0.1935793 0.01673065  0.1605335  0.2262399   Inf 0      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=135.242 on 1 df, p=0, n=58806
# Wald test = 23365.71 on 1 df, p = 0


# Libellula fulva with chemical: Temp Water
#596
# Model fitted by Penalized ML
# Coefficients:
#                coef   se(coef) lower 0.95 upper 0.95   Chisq            p method
# (Intercept)   -5.0179031 0.04210377  -5.101618 -4.9365206     Inf 0.000000e+00      2
# scaled_result  0.2299029 0.04013536   0.150749  0.3083044 32.0488 1.503478e-08      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=32.0488 on 1 df, p=1.503478e-08, n=88453
# Wald test = 14643.89 on 1 df, p = 0


# Libellula fulva with chemical: pH 
# 350
# Model fitted by Penalized ML
# Coefficients:
#                coef   se(coef)    lower 0.95 upper 0.95    Chisq          p method
# (Intercept)   -5.259742 0.05383468 -5.3671634620  -5.156044      Inf 0.00000000      2
# scaled_result  0.111091 0.05696627  0.0004293585   0.224667 3.871979 0.04909859      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=3.871979 on 1 df, p=0.04909859, n=67501
# Wald test = 9621.343 on 1 df, p = 0
# 
# Libellula fulva with chemical: O Diss %sat 
# 300
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef) lower 0.95 upper 0.95    Chisq            p method
# (Intercept)   -5.3736531 0.05897795 -5.4915640 -5.2602598      Inf 0.000000e+00      2
# scaled_result -0.2093018 0.04777442 -0.3000276 -0.1121534 16.57442 4.677781e-05      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=16.57442 on 1 df, p=4.677781e-05, n=63657
# Wald test = 8501.439 on 1 df, p = 0
# 
# Libellula fulva with chemical: N Oxidised
#304
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef)  lower 0.95 upper 0.95   Chisq          p method
# (Intercept)   -5.270423 0.05778142 -5.38583555 -5.1592389     Inf 0.0000e+00      2
# scaled_result  0.105463 0.01915628  0.07023117  0.1553255 27.7388 1.3885e-07      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=27.7388 on 1 df, p=1.3885e-07, n=58806
# Wald test = 8322.352 on 1 df, p = 0

# Cordulegaster boltonii with chemical: Temp Water 
# 397
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef)  lower 0.95 upper 0.95    Chisq            p method
# (Intercept)   -5.418306 0.05116613 -5.52034947 -5.3196952      Inf 0.000000e+00      2
# scaled_result  0.195055 0.04935093  0.09764392  0.2914783 15.31406 9.103637e-05      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=15.31406 on 1 df, p=9.103637e-05, n=88453
# Wald test = 11481.86 on 1 df, p = 0

# Cordulegaster boltonii with chemical: pH 
#487 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef) lower 0.95 upper 0.95    Chisq           p method
# (Intercept)   -4.9573430 0.04667625 -5.0502632 -4.8672221      Inf 0.00000e+00      2
# scaled_result -0.2448382 0.03358388 -0.3085179 -0.1764271 42.13705 8.50956e-11      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=42.13705 on 1 df, p=8.50956e-11, n=67501
# Wald test = 11579.61 on 1 df, p = 0

# Cordulegaster boltonii with chemical: N Oxidised 
#306 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef  se(coef) lower 0.95 upper 0.95    Chisq            p method
# (Intercept)   -5.2724034 0.0586923 -5.3898608 -5.1596160      Inf 0.0000000000      2
# scaled_result -0.2565875 0.0816071 -0.4230945 -0.1017687 11.01428 0.0009041246      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=11.01428 on 1 df, p=0.0009041246, n=58806
# Wald test = 8366.503 on 1 df, p = 0

# Cordulegaster boltonii with chemical: Cond @ 25C
# 349 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef) lower 0.95 upper 0.95 Chisq p method
# (Intercept)   -5.447852 0.08129931  -5.612236  -5.293021   Inf 0      2
# scaled_result -3.798768 0.45356467  -4.700906  -2.919723   Inf 0      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=83.67876 on 1 df, p=0, n=55575
# Wald test = 8532.2 on 1 df, p = 0

# Platycnemis pennipes with chemical: pH 
# 532 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef) lower 0.95 upper 0.95 Chisq p method
# (Intercept)   -4.9444329 0.04705274 -5.0380917 -4.8535425   Inf 0      2
# scaled_result  0.5011964 0.04023426  0.4210019  0.5793747   Inf 0      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=129.6953 on 1 df, p=0, n=67501
# Wald test = 11895.84 on 1 df, p = 0
# 

# Platycnemis pennipes with chemical: O Diss %sat 
# 491 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef)  lower 0.95 upper 0.95    Chisq            p method
# (Intercept)   -4.8657843 0.04566007 -4.95666757 -4.7775481      Inf 0.0000000000      2
# scaled_result  0.1474047 0.04017564  0.06165665  0.2225215 10.89841 0.0009624695      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=10.89841 on 1 df, p=0.0009624695, n=63657
# Wald test = 11464.37 on 1 df, p = 0

# Gomphus vulgatissimus with chemical: BOD ATU 
# 65 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef    se(coef)   lower 0.95  upper 0.95    Chisq          p method
# (Intercept)   -7.27921294 0.123574152 -7.531595126 -7.04638436      Inf 0.00000000      2
# scaled_result  0.02718557 0.007590577  0.001387752  0.04061904 4.040963 0.04440848      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=4.040963 on 1 df, p=0.04440848, n=95072
# Wald test = 3470.555 on 1 df, p = 0

# Gomphus vulgatissimus with chemical: pH 
# 85 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef  se(coef) lower 0.95 upper 0.95    Chisq            p method
# (Intercept)   -6.732780 0.1138753 -6.9657350 -6.5166809      Inf 0.0000000000      2
# scaled_result  0.388231 0.1015696  0.1637266  0.5731608 11.11984 0.0008540899      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=11.11984 on 1 df, p=0.0008540899, n=67501
# Wald test = 3747.758 on 1 df, p = 0
# 
# 
# Gomphus vulgatissimus with chemical: Sld Sus@105C
#76 pairs
# Model fitted by Penalized ML
# Coefficients:
#   coef    se(coef)   lower 0.95  upper 0.95    Chisq         p method
# (Intercept)   -6.79889993 0.114392757 -7.031791857 -6.58274613      Inf 0.0000000      2
# scaled_result  0.02507896 0.007284493  0.002055847  0.03815203 4.234525 0.0396097      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=4.234525 on 1 df, p=0.0396097, n=68692
# Wald test = 3533.003 on 1 df, p = 0