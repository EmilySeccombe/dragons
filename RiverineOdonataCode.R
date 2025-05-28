# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
TODO: check what else should go here

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

# Import Data
library(readr)
riverine_odonata <- read_csv("G:/My Drive/Research project/riverine_odonata.csv")

# Clean Data
install.packages("tidyverse")
library(tidyverse)
# Select columns required
r_o_selected_headers <- select(riverine_odonata, lifeStage, eventDate, habitat, locationID, higherGeographyID, waterBody, geodeticDatum, coordinatePrecision, gridReference, scientificName)

# Look at life stage variety:
lifeStageVariety <- table(r_o_selected_headers$lifeStage)
view(lifeStageVariety)

# Look at record dates:
eventDateVariety <- table(r_o_selected_headers$eventDate)
view(eventDateVariety)
# Remove anything before 2010
TODO
r_o_selected_headers %>% filter(eventDate > '2010-01-01')
