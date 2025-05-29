# Riverine Odonata Project Code
# 28/05/2025 Emily Seccombe
# This file imports riverine Odonata presence-only data from the British 
# Dragonfly Society Recording Scheme dataset on NBN Atlas. The data is then
# sorted for use in analysis.

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
library(readr)
riverine_odonata <- read_csv("G:/My Drive/Research project/riverine_odonata.csv")

# Clean Dragonfly Data ----
install.packages("tidyverse")
library(tidyverse)
# Select columns required
r_o_selected_headers <- select(riverine_odonata, lifeStage, eventDate, gridReference, scientificName)

# Look at life stage variety:
lifeStageVariety <- table(r_o_selected_headers$lifeStage)
view(lifeStageVariety)

# Convert to R Date format
#TODO check the next step deals with all eventualities correctly!
r_o_selected_headers$eventDate <- as.Date(r_o_selected_headers$eventDate)
# Remove anything before 2010
r_o_since2010 <- r_o_selected_headers %>% filter(year(eventDate) > "2010")

# Look at species variety:
spVariety <- table(r_o_since2010$scientificName)
view(spVariety)
# Maybe not enough of some species - to check later 

# Look at grid refs and their precision:
grVariety <- table(r_o_since2010$gridReference)
view(grVariety)
r_o_since2010$grPrecision <- nchar(r_o_since2010$gridReference)
grPresVariety <- table(r_o_since2010$grPrecision)
view(grPresVariety)
# Remove entries where precision is less than 6 figures.
r_o_since2010_more_precise <- r_o_since2010 %>% filter(grPrecision > 5)

# Import water quality data ----
