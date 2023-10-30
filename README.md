# Hawaiian-Ferns

## Conservation applications of ecological niche modeling: non-native ferns compete with native ferns for limited suitable habitat in Hawaiian dryland ecosystems

by Krystalyn Edwards-Calma, Laura Jiménez, Rosana Zenil-Ferguson, Karolina Heyduk, Miles K. Thomas, and Carrie M. Tribble

This repository provides all the R code used to prepare data, make analyses, and produce figures for the manuscript.

- 00_Niche_functions.R: contains auxiliary functions needed to reproduce all the analyses and can be used to adapt our methodology to other studies.

- 01_Create_climate_layers.R: code used to create temperature and precipitation-related variables from monthly means.

- 02_Data_cleaning_Correlation.R: data cleaning process applied to occurrence data. This includes extracting climate values from all the candidate climatic variables and calculating pairwise correlations to discard correlated climate variables.

- 03_Extracting_climatic_values.R: once we selected the final occurrence points and variables to be used in the ecological niche modeling exercise, we extracted the climatic values at each occurrence site (for native and non-native species) and random sample selected from the study area, and created the data tables needed for further analyses.

- 04_Niche_modeling.R: we applied the methodology proposed by Jiménez & Soberón (2022) to estimate the fundamental niches of the native fern species, using their corresponding occurrences and the random sample representing the climate availability in the Hawaiian islands. This file includes code to estimate and save the parameters of interest, create plots of the estimated niches in environmental space, and create a raster with the model's output converted into a suitability index.

- 05_Suitability_maps.R: code used to create continuous suitability maps that showcase the model's output. These continuous maps are then converted to binary maps where a value of zero represents sites outside the estimated fundamental niche, and a positive value is assigned to sites with suitable conditions for the survival of the species. We combined the output of the two models of interest, added occurrence data from non-native species to the maps, and calculated the percentage of native and non-native occurrences inside the predicted potential distributions.

- 06_LandCover_Hawaii.R: the first part of this script shows how we reclassified the land-cover raster into two different classifications of interest. Then, we combined the new land-cover maps with the binary maps of models 1 and 2 and calculated the intersection between the suitable area and each of the land-cover types in each classification.

- Occurrences folder: contains a CSV file with the occurrence data of all the native and non-native fern species, except *D. angelica* for which we do not share the data given conservation concerns.
