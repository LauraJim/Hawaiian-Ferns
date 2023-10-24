## Extracting climatic layers

# First version: October, 2022
# Last update: Febraury 2023

# Packages----------
library(tidyverse)
library(data.table)
library(ggplot2)
library(terra)
library(sf)

## EXTRACTING CLIMATIC VALUES ----------------------

# Read & stack climatic layers 

# Diffuse Radiation
drad <- rast("./Climate layers/dif_rd_ann.txt")
# Leaf Area Index
lai <- rast("./Climate layers/lai_ann.txt")
# Relative Humidity
humidity <- rast("./Climate layers/rh_ann.txt")
# SD of monthly precipitation * 100
season <- rast("./Climate layers/seasonality.grd")
# Average temperature of the hottest four months of the year
hotmean <- rast("./Climate layers/new_clim_vars/hot_mean.grd")
# Average temperature of the driest three months (June, July, August)
dry <- rast("./Climate layers/dry_avg.grd")

# Stack layers
climate <- c(drad, lai, humidity, season, hotmean, dry)

# plot and check that we read the correct layers
plot(climate)

# Read shapefile with island boundaries
hawaii <- st_read("./hawaii-islands/Island_boundaries.shp")
plot(hawaii)
# check whether we still these two lines:
#hawaii.tr <- spTransform(hawaii, CRS("+proj=longlat +datum=WGS84"))
#hawaii.fort <- fortify(hawaii.tr) 

# Get random sample of points inside the study area 
sample <- as.data.frame(spatSample(climate, 8000, "random", xy=T,
                     na.rm=TRUE, as.points=TRUE))
#sample <- data.frame(rs.inE(region=hawaii.tr,N=10000,Estck=climate))
colnames(sample) <- c("dif_rad","leaf_ai","rel_humidity",
                      "seasonality","hot_mean","dry_avg")

# Save environmental conditions of random sample
write.csv(sample,"./Climate layers/8Kpnts_hawaii_clim_values.csv",
          row.names = F)

# Climatic values for occurrences ------------------------
occ.data <- read_csv("./Occurrences/occurrences_cleaned_9feb2023.csv")

# Extract climatic values at occurrence points
occ.clim <- extract(climate,occ.data[,5:6], ID=F)
occ.complete <- cbind(occ.data,occ.clim)
colnames(occ.complete) <- c(colnames(occ.data),
                            "dif_rad","leaf_ai","rel_humidity",
                            "seasonality","hot_mean","dry_avg")

# Save table
write.csv(occ.complete,"./Occurrences/occurrences_with_clim_values.csv",
          row.names = F)


# END
