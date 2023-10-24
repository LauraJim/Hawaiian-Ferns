# Create new climatic variables of temperature and precipitation

library(raster)
library(reshape2)
library(ggplot2)

setwd("new_clim_var")

Tmin <- stack("Monthly_Tminstack/Monthly_Tminstack.tif" )
Tmax <- stack("Monthly_Tmaxstack/Monthly_Tmaxstack.tif" )

# COLD 
Tmin_data <- vector("list" , length = 12)

for (i in 1:length(Tmin_data)) {
  
  print(i)
  Tmin_data[[i]] <- matrix(Tmin@layers[[i]])
  
}

names(Tmin_data) <- c("Jan.", "Feb", "March","April", "May", "June", 
                      "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")
boxplot(Tmin_data)

# mean of Dec, Jan., Feb., and March:
cold <- Tmin[[c(1,2,3,12)]]
cold_mean <- calc(cold, mean)

# now for HOT 
Tmax_data <- vector("list" , length = 12)

for (i in 1:length(Tmax_data)) {
  
  print(i)
  Tmax_data[[i]] <- matrix(Tmax@layers[[i]])
  
}

names(Tmax_data) <- c("Jan.", "Feb", "March","April", "May", "June", 
                      "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")
boxplot(Tmax_data)

# mean of July, Aug, Sept., Oct. 
hot <- Tmax[[c(7,8,9,10)]]
hot_mean <- calc(hot, mean)

# export these as raster data stuff 
writeRaster(cold_mean, "cold_mean")
writeRaster(hot_mean, "hot_mean")

# get rainfall variables
# 1. seasonality (variance)
# 2. max rainfall averaged over wettest 3 months
# 3. min rainfall averaged over driest 3 months 

# get list of files 
files <- list.files("StateASCIIGrids_mm/", full.names = TRUE)
files <- files[grep(".txt", files)]
files <- files[-grep("ann", files)]

# read in rasters and store as a rasterstack
rainfall <- vector("list", length = length(files))
for (i in 1:length(files)) {
  rainfall[[i]] <- raster(files[i])
}
rainfall_stack <- stack(rainfall)

# get the sd*100 (to mirror worldclim)
seasonality <- calc(rainfall_stack, sd)
seasonality <- seasonality * 100

# save seasonality 
writeRaster(seasonality, "seasonality")

# transform rasterstack to list of matrices
rainfall_matrices <- vector("list", length = length(rainfall))
for (i in 1:length(rainfall)){
  rainfall_matrices[[i]] <- matrix(rainfall[[i]])
}

names(rainfall_matrices) <- c("Jan", "Feb", "March", "April",
                              "May", "June", "July", "Aug",  
                              "Sept", "Oct", "Nov", "December")

# plot months to pick wet and dry 
boxplot(rainfall_matrices)

# wet months: November, December, January
# dry months: June, July, August 

wet <- rainfall_stack[[c(1, 11, 12)]]
dry <- rainfall_stack[[c(6, 7, 8)]]

wet_avg <- calc(wet, mean)
dry_avg <- calc(dry, mean)

writeRaster(wet_avg, "wet_avg")
writeRaster(wet_avg, "dry_avg")
