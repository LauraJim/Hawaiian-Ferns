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

