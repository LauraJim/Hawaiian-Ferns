# Comparing the model outputs when increasing the sample size of the random
# points used to estimate the distribution of climates in the accessible area
# We compare the results from two samples, one with 8000 points and another
# with 10000 points
# Author: Laura Jimenez
# First version: February, 2024

# Packages & Functions --------------------------------------------------
library(tidyverse)

# Load functions
source("00_Niche_functions.R")
# function to binarize the categorical rasters, we want to keep the suitable
# areas identified by both Model 1 and Model 2
rasbin <- function(x) {
  ifelse(x >= 300, 1, 0)
}

dif.map <- function(bmodel){
  # common colors
  seamap <- "aliceblue"
  framemap <- 'grey5'
  
  # Convert raster to data frame for plots
  bm.df <- as.data.frame(bmodel, xy=T)
  colnames(bm.df) <- c("Lon","Lat","Value")
  bm.df$Value <- factor(bm.df$Value, levels=c(-1,0,1))
  vls <- length(unique(bm.df$Value))
  
  # Map for a single species
  if(vls==3){
    basemap <- ggplot(data = bm.df, aes(x=Lon, y=Lat)) +
      geom_tile(aes(fill=Value)) +
      scale_colour_manual(values=c("royalblue", "grey80", "tomato"), aesthetics="fill",
                          guide = guide_legend(order = 1)) +
      theme_void()
  }
  if(vls==2){
    basemap <- ggplot(data = bm.df, aes(x=Lon, y=Lat)) +
      geom_tile(aes(fill=Value)) +
      scale_colour_manual(values=c("grey80", "tomato"), aesthetics="fill",
                          guide = guide_legend(order = 1)) +
      theme_void()
  }
  map <- aloha(spn="", basemap=basemap, dleg=T, axes.leg=F)
  return(map)
}

# Read rasters with niche model outputs ----------------------------------

# output maps when using 8K random points
m8.ange <- rast("./Results-8K-5feb2024/Models1-2/Dangelica-Models12-categorical_95.tif")
m8.deci <- rast("./Results-8K-5feb2024/Models1-2/Ddecipiens-Models12-categorical_95.tif")
m8.deco <- rast("./Results-8K-5feb2024/Models1-2/Ddecora-Models12-categorical_95.tif")
m8.tern <- rast("./Results-8K-5feb2024/Models1-2/Pternifolia-Models12-categorical_95.tif")

# output maps when using 10K random points
m10.ange <- rast("./Results/Models1-2/Dangelica-Models12-categorical_95.tif")
m10.deci <- rast("./Results/Models1-2/Ddecipiens-Models12-categorical_95.tif")
m10.deco <- rast("./Results/Models1-2/Ddecora-Models12-categorical_95.tif")
m10.tern <- rast("./Results/Models1-2/Pternifolia-Models12-categorical_95.tif")

x11()
par(mfrow=c(2,1))
plot(m8.deci)
plot(m10.deci)

# binarize results and calculate the difference between the rasters
# 8K random points
dif.ange <- app(m8.ange, fun=rasbin) - app(m10.ange, fun=rasbin)
dif.deci <- app(m8.deci, fun=rasbin) - app(m10.deci, fun=rasbin)
dif.deco <- app(m8.deco, fun=rasbin) - app(m10.deco, fun=rasbin)
dif.tern <- app(m8.tern, fun=rasbin) - app(m10.tern, fun=rasbin)

x11()
par(mfrow=c(2,2))
plot(dif.ange)
plot(dif.deci)
plot(dif.deco)
plot(dif.tern)

# Apply function to create a map and save
x11()
dif.map(dif.ange)

ggsave(plot=dif.map(dif.ange), width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Dangelica_differences_8K-10K.png")
ggsave(plot=dif.map(dif.deco), width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Ddecora_differences_8K-10K.png")
ggsave(plot=dif.map(dif.deci), width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Ddecipiens_differences_8K-10K.png")
ggsave(plot=dif.map(dif.tern), width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Pternifolia_differences_8K-10K.png")


# END