# Data Cleaning
# First version: October, 2022
# Last updated: September 2023

# Packages----------
library(tidyverse)
library(data.table)
library(rgdal)
library(ggplot2)
library(raster)
library(sf)
library(corrplot)

# DATA CLEANING 1 -------------------------------------------------------------
# Read the occurrence data
occ.data <- read_csv("./Occurrences/occurrences_cleaned.csv")
colnames(occ.data) <- c("island","species","year","source","long","lat")

plot(occ.data$long,occ.data$lat,pch=15,col=factor(occ.data$species))

# Read shapefile with island boundaries
hawaii <- readOGR("./Hawaiian_islands/Island_boundaries.shp")
plot(hawaii)

hawaii.tr <- spTransform(hawaii, CRS("+proj=longlat +datum=WGS84"))
hawaii.fort <- fortify(hawaii.tr) 

# Plot islands and add occurrences to check if they fall within the islands
p1 <- ggplot() +
  geom_polygon(data=hawaii.fort,aes(long, lat, group = group),fill="lavender") +
  theme_bw()
p1

p2 <- p1 +
  geom_point(data = occ.data, aes(x = long, y = lat, color = species))
p2  
# Get rid of observations outside of the islands --------
occ.spt <- st_as_sf(occ.data, coords = c("long","lat"),
                    crs=CRS("+proj=longlat +datum=WGS84"))
occ.inhi <- st_intersection(st_as_sf(hawaii.tr), occ.spt)
clean.coords <- st_coordinates(occ.inhi)

occ.clean <- data.frame(island=occ.inhi$Island, species=occ.inhi$species,
                        year=occ.inhi$year, source=occ.inhi$source,
                        lon=clean.coords[,1],
                        lat=clean.coords[,2])
write.csv(occ.clean,"./Occurrences/occurences_cleaned.csv", row.names = F)

# Make a new plot with cleaned occurrences-------
p3 <- ggplot() +
  geom_sf(data = st_as_sf(hawaii.tr),fill="grey70") +
  geom_sf(data = occ.inhi, aes(color = species), size = 2) + 
  scale_color_manual(name = 'Species',
                     values = c(alpha(colour =  'pink2', alpha = 0.5),
                                alpha(colour = 'darkgreen', alpha = 0.5),
                                alpha(colour = 'purple1', alpha = 0.5),
                                alpha(colour = 'coral1', alpha = 0.5),
                                alpha(colour = 'firebrick2', alpha = 0.5),
                                alpha(colour = 'darkorange', alpha = 0.5),
                                alpha(colour = 'deepskyblue', alpha = 0.5)),
                     labels = c("D. decipiens", "D. decora", "D. angelica", "P. austroamericana",
                                "P. calomelanos", "C. viridis", "P. ternifolia")) +
  labs(x = 'Longitude', y = 'Latitude', title = "Occurrences of Dry-adapted species in Hawai'i") + 
  theme_bw()
p3



## EXTRACTING ALL CLIMATIC VALUES ----------------------

# Read & stack climatic layers 

# Diffuse Radiation
drad <- rast("./Climatic_layers/dif_rd_ann.txt")
# Leaf Area Index
lai <- rast("./Climatic_layers/lai_ann.txt")
# Relative Humidity
humidity <- rast("./Climatic_layers/rh_ann.txt")
# SD of monthly precipitation * 100
season <- rast("./Climatic_layers/seasonality.grd")
# Average temperature of the hottest four months of the year
hotmean <- rast("./Climatic_layers/hot_mean.grd")
# Average temperature of the driest three months (June, July, August)
dry <- rast("./Climatic_layers//dry_avg.grd")
#Average temperature of the coldest four months of the year
cold <- rast("./Climatic_layers/cold_mean.grd")
# Average temperature of the wettest three months
wet_avg <- rast("./Climatic_layers/wet_avg.grd")
#Soil Evaporation
s_evap <- rast("./Climatic_layers/se_mm_ann.txt")
#Soil Radiation
s_rad <- rast("./Climatic_layers/cl_sw_ann.txt")
#Soil Moisture
s_mois <- rast("./Climatic_layers/sl_mst_ann.txt")
#Area evaporation
a_evap <- rast("./Climatic_layers/aet_mm_ann.txt")

# Stack layers
climate <- c(drad, lai, humidity, season, hotmean, dry, cold, wet_avg, s_evap, s_rad, s_mois, a_evap)
rm(drad, lai, humidity, season, hotmean, dry, cold, wet_avg, s_evap, s_rad, s_mois, a_evap)

# Get random sample of points inside the study area 
sample <- as.data.frame(spatSample(climate, 8000, "random", xy=T,
                                   na.rm=TRUE, as.points=TRUE, 
                                   exhaustive = TRUE))

colnames(sample) <- c("dif_rad","leaf_ai","rel_humidity",
                      "seasonality","hot_mean","dry_avg","cold_mean",
                      "wet_avg","s_evap","s_rad","s_mois","a_evap")

# Save environmental conditions of random sample
write.csv(sample,"./Climatic_layers/8Kpnts_hawaii_allclim_values.csv", row.names = F)

# Climatic values for occurrences ------------------------
occ.data <- read_csv("./Occurrences/occurences_cleaned_9feb2023.csv")

# Extract climatic values at occurrence points
occ.clim <- extract(climate,occ.data[,5:6], ID=F) # cols 5 and 6 are lat and lon
# add lon lat columns to dataframe
occ.complete <- cbind(occ.data[,c(2,5:6)],occ.clim) 
# rename columns for clarity
colnames(occ.complete) <- c("species", "lon","lat","dif_rad","leaf_ai","rel_humidity",
                            "seasonality","hot_mean","dry_avg","cold_mean",
                            "wet_avg","s_evap","s_rad","s_mois","a_evap")
rm(occ.clim)

# Save table
write.csv(occ.complete,"./Occurrences/occurrences_with_allclim_values.csv", row.names = F)


# remove any NA values from occurrence climate extracted points 
occ.complete <- na.omit(occ.complete)

head(occ.complete[,4:15])

# Combine climatic values for occurrences AND samples
clim.values <- rbind(sample,occ.complete[,4:15])

x <- as.numeric(factor(as.matrix(occ.complete[,1])))
y <- c(rep(0,nrow(sample)),x)

# set color palette 
my_favorite_colors <- c("black", "chartreuse1", "sienna1", "darkorchid1",
                        "deepskyblue1", "gold1", "red1", "aquamarine1")
palette(my_favorite_colors)

col1 <- y+1
mypanel <- function(x,y,col=col1,pch=20) {
  points(x,y,col=col1,pch=20)
}

# legend names 
names <- c("Random sample",sort(unique(occ.complete[,1])))
names <- gsub("_", " ", names)

## Plot 5k sample with occurrences
pdf("~/Desktop/all_vars_occ_and_sample.pdf", height = 15, width = 15)
pairs(clim.values,lower.panel=NULL, panel=mypanel)
par(xpd = TRUE)
legend("bottomleft", 
       fill = my_favorite_colors, 
       legend = names,
       bty = "n",
       cex = 2)
dev.off()

## subset to selected 6 variables 
clim.values.selected <- clim.values[, c(1:6)] 

pdf("~/Desktop/all_vars_occ_and_sample_selected.pdf", height = 15, width = 15)
pairs(clim.values.selected,lower.panel=NULL, panel=mypanel)
par(xpd = TRUE)
legend("bottomleft", 
       fill = my_favorite_colors, 
       legend = names,
       bty = "n",
       cex = 2)
dev.off()

