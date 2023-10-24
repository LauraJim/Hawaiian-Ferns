# Data Cleaning & 
# Correlation plots for species: D_decora, D_decipiens, C_viridis, D_angelica,
#                                   P_austroamericana, P_calomelanos, P_ternifolia 
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
occ.data <- read_csv("./Data/occurrences_cleaned.csv")
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
write.csv(occ.clean,"./Data/occurences_cleaned.csv",
          row.names = F)

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


# Correlation (all variables)--------------------------------------------------------
## Plot occurrences as numeric values to see climate correlation
pdf("~/Desktop/corr_occurrences_allvars.pdf", width = 8, height = 8)
Mcor <- cor(as.matrix(occ.complete[,4:15]))
corrplot(Mcor, 
         col = rev(COL2('RdBu', 200)),
         method="number")
dev.off()

## plot the climatic layers chosen
vars.keep <- c("dif_rad","leaf_ai","seasonality","rel_humidity","hot_mean","dry_avg")
clim.dat.subset <- occ.complete[,vars.keep]
Mcor.subset <- cor(as.matrix(clim.dat.subset))

pdf("~/Desktop/corr_occurrences_selectedvars.pdf", width = 8, height = 8)
corrplot(Mcor.subset, 
         col = rev(COL2('RdBu', 200)),
         method="number")
dev.off()

