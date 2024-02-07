## Extracting climatic layers &
# Correlation plots for species: D_decora, D_decipiens, C_viridis, D_angelica,
#                                   P_austroamericana, P_calomelanos, P_ternifolia 

# First version: October, 2022

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
sample <- as.data.frame(spatSample(climate, 10000, "random", xy=T,
                     na.rm=TRUE, as.points=TRUE))
#sample <- data.frame(rs.inE(region=hawaii.tr,N=10000,Estck=climate))
colnames(sample) <- c("dif_rad","leaf_ai","rel_humidity",
                      "seasonality","hot_mean","dry_avg")

# Save environmental conditions of random sample
write.csv(sample,"./Climate layers/10Kpnts_hawaii_clim_values.csv",
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


# Correlation --------------------------------------------------------

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

#pch1 <- ifelse(y==0,20,19)
col1 <- y+1
mypanel <- function(x,y,col=col1,pch=20) {
  points(x,y,col=col1,pch=20)
}

# legend names 
names <- c("Random sample",sort(unique(occ.complete[,1])))
names <- gsub("_", " ", names)

## Plot random sample with occurrences

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
# END
