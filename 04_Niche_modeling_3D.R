# Estimating the fundamental niche of a species in a 3D environmental space
# Author: Laura Jimenez
# First version: June, 2022
# Last modification: July, 2023

# Packages ----------------------------
library(tidyverse)
library(rgl)

# Load functions
source("00_Niche_functions.R")

# MAIN -------------------------------------------------

# Read PRESENCE data
occs <- na.omit(read_csv("./Occurrences/occurrences_with_clim_values.csv",
                 col_names = T))
# scale variable
occs$seasonality <- occs$seasonality/100
# identify rows such that leaf_ai==0
#laocc.ind <- which(occs$leaf_ai==0)

# species names
fsp <- factor(occs$species)
spname <- levels(fsp)
# observations per species
table(fsp)
# split by species
viridis <- filter(occs, species == spname[1])
angelica <- filter(occs, species == spname[2])
decipiens <- filter(occs, species == spname[3])
decora <- filter(occs, species == spname[4])
ternifolia <- filter(occs, species == spname[5])
austroam <- filter(occs, species == spname[6])
calomelanos <- filter(occs, species == spname[7])

# Read random sample of sites within accessible area
Mclim<- na.omit(read_csv("./Climate layers/8Kpnts_hawaii_clim_values.csv",
                 col_names = T))
# scale variable
Mclim$seasonality <- Mclim$seasonality/100
# identify rows such that leaf_ai==0
#laM.ind <- which(Mclim$leaf_ai==0)

# Choose variables to be used
colnames(occs)
colnames(Mclim)

# Fit weighted-Normal model:

# # 2D example with single species -----------------------------
# 
# # model using dif_rad, rel_humidity
# occ.var <- c(7,9)
# M.var <- c(1,3)
# mod.lab <- "Model1"
# # Cheilanthes viridis
# (mod.viri <- fitNiche(E.occ = viridis[,occ.var], E.samM = Mclim[,M.var]))
# 
# # Plot
# # define estimated ellipse
# el <- ellipse::ellipse(x=mod.viri$wn.sigma, centre=mod.viri$wn.mu, level=0.95)
# #el1 <- ellipse::ellipse(x=mod.viri$maha.sigma, centre=mod.viri$maha.mu, level=0.95)
# 
# # set colorpalette
# colpal <- c("grey70", "chartreuse2","chartreuse4","coral2", "coral4")
# 
# x11()
# # plot background points
# plot(Mclim[,M.var],col=colpal[1],pch=1,cex=0.5,
#      main="Fitted model of the fundamental niche")
# # add presence points to the plot
# points(viridis[,occ.var], pch = 19, col=colpal[2]) 
# # ellipse 
# lines(el,col=colpal[3],lwd=2)
# #lines(el1,col="blue",lwd=2)
# # center of fundamental niche
# points(mod.viri$wn.mu[1],mod.viri$wn.mu[2],col=colpal[3],pch=8,cex=1.5) 
# #points(mod.viri$maha.mu[1],mod.viri$maha.mu[2],col="blue",pch=1,cex=1.5) 

# 3D, for all the species ---------------------------------

# For Model 1: dif_rad, rel_humidity, seasonality
occ.var1 <- c(7,9,10)
M.var1 <- c(1,3,4)
mod.lab1 <- "Model1"
# # For Model 2: leaf_ai, hot_mean, dry_avg
occ.var2 <- c(8,11,12)
M.var2 <- c(2,5,6)
mod.lab2 <- "Model2"

# Run either the remainder lines in this section or next section!!!

# Cheilanthes viridis
(mod1.viri <- fitNiche(E.occ = viridis[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.viri <- fitNiche(E.occ = viridis[,occ.var2], E.samM = Mclim[,M.var2]))
# Doryopteris angelica
(mod1.ange <- fitNiche(E.occ = angelica[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.ange <- fitNiche(E.occ = angelica[,occ.var2], E.samM = Mclim[,M.var2]))
# Doryopteris decipiens
(mod1.deci <- fitNiche(E.occ = decipiens[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.deci <- fitNiche(E.occ = decipiens[,occ.var2], E.samM = Mclim[,M.var2]))
# Doryopteris decora
(mod1.deco <- fitNiche(E.occ = decora[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.deco <- fitNiche(E.occ = decora[,occ.var2], E.samM = Mclim[,M.var2]))
# Pellaea ternifolia
(mod1.tern <- fitNiche(E.occ = ternifolia[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.tern <- fitNiche(E.occ = ternifolia[,occ.var2], E.samM = Mclim[,M.var2]))
# Pityrogramma austroamericana
(mod1.aust <- fitNiche(E.occ = austroam[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.aust <- fitNiche(E.occ = austroam[,occ.var2], E.samM = Mclim[,M.var2]))
# Pityrogramma calomelanos
(mod1.calo <- fitNiche(E.occ = calomelanos[,occ.var1], E.samM = Mclim[,M.var1]))
(mod2.calo <- fitNiche(E.occ = calomelanos[,occ.var2], E.samM = Mclim[,M.var2]))

# Save estimated parameters for future analyses (mles)
mles1 <- rbind(unlist(mod1.viri), unlist(mod1.ange), unlist(mod1.deci),
               unlist(mod1.deco), unlist(mod1.tern), unlist(mod1.aust),
               unlist(mod1.calo))
mles1 <- cbind(rep("Model 1", length(spname)), spname, mles1)
colnames(mles1) <- c("MODEL", "Species",
                     paste0("wn.mu",1:3), paste0("wn.sig",1:9),
                     paste0("maha.mu",1:3), paste0("maha.sig",1:9))
mles2 <- rbind(unlist(mod2.viri), unlist(mod2.ange), unlist(mod2.deci),
               unlist(mod2.deco), unlist(mod2.tern), unlist(mod2.aust),
               unlist(mod2.calo))
mles2 <- cbind(rep("Model 2", length(spname)), spname, mles2)
colnames(mles2) <- colnames(mles1)

all.mles <- rbind(mles1, mles2)
write.csv(all.mles, "./Results/Estimated_parameters_allSpecies_M1M2.csv", row.names = F)

# Run instead of previous section -----------------------------
# All the resuts generated above were saved into this .RData file
load("MLEs_saved.RData")

# PLOTS in a 3D niche space ----------------------------------------

# Use the following code to plot the presence points and the 95% confidence
# ellipse that represents the border of the fundamental niche, for all the species

# For one species: comment the lines used to plot ellipses of other species,
# also, comment line 132 and uncomment line 133 modified according to the species

# For a different model: change all the instances of 'mod.species'

niches3D_m1 <- function(pback=TRUE,col.oc, sh.oc, a.el, col.el){
  # plot points from calibration area
  if(pback==T){
    plot3d(Mclim[,M.var1], col="grey", pch=1, cex=0.5)
  } else{
    plot3d(Mclim[,M.var1], type="n")
  }
  
  # add occurrences for each species
  pch3d(occs[,occ.var1], pch = sh.oc[fsp], col = col.oc[fsp], cex=0.2)

  #add estimated ellipsis
  # Doryopteris angelica
  wire3d(ellipse3d(x=mod1.ange$maha.sigma, centre = mod1.ange$maha.mu),
         alpha = a.el, col = col.el[2], lit=F)
  # Doryopteris decipiens
  wire3d(ellipse3d(x=mod1.deci$wn.sigma, centre = mod1.deci$wn.mu),
         alpha = a.el, col = col.el[3], lit=F)
  # Doryopteris decora
  wire3d(ellipse3d(x=mod1.deco$wn.sigma, centre = mod1.deco$wn.mu),
         alpha = a.el, col = col.el[4], lit=F)
  # Pellaea ternifolia
  wire3d(ellipse3d(x=mod1.tern$wn.sigma, centre = mod1.tern$wn.mu),
         alpha = a.el, col = col.el[5], lit=F)
}

niches3D_m2 <- function(pback=TRUE,col.oc, sh.oc, a.el, col.el){
  # plot points from calibration area
  if(pback==T){
    plot3d(Mclim[,M.var2], col="grey", pch=1, cex=0.5)
  } else{
    plot3d(Mclim[,M.var2], type="n")
  }
  
  # add occurrences for each species
  pch3d(occs[,occ.var2], pch = sh.oc[fsp], col = col.oc[fsp], cex=0.2)
  #pch3d(viridis[,occ.var], pch = sh.oc[1], col = col.oc[1], cex=0.2)
  
  #add estimated ellipsis
  # Doryopteris angelica
  wire3d(ellipse3d(x=mod2.ange$wn.sigma, centre = mod2.ange$wn.mu),
         alpha = a.el, col = col.el[2], lit=F)
  # Doryopteris decipiens
  wire3d(ellipse3d(x=mod2.deci$wn.sigma, centre = mod2.deci$wn.mu),
         alpha = a.el, col = col.el[3], lit=F)
  # Doryopteris decora
  wire3d(ellipse3d(x=mod2.deco$wn.sigma, centre = mod2.deco$wn.mu),
         alpha = a.el, col = col.el[4], lit=F)
  # Pellaea ternifolia
  wire3d(ellipse3d(x=mod2.tern$maha.sigma, centre = mod2.tern$maha.mu),
         alpha = a.el, col = col.el[5], lit=F)
}

# Plot the environmental space with the background points,
# occurrence points, and the estimated ellipsis

# set colorpalette
col.el <- c("chartreuse4", "sienna3", "darkorchid4",
            "deepskyblue4", "gold4", "red4", "aquamarine4")
# species order: viridis, angelica, decipiens, decora, 
# ternifolia, austroamericana, calomelanos
col.oc <- c("chartreuse1", "sienna1", "darkorchid1",
            "deepskyblue4", "gold1", "red1", "aquamarine1")
sh.oc <- c(0,19,17,15,18,2,5)
a.el <- 0.2

# 3D plot Model 1
niches3D_m1(pback=F,col.oc, sh.oc, a.el, col.el)
# add legend (optional)
#legend3d("right", pch = sh.oc, col = col.oc, legend = spname)

rgl.snapshot('./Results/M1-niches3d_27july2023_plot2.png', fmt = 'png')

# # Save the current viewpoint
# view <- niches1("userMatrix")
# # Restore the saved viewpoint
# niches1(userMatrix = view)

# 3D plot Model 2
niches3D_m2(pback=F,col.oc, sh.oc, a.el, col.el)
# add legend (optional)
legend3d("right", pch = sh.oc, col = col.oc, legend = spname)

rgl.snapshot('./Results/M2-niches3d_27july2023_plot3.png', fmt = 'png')

# PLOTS in geography ----------------------------------------
# First calculate suitability index for the region of interest
# using the estimated parameters

# Read & stack climatic layers 
# Diffuse Radiation
drad <- rast("./Climate layers/dif_rd_ann.txt")
# Leaf Area Index
lai <- rast("./Climate layers/lai_ann.txt")
# Relative Humidity
humidity <- rast("./Climate layers/rh_ann.txt")
# SD of monthly precipitation * 100
season <- rast("./Climate layers/seasonality.grd") / 100
# Average temperature of the hottest four months of the year
hotmean <- rast("./Climate layers/new_clim_vars/hot_mean.grd")
# Average temperature of the driest three months (June, July, August)
dry <- rast("./Climate layers/dry_avg.grd")

# Stack layers
# For Model 1: dif_rad, rel_humidity, seasonality
clim.m1 <- c(drad, humidity, season)
# For Model 2: leaf_ai, hot_mean, dry_avg
clim.m2 <- c(lai, hotmean, dry)

# Calculate suitability raster for each species and each model
# Cheilanthes viridis
smap1.viri <- niche.G(Estck=clim.m1, mu=mod1.viri$wn.mu, Sigma = mod1.viri$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_viridis.tif")
smap2.viri <- niche.G(Estck=clim.m2, mu=mod2.viri$wn.mu, Sigma = mod2.viri$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_viridis.tif")
# Doryopteris angelica
# we'll use Mahalanobis model as Model 1
smap1.ange <- niche.G(Estck=clim.m1, mu=mod1.ange$maha.mu, Sigma = mod1.ange$maha.sigma,
                      save.ras="./Results/Model1/Suitability_m1_angelica.tif")
smap2.ange <- niche.G(Estck=clim.m2, mu=mod2.ange$wn.mu, Sigma = mod2.ange$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_angelica.tif")
# Doryopteris decipiens
smap1.deci <- niche.G(Estck=clim.m1, mu=mod1.deci$wn.mu, Sigma = mod1.deci$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_decipiens.tif")
smap2.deci <- niche.G(Estck=clim.m2, mu=mod2.deci$wn.mu, Sigma = mod2.deci$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_decipiens.tif")
# Doryopteris decora
smap1.deco <- niche.G(Estck=clim.m1, mu=mod1.deco$wn.mu, Sigma = mod1.deco$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_decora.tif")
smap2.deco <- niche.G(Estck=clim.m2, mu=mod2.deco$wn.mu, Sigma = mod2.deco$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_decora.tif")
# Pellaea ternifolia
smap1.tern <- niche.G(Estck=clim.m1, mu=mod1.tern$wn.mu, Sigma = mod1.tern$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_ternifolia.tif")
# we'll use Mahalanobis model as Model 2
smap2.tern <- niche.G(Estck=clim.m2, mu=mod2.tern$maha.mu, Sigma = mod2.tern$maha.sigma,
                      save.ras="./Results/Model2/Suitability_m2_ternifolia.tif")
# Pityrogramma austroamericana
smap1.aust <- niche.G(Estck=clim.m1, mu=mod1.aust$wn.mu, Sigma = mod1.aust$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_austroamericana.tif")
smap2.aust <- niche.G(Estck=clim.m2, mu=mod2.aust$wn.mu, Sigma = mod2.aust$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_austroamericana.tif")
# Pityrogramma calomelanos
smap1.calo <- niche.G(Estck=clim.m1, mu=mod1.calo$wn.mu, Sigma = mod1.calo$wn.sigma,
                      save.ras="./Results/Model1/Suitability_m1_calomelanos.tif")
smap2.calo <- niche.G(Estck=clim.m2, mu=mod2.calo$wn.mu, Sigma = mod2.calo$wn.sigma,
                      save.ras="./Results/Model2/Suitability_m2_calomelanos.tif")


# END

# Resources
# http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
