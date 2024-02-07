# Plotting the suitability maps for the species of ferns in Hawaii
# First version: June, 2022

# Packages & Functions --------------------------------------------------
library(tidyverse)

# Load functions
source("00_Niche_functions.R")

# Read input files -------------------------------------------------
# Read PRESENCE data
occs <- read_csv("./Occurrences/occurrences_with_clim_values.csv",
                 col_names = T) %>% select(-year)
occs <- na.omit(occs)
# scale variable
occs$seasonality <- occs$seasonality/100
# species names
fsp <- factor(occs$species)
spname <- levels(fsp)
# split by species
angelica <- filter(occs, species == spname[2])
decipiens <- filter(occs, species == spname[3])
decora <- filter(occs, species == spname[4])
ternifolia <- filter(occs, species == spname[5])
viridis <- filter(occs, species == spname[1])
austroam <- filter(occs, species == spname[6])
calomelanos <- filter(occs, species == spname[7])

# Read raster files with model outputs, needed for PARTS 1 & 2
# (created with 03_Suitability_maps.R)

## NATIVE SPECIES
# Doryopteris angelica
smap1.ange <- rast("./Results/Model1/Suitability_m1_angelica.tif")
smap2.ange <- rast("./Results/Model2/Suitability_m2_angelica.tif")
# Doryopteris decipiens
smap1.deci <- rast("./Results/Model1/Suitability_m1_decipiens.tif")
smap2.deci <- rast("./Results/Model2/Suitability_m2_decipiens.tif")
# Doryopteris decora
smap1.deco <- rast("./Results/Model1/Suitability_m1_decora.tif")
smap2.deco <- rast("./Results/Model2/Suitability_m2_decora.tif")
# Pellaea ternifolia
smap1.tern <- rast("./Results/Model1/Suitability_m1_ternifolia.tif")
smap2.tern <- rast("./Results/Model2/Suitability_m2_ternifolia.tif")

# PART 1: Plot suitability maps -----------------------------------------

# Set colorpalette
col1 <- c("chartreuse4", "sienna3", "darkorchid4",
             "deepskyblue4", "gold4", "red4", "aquamarine4")
# species order: viridis, angelica, decipiens, decora, 
# ternifolia, austroamericana, calomelanos
col2 <- c("chartreuse1", "sienna1", "darkorchid1",
             "deepskyblue1", "gold1", "red1", "aquamarine1")
# 2
ange1 <- suit.map(spn="D. angelica, Model 1", sm.species = smap1.ange,
                  occ.data = angelica, c1=col1[2], c2=col2[2], spl=T)
ange2 <- suit.map(spn="D. angelica, Model 2", sm.species = smap2.ange,
                  occ.data = angelica, c1=col1[2], c2=col2[2], spl=T)
# 3
deci1 <- suit.map(spn="D. decipiens, Model 1", sm.species = smap1.deci,
                  occ.data = decipiens, c1=col1[3], c2=col2[3], spl=T)
deci2 <- suit.map(spn="D. decipiens, Model 2", sm.species = smap2.deci,
                  occ.data = decipiens, c1=col1[3], c2=col2[3], spl=T)
# 4
deco1 <- suit.map(spn="D. decora, Model 1", sm.species = smap1.deco,
                  occ.data = decora, c1=col1[4], c2=col2[4], spl=T)
deco2 <- suit.map(spn="D. decora, Model 2", sm.species = smap2.deco,
                  occ.data = decora, c1=col1[4], c2=col2[4], spl=T)
# 5
tern1 <- suit.map(spn="P. ternifolia, Model 1", sm.species = smap1.tern,
                  occ.data = ternifolia, c1=col1[5], c2=col2[5], spl=T)
tern2 <- suit.map(spn="P. ternifolia, Model 2", sm.species = smap2.tern,
                  occ.data = ternifolia, c1=col1[5], c2=col2[5], spl=T)

x11()
tern1

# Save map as image

# # If split=F' in the last block:
# ggsave(plot=ange1 + ange2, filename="./Results/SuitabilityMap_m1m2_angelica_zoom.png",
#        width = 24, height = 12, units = "cm", dpi = 300, pointsize = 6)
# ggsave(plot=deci1 + deci2, filename="./Results/SuitabilityMap_m1m2_decipiens.png",
#        width = 24, height = 12, units = "cm", dpi = 300, pointsize = 6)
# ggsave(plot=deco1 + deco2, filename="./Results/SuitabilityMap_m1m2_decora.png",
#        width = 24, height = 12, units = "cm", dpi = 300, pointsize = 6)
# ggsave(plot=tern1 + tern2, filename="./Results/SuitabilityMap_m1m2_ternifolia.png",
#        width = 24, height = 12, units = "cm", dpi = 300, pointsize = 6)

# If split=T' in the last block:
ggsave(plot=ange1, filename="./Results/Model1/SuitabilityMap_m1_Dangelica.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=ange2, filename="./Results/Model2/SuitabilityMap_m2_Dangelica.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=deci1, filename="./Results/Model1/SuitabilityMap_m1_Ddecipiens.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=deci2, filename="./Results/Model2/SuitabilityMap_m2_Ddecipiens.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=deco1, filename="./Results/Model1/SuitabilityMap_m1_Ddecora.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=deco2, filename="./Results/Model2/SuitabilityMap_m2_Ddecora.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=tern1, filename="./Results/Model1/SuitabilityMap_m1_Pternifolia.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)
ggsave(plot=tern2, filename="./Results/Model2/SuitabilityMap_m2_Pternifolia.png",
       width = 40, height = 8, units = "cm", dpi = 300, pointsize = 12)

# PART 2: Binarization of suitability maps ---------------------------------------------

# Read estimated parameters of niche models
mles <- read_csv("./Results/Estimated_parameters_allSpecies_M1M2_5feb2024_10K.csv")

# For Model 1: dif_rad, rel_humidity, seasonality
occ.var1 <- c(6,8,9)
# # For Model 2: leaf_ai, hot_mean, dry_avg
occ.var2 <- c(7,10,11)

# Doryopteris angelica
# Model 1 with Mahalanobis estimates
# sm.df <- as.data.frame(smap1.ange, xy=T)
# colnames(sm.df) <- c("Longitude","Latitude","Suitability")
# quantile(x=sm.df$Suitability, probs=0.95, na.rm=T)
ange1 <- bin3.ras(sm.species=smap1.ange, occ.data=angelica[,occ.var1],
                  mu.hat=c(mles$maha.mu1[2],mles$maha.mu2[2],mles$maha.mu3[2]),
                  s.hat=matrix(as.numeric(mles[2,18:26]), byrow=T, ncol=3),
                  method="alpha", bval=100,
                  save.ras="./Results/Model1/D_angelica_Model1_binary_95.tif")
# Model 2 with Weighted-normal estimates
ange2 <- bin3.ras(sm.species=smap2.ange, occ.data=angelica[,occ.var2],
                  mu.hat=c(mles$wn.mu1[9],mles$wn.mu2[9],mles$wn.mu3[9]),
                  s.hat=matrix(as.numeric(mles[9,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=200,
                  save.ras="./Results/Model2/D_angelica_Model2_binary_95.tif")
# Doryopteris decipiens
deci1 <- bin3.ras(sm.species=smap1.deci, occ.data=decipiens[,occ.var1],
                  mu.hat=c(mles$wn.mu1[3],mles$wn.mu2[3],mles$wn.mu3[3]),
                  s.hat=matrix(as.numeric(mles[3,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=100,
                  save.ras="./Results/Model1/D_decipiens_Model1_binary_95.tif")
deci2 <- bin3.ras(sm.species=smap2.deci, occ.data=decipiens[,occ.var2],
                  mu.hat=c(mles$wn.mu1[10],mles$wn.mu2[10],mles$wn.mu3[10]),
                  s.hat=matrix(as.numeric(mles[10,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=200,
                  save.ras="./Results/Model2/D_decipiens_Model2_binary_95.tif")
# Doryopteris decora
deco1 <- bin3.ras(sm.species=smap1.deco, occ.data=decora[,occ.var1],
                  mu.hat=c(mles$wn.mu1[4],mles$wn.mu2[4],mles$wn.mu3[4]),
                  s.hat=matrix(as.numeric(mles[4,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=100,
                  save.ras="./Results/Model1/D_decora_Model1_binary_95.tif")
deco2 <- bin3.ras(sm.species=smap2.deco, occ.data=decora[,occ.var2],
                  mu.hat=c(mles$wn.mu1[11],mles$wn.mu2[11],mles$wn.mu3[11]),
                  s.hat=matrix(as.numeric(mles[11,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=200,
                  save.ras="./Results/Model2/D_decora_Model2_binary_95.tif")
# Pellaea ternifolia
# Model 1 with Weighted-normal estimates
tern1 <- bin3.ras(sm.species=smap1.tern, occ.data=ternifolia[,occ.var1],
                  mu.hat=c(mles$wn.mu1[5],mles$wn.mu2[5],mles$wn.mu3[5]),
                  s.hat=matrix(as.numeric(mles[5,6:14]), byrow=T, ncol=3),
                  method="alpha", bval=100,
                  save.ras="./Results/Model1/P_ternifolia_Model1_binary_95.tif")
# Model 2 with Mahalanobis estimates
tern2 <- bin3.ras(sm.species=smap2.tern, occ.data=ternifolia[,occ.var2],
                  mu.hat=c(mles$maha.mu1[12],mles$maha.mu2[12],mles$maha.mu3[12]),
                  s.hat=matrix(as.numeric(mles[12,18:26]), byrow=T, ncol=3),
                  method="alpha", bval=200,
                  save.ras="./Results/Model2/P_ternifolia_Model2_binary_95.tif")

x11()
par(mfrow=c(1,2))
plot(tern1)
plot(tern2)

# Combine binary maps of models 1 and 2 --------------------------------------
ange12 <- ange1 + ange2
deci12 <- deci1 + deci2
deco12 <- deco1 + deco2
tern12 <- tern1 + tern2
# Create rasters with combined categories
writeRaster(ange12, "./Results/Models1-2/Dangelica-Models12-categorical_95.tif",
            overwrite = T)
writeRaster(deci12, "./Results/Models1-2/Ddecipiens-Models12-categorical_95.tif",
            overwrite = T)
writeRaster(deco12, "./Results/Models1-2/Ddecora-Models12-categorical_95.tif",
            overwrite = T)
writeRaster(tern12, "./Results/Models1-2/Pternifolia-Models12-categorical_95.tif",
            overwrite = T)

# Draw maps of model categories (optional: adding presence points of natives)
ange12.m <- bin.map(spn="Doryopteris angelica", bmodel=ange12) #, pnts=angelica[,4:5]
deci12.m <- bin.map(spn="Doryopteris decipiens", bmodel=deci12) #, pnts=decipiens[,4:5]
deco12.m <- bin.map(spn="Doryopteris decora", bmodel=deco12) #, pnts=decora[,4:5]
tern12.m <- bin.map(spn="Pellaea ternifolia", bmodel=tern12) #, pnts=ternifolia[,4:5]

x11()
tern12.m

ggsave(plot=ange12.m, width = 40, height = 8, units = "cm", dpi = 600, pointsize = 12,
       filename="./Results/Models1-2/Dangelica-Models12-categorical_95pnts.png")
ggsave(plot=deci12.m, width = 40, height = 8, units = "cm", dpi = 600, pointsize = 12,
       filename="./Results/Models1-2/Ddecipiens-Models12-categorical_95pnts.png")
ggsave(plot=deco12.m, width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Models1-2//Ddecora-Models12-categorical_95pnts.png")
ggsave(plot=tern12.m, width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Models1-2/Pternifolia-Models12-categorical_95pnts.png")

# PART 3: Adding presence data to categorical maps -----------------------------------

# Non-natives: Cheilanthes viridis, Pityrogramma austroamericana, P. calomelanos
non.nat <- rbind(viridis, austroam, calomelanos)

# Only consider areas where both models predict high suitability

# D. angelica
ange12 <- rast("./Results/Models1-2/Dangelica-Models12-categorical_95.tif")
# change raster values and create single table
ange12 <- subst(ange12, c(100,200), c(0,0))
ange.non <- cbind(rbind(non.nat[,4:5], angelica[,4:5]),
                 c(rep(2,nrow(non.nat)),rep(1,nrow(angelica))))
colnames(ange.non) <- c('lon','lat','cats')
# make map
ange12pp <- bin.map12(spn="D. angelica", bmodel=ange12, pnts=ange.non,
                      col.pnts = c('green','yellow'))
x11()
ange12pp

# D. decipiens
deci12 <- rast("./Results/Models1-2/Ddecipiens-Models12-categorical_95.tif")
deci12 <- subst(deci12, c(100,200), c(0,0))
deci.non <- cbind(rbind(non.nat[,4:5], decipiens[,4:5]),
                 c(rep(2,nrow(non.nat)),rep(1,nrow(decipiens))))
colnames(deci.non) <- c('lon','lat','cats')
deci12pp <- bin.map12(spn="D. decipiens", bmodel=deci12, pnts=deci.non,
                      col.pnts = c('green','yellow'))
deci12pp

# D. decora
deco12 <- rast("./Results/Models1-2/Ddecora-Models12-categorical_95.tif")
deco12 <- subst(deco12, c(100,200), c(0,0))
deco.non <- cbind(rbind(non.nat[,4:5], decora[,4:5]),
                  c(rep(2,nrow(non.nat)),rep(1,nrow(decora))))
colnames(deco.non) <- c('lon','lat','cats')
deco12pp <- bin.map12(spn="D. decora", bmodel=deco12, pnts=deco.non,
                      col.pnts = c('green','yellow'))
deco12pp

# P. ternifolia
tern12 <- rast("./Results/Models1-2/Pternifolia-Models12-categorical_95.tif")
tern12 <- subst(tern12, c(100,200), c(0,0))
tern.non <- cbind(rbind(non.nat[,4:5], ternifolia[,4:5]),
                  c(rep(2,nrow(non.nat)),rep(1,nrow(ternifolia))))
colnames(tern.non) <- c('lon','lat','cats')
tern12pp <- bin.map12(spn="D. ternifolia", bmodel=tern12, pnts=tern.non,
                      col.pnts = c('green','yellow'))
tern12pp

# Save maps: suitable cell under both models, with presence points
#           used to fit the models, plus presences of non-natives
ggsave(plot=ange12pp, width = 40, height = 8, units = "cm", dpi = 600, pointsize = 12,
       filename="./Results/Models1-2/Dangelica-Models12only-95nonat.png")
ggsave(plot=deci12pp, width = 40, height = 8, units = "cm", dpi = 600, pointsize = 12,
       filename="./Results/Models1-2/Ddecipiens-Models12only-95nonat.png")
ggsave(plot=deco12pp, width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Models1-2//Ddecora-Models12only-95nonat.png")
ggsave(plot=tern12pp, width = 40, height = 8, units = "cm", dpi = 400, pointsize = 12,
       filename="./Results/Models1-2/Pternifolia-Models12only-95nonat.png")

# PART 4: identify how many occurrences are inside native niches -----------
# non-natives
non.ange <- extract(ange12, non.nat[,4:5], ID=F)
length(which(non.ange>0)) *100 / nrow(non.nat) # 2.645503

non.deci <- extract(deci12, non.nat[,4:5], ID=F)
length(which(non.deci>0)) *100 / nrow(non.nat) # 42.32804

non.deco <- extract(deco12, non.nat[,4:5], ID=F) # 65.60847
length(which(non.deco>0)) *100 / nrow(non.nat)

non.tern <- extract(tern12, non.nat[,4:5], ID=F) # 34.39153
length(which(non.tern>0)) *100 / nrow(non.nat)
# natives
n.ange <- extract(ange12, angelica[,4:5], ID=F)
length(which(n.ange>0)) *100 / nrow(angelica) # 82.75862

n.deci <- extract(deci12, decipiens[,4:5], ID=F)
length(which(n.deci>0)) *100 / nrow(decipiens) # 91.11111

n.deco <- extract(deco12, decora[,4:5], ID=F) # 100
length(which(n.deco>0)) *100 / nrow(decora)

n.tern <- extract(tern12, ternifolia[,4:5], ID=F) # 96.20253
length(which(n.tern>0)) *100 / nrow(ternifolia)

### END