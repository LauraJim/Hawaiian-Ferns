# Adding information on land cover categories to the niche model output
# Author: Laura Jimenez
# First version: July, 2022

# Packages & Functions --------------------------------------------------
library(sf)
library(dplyr)
library(raster)
library(dplyr)
library(ggpubr)
library(stringr)
library(tidyverse)

# Load functions
source("00_Niche_functions.R")

# Read rasters with niche model outputs ----------------------------------
m.ange <- rast("./Results/Models1-2/Dangelica-Models12-categorical_95.tif")
m.deci <- rast("./Results/Models1-2/Ddecipiens-Models12-categorical_95.tif")
m.deco <- rast("./Results/Models1-2/Ddecora-Models12-categorical_95.tif")
m.tern <- rast("./Results/Models1-2/Pternifolia-Models12-categorical_95.tif")

# PART 1: Prepare land cover layer ---------------------------------------------
# OPTION 1: with functions from the 'raster' package
land.cover <- raster("./hawaii-islands/CAH_LandCover/CAH_LandCover.tif", RAT = TRUE)
lc.rat <- levels(land.cover)[[1]]
#cat(land.cover)
x11()
plot(land.cover)

# landc.h <- hist(land.cover, breaks=c(0:50,100,70000))
# landc.h$counts
# landc.h$breaks

# OPTION 2: with functions from 'terra', we'll keep working with this raster
(landcover <- rast("./hawaii-islands/CAH_LandCover/CAH_LandCover.tif"))
# we have a categorical raster
levels(landcover)
# plot original land cover layer
x11()
plot(landcover)

# PART 2: Prepare land cover layer ---------------------------------------------
# FIRST CLASSIFICATION
# to work with the different categories in the raster change the active category
activeCat(landcover) <- 6
plot(landcover)
# we use a combination of 'Biome_Units' and 'Detailed_l' categories
convert <- levels(landcover)[[1]]
convert$Biome_Unit[23] <- "Alien Tree Plantation"
convert$Biome_Unit[43] <- "Developed open space"
convert$Biome_Unit[44] <- "High intensity developed"
convert$Biome_Unit[45] <- "Low intensity developed"
convert$Biome_Unit[46] <- "Medium intensity developed"
# keep track of category transformations
colnames(convert) <- c("ID","LC.type")
lc.rat1 <- inner_join(lc.rat[,c(1,4)], convert, by='ID')
# save new categories
convert <- unique(convert$LC.type)
new.lc <- data.frame(LC.type=convert, lc.lab=1:length(convert))
lc.rat1 <- inner_join(lc.rat1, new.lc, by='LC.type')

# SECOND CLASSIFICATION
# we will use the 'General_LC' classes to create a new classification
activeCat(landcover) <- 4
plot(landcover)
# we use a simple four-categories classification
convert <- levels(landcover)[[1]]
convert$General_LC <- str_split_fixed(convert$General_LC, " ", n=2)[,1]
convert$General_LC[40] <- "Mixed"
convert$General_LC[41] <- "Mixed"
convert$General_LC[42] <- "Alien"
convert$General_LC[47] <- "Mixed"
convert$General_LC[48] <- "Mixed"
# keep track of category transformations
colnames(convert) <- c("ID","LC.type2")
lc.rat2 <- inner_join(lc.rat1, convert, by='ID')
# save new categories
convert <- unique(convert$LC.type2)
new.lc2 <- data.frame(LC.type2=convert, lc.lab2=1:length(convert))
lc.rat2 <- inner_join(lc.rat2, new.lc2, by='LC.type2')

# Save key table of LC category transformations
write.csv(lc.rat2, "./Results/LandCover/NewLandCover-categories.csv", row.names = F)

# PART 3: Prepare land cover layer ---------------------------------------------
# Changing the values in the land cover layer (from area values to category labels)
activeCat(landcover) <- 1
# from 'Area_Ha' to 'Detailed_L'
lookup <- data.frame(levels(landcover)[[1]][,2], 1:48)
landcover_rc <- classify(landcover, lookup)

# FIRST CLASSIFICATION
# from 'Detailed_L' to new categories stored in 'new.lc'
lookup1 <- data.frame(1:48, lc.rat2$lc.lab)
landcover_rc1 <- classify(landcover, lookup1)
landcover_rc1 <- subst(landcover_rc1, from=65535, to=NA)
(landcover_rc1 <- subst(landcover_rc1, from=0, to=NA))
plot(landcover_rc1)

# SECOND CLASSIFICATION
# from 'Detailed_L' to new categories stored in 'new.lc'
lookup2 <- data.frame(1:48, lc.rat2$lc.lab2)
landcover_rc2 <- classify(landcover, lookup2)
landcover_rc2 <- subst(landcover_rc2, from=65535, to=NA)
(landcover_rc2 <- subst(landcover_rc2, from=0, to=NA))
plot(landcover_rc2)

# COMBINE LAND COVER WITH MODEL OUTPUTS ----------------------------------------
# Choose only one of the LC classifications created above
landcover_rc <- landcover_rc1
lc.tab <- distinct(lc.rat2[,3:4]) # change to 5:6 for second classification
colnames(lc.tab) <- c("LC.type", "lc.lab")
nm <- "LC_1stclassification-m12" # change to "LC_2ndclassification-m12" for second classification

# Doryopteris angelica ----------------------------------------------------
# match model and land-cover rasters
(lc.ange <- add.rmatch(landcover_rc, m.ange))
  # checkpoint: values of the new layer
  # lc.stats <- hist(lc.ange, breaks=0:350)
  #lc.stats$counts
# filter out irrelevant categories
(lc.ange1 <- subst(lc.ange, from=1:299, to=0))
  # plot(lc.ange1)
# create table / needed for barplots
bm.ange <- as.data.frame(lc.ange1, xy=T)
colnames(bm.ange) <- c("Lon","Lat","Value")
# plot the result - OPTIONAL
b.ange <- ggplot(data = bm.ange, aes(x=Lon, y=Lat)) +
  geom_tile(aes(fill=as.factor(Value))) +
  scale_fill_viridis(discrete=T, option="H") +
  theme_void()
aloha(spn="Doryopteris angelica", basemap=b.ange, dleg=T, axes.leg=F)
# Calculate the area covered by each class
area.ange <- bm.ange %>%
  group_by(Value) %>%
  tally() %>%
  mutate(area = n * res(lc.ange1)[1] * res(lc.ange1)[2])
# add a column with land cover types using categories in the first column
# and another one indicating which model predicted the cell as suitable
lc.lab <- area.ange$Value[-1] - 300
lc.m12 <- rbind(c(0, "Unsuitable"),
                inner_join(as.data.frame(lc.lab), lc.tab, by='lc.lab'))
# ensemble categories
table1 <- cbind(LC.type=lc.m12$LC.type, area.ange)
# add column of relative counts
table1 <- table1 %>% mutate(rel_count = n/sum(table1$n))
# add column with species name
(table1 <- cbind(species=rep("D. angelica", nrow(table1)), table1))

# Doryopteris decipiens ------------------------------------------------
lc.deci <- add.rmatch(landcover_rc, m.deci)
lc.deci1 <- subst(lc.deci, from=1:299, to=0)
bm.deci <- as.data.frame(lc.deci1, xy=T)
colnames(bm.deci) <- c("Lon","Lat","Value")
# plot - optional
b.deci <- ggplot(data = bm.deci, aes(x=Lon, y=Lat)) +
  geom_tile(aes(fill=as.factor(Value))) +
  scale_fill_viridis(discrete=T, option="H") +
  theme_void()
aloha(spn="Doryopteris decipiens", basemap=b.deci, dleg=T, axes.leg=F)
#
area.deci <- bm.deci %>%
  group_by(Value) %>%
  tally() %>%
  mutate(area = n * res(lc.deci1)[1] * res(lc.deci1)[2])
lc.lab <- area.deci$Value[-1] - 300
lc.m12 <- rbind(c(0, "Unsuitable"),
                inner_join(as.data.frame(lc.lab), lc.tab, by='lc.lab'))
table2 <- cbind(LC.type=lc.m12$LC.type, area.deci)
table2 <- table2 %>% mutate(rel_count = n/sum(table2$n))
(table2 <- cbind(species=rep("D. decipiens", nrow(table2)), table2))

# Doryopteris decora -----------------------------------------------------
lc.deco <- add.rmatch(landcover_rc, m.deco)
lc.deco1 <- subst(lc.deco, from=1:299, to=0)
bm.deco <- as.data.frame(lc.deco1, xy=T)
colnames(bm.deco) <- c("Lon","Lat","Value")
# plot - optional
b.deco <- ggplot(data = bm.deco, aes(x=Lon, y=Lat)) +
  geom_tile(aes(fill=as.factor(Value))) +
  scale_fill_viridis(discrete=T, option="H") +
  theme_void()
aloha(spn="Doryopteris decora", basemap=b.deco, dleg=T, axes.leg=F)
#
area.deco <- bm.deco %>%
  group_by(Value) %>%
  tally() %>%
  mutate(area = n * res(lc.deco1)[1] * res(lc.deco1)[2])
lc.lab <- area.deco$Value[-1] - 300
lc.m12 <- rbind(c(0, "Unsuitable"), inner_join(as.data.frame(lc.lab), lc.tab, by='lc.lab'))
table3 <- cbind(LC.type=lc.m12$LC.type, area.deco)
table3 <- table3 %>% mutate(rel_count = n/sum(table3$n))
(table3 <- cbind(species=rep("D. decora", nrow(table3)), table3))

# Pellaea ternifolia -------------------------------------------------------
lc.tern <- add.rmatch(landcover_rc, m.tern)
lc.tern1 <- subst(lc.tern, from=1:299, to=0)
bm.tern <- as.data.frame(lc.tern1, xy=T)
colnames(bm.tern) <- c("Lon","Lat","Value")
# plot - optional
b.tern <- ggplot(data = bm.tern, aes(x=Lon, y=Lat)) +
  geom_tile(aes(fill=as.factor(Value))) +
  scale_fill_viridis(discrete=T, option="H") +
  theme_void()
aloha(spn="Pellaea ternifolia", basemap=b.tern, dleg=T, axes.leg=F)
#
area.tern <- bm.tern %>%
  group_by(Value) %>%
  tally() %>%
  mutate(area = n * res(lc.tern1)[1] * res(lc.tern1)[2])
lc.lab <- area.tern$Value[-1] - 300
lc.m12 <- rbind(c(0, "Unsuitable"),
                inner_join(as.data.frame(lc.lab), lc.tab, by='lc.lab'))
table4 <- cbind(LC.type=lc.m12$LC.type, area.tern)
table4 <- table4 %>% mutate(rel_count = n/sum(table4$n))
(table4 <- cbind(species=rep("P. ternifolia", nrow(table4)), table4))

# Combine the four tables and save
master <- rbind(table1, table2, table3, table4)
write.csv(master, paste0("./Results/LandCover/", nm, ".csv"), row.names = F)
# repeat twice, changing lines 109-114


# CREATE BAR PLOTS -----------------------------------------------------------

master1 <- read_csv("./Results/LandCover/LC_1stclassification-m12.csv") %>%
  filter(LC.type != "Unsuitable")
master2 <- read.csv("./Results/LandCover/LC_2ndclassification-m12.csv") %>%
  filter(LC.type != "Unsuitable")

# calculate percentage of suitable area covered by lc-type
t1 <- master1 %>% dplyr::select(species, LC.type, n) %>%
  group_by(species) %>% mutate(prop=n*100/sum(n))
t2 <- master2 %>% dplyr::select(species, LC.type, n) %>%
  group_by(species) %>% mutate(prop=n*100/sum(n))
write.csv(rbind(t1,t2), "./Results/LandCover/LC_classification12-m12_percentages.csv",
          row.names = F)

# re-arrange the categories, if you want them to be plotted in a particular order
master1$LC.type <- factor(master1$LC.type, levels = levels(master1$LC.type)[c(1:3,8,9,7,13,4,6,5,10,12,11,15,17,16,18,14)])
levels(master1$LC.type) <- paste0(1:length(levels(master1$LC.type)),": ", levels(master1$LC.type))

master2$LC.type <- factor(master2$LC.type, levels = levels(master2$LC.type)[c(2,1,3:5)])
levels(master2$LC.type)[1] <- "Developed                               "

# FIRST PLOT

# custom colors:
colors1 <- 
  c( "1: Agriculture" =  "#E8A3E0",     
     "2: Alien Tree Plantation" = "#AA4499",
     "7: Not Vegetated" = "#C19A61",
     
     "3: Developed open space" = "#E0E0E0",   
     "6: High intensity developed" = "#232323",
     "5: Medium intensity developed" = "#686868",
     "4: Low intensity developed" = "#BBBBBB",
     
     "8: Dry Forest" = "#EA806E",
     "9: Dry Shrubland" = "#CC3311",           
     "10: Dry Grassland" = "#7F1301",
     
     "11: Mesic Forest"= "#6AA37B",
     "12: Mesic Shrubland" = '#117733',         
     "13: Mesic Grassland" = "#012B0E",
     
     "14: Wet Forest" = "#33BBEE",
     "15: Wet Shrubland" = "#2992B2",        
     "16: Wet Grassland" = "#0F5970",
     "17: Wetland" = "#012D3A")

master1 %>%
  group_by(species) %>%
  mutate(rel_area = area/(sum(area))) %>%
  ungroup() -> dat

# add missing categories to angelica
categories <- dat[grep("angelica", dat$species),"LC.type"]
categories_to_add <- levels(dat$LC.type)[!(levels(dat$LC.type) %in% unlist(categories))]
categories_to_add <- categories_to_add[-which(categories_to_add == "18: Unsuitable")]
rows_to_add <- as.data.frame(matrix(0,
                                    nrow = length(categories_to_add),
                                    ncol = ncol(dat)))
colnames(rows_to_add) <- colnames(dat)
rows_to_add$LC.type <- categories_to_add
rows_to_add$species <- "D. angelica"

get_num <- function(x) {
  t <- strsplit(as.character(x), ": ")
  num <- matrix(unlist(t), ncol = 2, byrow = T)[ ,1]
}

rbind(dat, rows_to_add) %>%
  mutate(label = get_num(LC.type)) %>%
  mutate(label = ifelse(rel_area < 0.01, NA, label)) %>%
  ggplot(aes(x=factor(species), 
             y=rel_area, 
             fill=LC.type)) +

    geom_bar(stat = "identity") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "white") +
  ylab("Proportion of suitable area") +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(position = "top") +
  xlab("") +
  scale_fill_manual(values = colors1,
                    name="Land cover") +

    theme_pubclean() +
  theme(#legend.position = c(.59, .7),
    legend.direction = "vertical",
    legend.position = "right",
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 13),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 20, face = "italic"),
    axis.title.y = element_text(size = 20)) -> stbar1 

# SECOND PLOT 

# custom colors
colors2 <- c(
  "Developed                               "= "#686868",
  "Alien" = "#EEDD88",
  "Mixed" = "#44BB99",
  "Native" = "#222255"
)

master2 %>%
  group_by(species) %>%
  mutate(rel_area = area/(sum(area))) %>%
  ungroup() %>%
  ggplot(aes(x=factor(species), 
             y=rel_area, 
             fill=LC.type)) +
  geom_bar(stat = "identity") +
  ylab("Proportion of suitable area") +
  scale_y_continuous(n.breaks = 10) +
  xlab("") + 
  scale_fill_manual(values = colors2,
                    name="Land cover") +
  theme_pubclean() +
  theme(legend.position = "right",
        legend.text = element_text(size = 13),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 20, face = "italic"),
        axis.title.y = element_text(size = 20)) -> stbar2 

# arrange both bar plots into a single plot
pdf("barplots.pdf", height = 18, width = 14)
lc.barp <- ggpubr::ggarrange(stbar1, stbar2, ncol = 1, nrow = 2, heights = c(3,1));lc.barp
dev.off()