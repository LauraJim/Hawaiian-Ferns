setwd("~/Desktop/dry_ferns_subsample/")

##### set up global parameters #####

# N is the number of samples to draw (total number of naturalized)
N <- sum(c(55, 82, 53)) # 55 P. calomelanos 82, 82 P. austroamericana, 53 C. viridis

# M is the number of reps per native species 
M <- 1000

# Create data frame to store data in 
dat <- data.frame(p_tern = numeric(length = M), 
                  d_deco = numeric(length = M),
                  d_deci = numeric(length = M),
                  d_ang  = numeric(length = M))

# Raster paths 
paths <- c("rasters/Pternifolia-Models12-categorical_95.tif",
           "rasters/Ddecora-Models12-categorical_95.tif",
           "rasters/Ddecipiens-Models12-categorical_95.tif",
           "rasters/Dangelica-Models12-categorical_95.tif")

##### Randomly sample #####

for (i in 1:length(paths)){
  for (j in 1:M) {
    # Generate N random points in the Hawaiian islands
    raster <- terra::rast(paths[i])
    random_points <- terra::spatSample(raster, N, na.rm = T)
    dat[j,i] <- 100 * (length(which(random_points>200)) / nrow(random_points))
  }
}

##### Make plots #####

a <- ggplot(dat) +
       geom_density(aes(p_tern), fill = "grey") +
       xlim(0,100) +
       geom_vline(xintercept = 34.39, 
                  color = "red", 
                  linetype="dashed") +
       annotate("text", 
                x = 17, 
                y = .12, 
                label = "34.39%", 
                color = "red",
                size = 5) +
       xlab("percent overlap") +
       ylab("") +
       ggtitle("Pellaea ternifolia") +
       theme_bw() +
       theme(plot.title=element_text(face="italic"))
  
b <- ggplot(dat) +
       geom_density(aes(d_deco), fill = "grey") +
       xlim(0,100) +
       geom_vline(xintercept = 65.60, 
                  color = "red",
                  linetype="dashed") +
      annotate("text", 
               x = 82, 
               y = .11, 
               label = "65.60%", 
               color = "red",
               size = 5) +
       xlab("percent overlap") +
       ylab("density") +
       ggtitle("Doryopteris decora") +
       theme_bw() +
       theme(plot.title=element_text(face="italic"))

c <- ggplot(dat) +
       geom_density(aes(d_deci), fill = "grey") +
       xlim(0,100) +
       geom_vline(xintercept = 42.32, 
                  color = "red", 
                  linetype="dashed") +
       annotate("text", 
                x = 59, 
                y = .13, 
                label = "42.32%", 
                color = "red",
                size = 5) +
       xlab("") +
       ylab("") +
       ggtitle("Doryopteris decipiens") +
       theme_bw() +
       theme(plot.title=element_text(face="italic"))

d <- ggplot(dat) +
       geom_density(aes(d_ang), fill = "grey") +
       xlim(0,10) +
       geom_vline(xintercept = 2.64, 
                  color = "red", 
                  linetype="dashed") +
       annotate("text", 
                x = 4, 
                y = 5.7, 
                label = "2.64%", 
                color = "red",
                size = 5) +
       xlab("")  +
       ylab("density") +
       ggtitle("Doryopteris angelica") +
       theme_bw() +
       theme(plot.title=element_text(face="italic"))

pdf("sampling_naturalized.pdf", height = 7, width = 7)
cowplot::plot_grid(d,c,b,a)
dev.off()
