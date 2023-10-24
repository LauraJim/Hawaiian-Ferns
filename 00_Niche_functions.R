# Auxiliary functions to reproduce all the analysis in Edwards-Calma et al.

# Required packages
library(optimr)
library(terra)
library(patchwork)
library(ggplot2)
library(viridis)
library(ggnewscale)
library(ggtext)
#if(!require(mvtnorm)){ install.packages("mvtnorm") }

# FUNCTIONS for 2D and 3D niches

# FUNCTION 1 --------------------------------------
# Negative log-likelihood function for theta=(mu,A)
## guess -- is a vector of length 5 when d=2, it contains the mu and A values as elements
## sam1 -- matrix containing the sample of environmental combinations that correspond to presences
## sam2 -- matrix containing a random sample of environmental combinations which come from the area of study (M)
negloglike <- function(guess,sam1,sam2){
  if(ncol(sam1) != ncol(sam2)){
    print("The matrices should have the same number of columns")
  } else{
    # number of dimensions in E-space
    d <- ncol(sam1)
    # original sample size: number of presence points in the sample
    n <- nrow(sam1)
  }
  # define the parameters of interest using the parameter 'guess'
  if(d==2){
    # function that calculates quadratic terms, use inverse of matrix
    quad <- function(xi) {
      ax <- as.matrix(xi - guess[1:d])
      mA <- rbind(c(guess[d+1], guess[d+2]),
                  c(guess[d+2], guess[d+3]))
      return( t(ax) %*% mA %*% ax )
    }
  }
  if(d==3){
    quad <- function(xi) {
      ax <- as.matrix(xi - guess[1:d])
      mA <- rbind(c(guess[d+1], guess[d+2], guess[d+3]),
                  c(guess[d+2], guess[d+4], guess[d+5]),
                  c(guess[d+3], guess[d+5], guess[d+6]))
      return( t(ax) %*% mA %*% ax )
    }
  }
  q1 <- apply(sam1, 1, quad) # quadratic terms of presence points
  q2 <- apply(sam2, 1, quad) # quadratic terms of M points
  # negative log-likelihood value
  S <- 0.5*sum(q1) + n*log(sum(exp(-0.5*q2)))
  return(S)
}

# FUNCTION 2 -------------------------------
## E.occ = sam1 -- matrix containing the sample of environmental combinations that correspond to presences
## E.samM = sam2 -- matrix containing a random sample of environmental combinations which come from the area of study (M)

# Maximum Likelihood Maximization
fitNiche <- function(E.occ, E.samM) {
  d <- ncol(E.occ)
  # calculate variance-covariance matrix
  Sig.ini <- cov(E.occ)
  # initial values
  mu.ini <- colMeans(E.occ)
  A.ini <- chol2inv(chol(Sig.ini))
  # initial parameters as a vector
  vals.ini <- c(mu.ini, unique(as.vector(A.ini)))
  # fix the values of the samples used to evaluate the neg-log-likelihood
  like.fn <- function(theta){
    negloglike(theta, sam1= E.occ, sam2= E.samM)
  }
  # maximize -log(Likelihood)
  #lower.val <- c(apply(E.samM,2,min),0,-Inf,-Inf,0,-Inf,0)
  find.mle <- optimr(par=vals.ini, fn=like.fn, method="Nelder-Mead")
  # mles
  mle.mu <- find.mle$par[1:d]
  if(d==2){
    mle.A <- rbind(c(find.mle$par[d+1], find.mle$par[d+2]),
                   c(find.mle$par[d+2], find.mle$par[d+3]))
  }
  if(d==3){
    mle.A <- rbind(c(find.mle$par[d+1], find.mle$par[d+2], find.mle$par[d+3]),
                   c(find.mle$par[d+2], find.mle$par[d+4], find.mle$par[d+5]),
                   c(find.mle$par[d+3], find.mle$par[d+5], find.mle$par[d+6]))
  }
  mle.sigma <- tryCatch(expr={chol2inv(chol(mle.A))},
                        error= function(e){NULL})
  # If the estimated matrix is not positive definite, try a second algorithm
  if(is.null(mle.sigma)){
    find.mle <- optimr(par=vals.ini, fn=like.fn, method="Rvmmin")
    # mles
    mle.mu <- find.mle$par[1:d]
    if(d==2){
      mle.A <- rbind(c(find.mle$par[d+1], find.mle$par[d+2]),
                     c(find.mle$par[d+2], find.mle$par[d+3]))
    }
    if(d==3){
      mle.A <- rbind(c(find.mle$par[d+1], find.mle$par[d+2], find.mle$par[d+3]),
                     c(find.mle$par[d+2], find.mle$par[d+4], find.mle$par[d+5]),
                     c(find.mle$par[d+3], find.mle$par[d+5], find.mle$par[d+6]))
    }
    mle.sigma <- tryCatch(expr={chol2inv(chol(mle.A))},
                          error= function(e){NULL})
  }
  # Save both the initial and the estimated values of the parameters
  # wn = weighted normal distribution
  return(list(wn.mu = mle.mu, wn.sigma = mle.sigma,
              maha.mu = mu.ini, maha.sigma = Sig.ini))
}

# FUNCTION 3.0 -------------------------------------
# Auxiliary function that calculates a suitability value for a raster cell

sui.fun <- function(cell, mu, Sigma){
  X <- as.vector(cell, mode = "numeric")
  sui.ind <- exp(-mahalanobis(x= X, center= mu, cov= Sigma)/2)
  return(sui.ind)
}
# equivalent and faster than:
# calculate optimum (suitability value at niche center)
# (max.val <- exp(-mahalanobis(x= mu, center= mu, cov= Sigma)/2))
#   sui.fun <- function(cell, mu, Sigma, maxval){
#   log(mvtnorm::dmvnorm(x=c(cell[1],cell[2],cell[3]),
#                        mean = mu, sigma = Sigma))-log(maxval)
# }

# FUNCTION 3.1 -------------------------------------
# The function niche.G projects ellipses that define suitable environments for a 
# species on a map as potential niches. The regions in the geographical space
# are colored by different degrees of suitability.

## Parameters:
# Estck = a raster stack with more than two climatic layers
# mu = the mean of the columns that contain environmental data, such as 
#       temperature and precipitation 
# Sigma = the covariance of the environmental data linked with a species' 
#         occurrence
# save.ras = character vector with path and name to save output

## Output:
# The function will produce a geographical map that represents areas that have 
# suitable environmental conditions for a species. 

niche.G <- function(Estck, mu, Sigma, save.ras=NULL) {
    # apply suitability function
    suit.rast <- app(x=Estck, fun=sui.fun, mu=mu, Sigma=Sigma)
    # either save as a file or return the raster
    if(!is.null(save.ras)){
      writeRaster(suit.rast, save.ras, overwrite = T)
    }
    return(suit.rast)
}

# FUNCTION 4 -------------------------------------
# Split map of Hawaii into pieces to improve visualization of results
# spn == character string with species name
# basemap == ggplot with basemap to be splitted into islands
# dleg == if TRUE, it displays the legend on the right side
# axes.leg == if TRUE, it displays the Lon and Lat values in the axes

aloha <- function(spn, basemap, dleg=T, axes.leg=T){
  # common colors
  seamap <- "aliceblue"
  framemap <- 'grey5'
  
  # Kauai
  kauai <- basemap +
    labs(title=spn) +
    coord_sf(xlim = c(-159.8,-159.28), ylim = c(21.85,22.25), expand = FALSE) +
    theme(panel.background = element_rect(fill = seamap, color = framemap),
          panel.ontop = FALSE, panel.grid = element_blank(),
          legend.position = "none", plot.title = element_text(face = "italic"))
  # Oahu
  oahu <- basemap +
    coord_sf(xlim = c(-158.3,-157.64), ylim = c(21.25,21.72), expand = FALSE) +
    theme(panel.background = element_rect(fill = seamap, color = framemap),
          panel.ontop = FALSE, panel.grid = element_blank(),
          legend.position = "none")
  # central islands
  central <- basemap +
    coord_sf(xlim = c(-157.07,-155.97), ylim = c(20.5,21.04), expand = FALSE) +
    theme(panel.background = element_rect(fill = seamap, color = framemap),
          panel.ontop = FALSE, panel.grid = element_blank(),
          legend.position = "none")
  # Big island
  if(dleg==T){
    if(axes.leg==T){
    hawaii <- basemap +
      coord_sf(xlim = c(-156.15,-154.75), ylim = c(18.85,20.28), expand = FALSE) +
      theme(panel.background = element_rect(fill = seamap, color = framemap),
            panel.ontop = FALSE, panel.grid = element_blank(),
            axis.text.x = element_text(size=7))
    } else{
      hawaii <- basemap +
        coord_sf(xlim = c(-156.15,-154.75), ylim = c(18.85,20.28), expand = FALSE) +
        theme(panel.background = element_rect(fill = seamap, color = framemap),
              panel.ontop = FALSE, panel.grid = element_blank(),
              legend.box = "vertical", legend.position = "right",
              legend.text = element_markdown())
    }
  }
  if(dleg==F){
    if(axes.leg==T){
      hawaii <- basemap +
      coord_sf(xlim = c(-156.15,-154.75), ylim = c(18.85,20.28), expand = FALSE) +
      theme(panel.background = element_rect(fill = seamap, color = framemap),
            panel.ontop = FALSE, panel.grid = element_blank(),
            axis.text.x = element_text(size=7), legend.position = "none")
    } else{
      hawaii <- basemap +
        coord_sf(xlim = c(-156.15,-154.75), ylim = c(18.85,20.28), expand = FALSE) +
        theme(panel.background = element_rect(fill = seamap, color = framemap),
              panel.ontop = FALSE, panel.grid = element_blank(),
              legend.position = "none")
    }
  }
  map <- kauai + oahu + central + hawaii + plot_layout(nrow = 1, byrow = T)
  return(map)
}

# FUNCTION 5 -------------------------------------
# Creating a suitability map from a raster file using ggplot

# Parameters:
# spn == character string with species name
# sm.species == raster file containing the output values of the model, suitability values in (0,1)
# occ.data == table (matrix/data.frame/tibble) with columns lon,lat containing the 
#             coordinates of presence data
# c1 == color for moderate suitability values
# c2 == color for high suitatility values
# spl == if FALSE, it plots a simple map of the Hawaiian islands.
#       if TRUE, it splits and rearranges the islands into three panels

suit.map <- function(spn, sm.species, occ.data, c1, c2, spl=F){
  # common colors
  c0 <- 'grey70'
  pnts0col <- 'greenyellow'
  seamap <- "aliceblue"
  framemap <- 'grey5'

  # Convert raster to data frame for plots
  sm.df <- as.data.frame(sm.species, xy=T)
  colnames(sm.df) <- c("Longitude","Latitude","Suitability")
  
  # Suitability map for a single species
  if(spl==F){
    map <- ggplot() +
      geom_tile(data = sm.df, aes(x=Longitude, y=Latitude, fill=Suitability)) +
      scale_fill_gradient2("Suitability index",limits=c(0,1), low = c0,
                           mid=c1, high = c2,na.value = NA,
                           midpoint = 0.5, n.breaks=4) +
      geom_point(data = occ.data, aes(x=lon, y=lat), shape = 8,
                 size=0.2, colour = pnts0col, show.legend = T) +
      theme(panel.background = element_rect(fill = seamap, color = framemap),
            panel.ontop = FALSE, panel.grid = element_blank(),
            legend.position = c(0.15, 0.25),
            legend.background = element_rect(fill = seamap)) +
      labs(title=spn)
  } else{
    basemap <- ggplot() +
      geom_tile(data = sm.df, aes(x=Longitude, y=Latitude, fill=Suitability)) +
      geom_point(data = occ.data, aes(x=lon, y=lat), shape = 8,
                 size=0.4, colour = pnts0col, show.legend = T) +
      scale_fill_gradient2("Suitability index",limits=c(0,1), low = c0,
                           mid=c1, high = c2, na.value = NA,
                           midpoint = 0.5, n.breaks=4)
    map <- aloha(spn = spn, basemap = basemap, dleg=T)
  }
  return(map)
}

# FUNCTION 6 -------------------------------------
# Creating a binary raster from a niche model with three variables
# using either the 'mpv' or a confidence level 'alpha'
# mpv = minimum presence value, which is the minimum suitability value
# under the model for a presence record

# Parameters:
# sm.species == raster file containing the output values of the model
# occ.data == table (matrix/data.frame/tibble) with environmental values
#             of the presence data
# mu.hat == estimated values of the niche center 
# s.hat == estimated covariance matrix of the niche model
# method == either 'mpv' or 'alpha' (the default confidence level is 0.95)
# bval == numeric value for the suitable cells; the default value is 1
# save.ras = character vector with path and name to save output

bin3.ras <- function(sm.species, occ.data, mu.hat, s.hat, method, bval=1, save.ras=NULL){
  if(method=='mpv'){
    # apply suitability function to find minimum presence suitability value
    presuit <- apply(X=occ.data, MARGIN=1, FUN=sui.fun, mu=mu.hat, Sigma=s.hat)
    # find minimum presence suitability value
    mpv <- min(presuit)
    #binarize
    rasbin <- function(x) {
      ifelse(x <=  mtp, 0, ifelse(x > mpv, bval, NA))
    }
    speciesbin <- app(sm.species, fun=rasbin)
  }
  if(method=="alpha"){
    # find 95% quantile if the corresponding chi-squared distribution
    qval <- exp(-qchisq(p=0.95, df=3)/2)
    #binarize
    rasbin <- function(x) {
      ifelse(x < qval, 0, ifelse(x >= qval, bval, NA))
    }
    speciesbin <- app(sm.species, fun=rasbin)
  }
  # either save as a file or return the raster
  if(!is.null(save.ras)){
    writeRaster(speciesbin, save.ras, overwrite = T)
  }
  return(speciesbin)
}

# FUNCTION 7 -------------------------------------
# Creating a map from a categorical raster

# Parameters:
# spn == character string with species name
# bmodel == raster file containing the output of the model in binary mode
# pnts == table (matrix/data.frame/tibble) with columns lon,lat containing the 
#      coordinates of presence data

bin.map <- function(spn, bmodel, pnts=NULL, col.pnts=NULL){
  # common colors
  seamap <- "aliceblue"
  framemap <- 'grey5'
  
  # Convert raster to data frame for plots
  bm.df <- as.data.frame(bmodel, xy=T)
  colnames(bm.df) <- c("Lon","Lat","Value")
  
  # Map for a single species
  basemap <- ggplot(data = bm.df, aes(x=Lon, y=Lat)) +
    geom_tile(aes(fill=as.factor(Value))) +
    scale_fill_viridis(discrete = T, option="H", name="Cell status",
                       labels = c('Unsuitable', 'Model 1', 'Model 2', 'Models 1-2')) +
    theme_void()
  if(!is.null(pnts)){
    basemap <- basemap +
      geom_point(data = pnts, aes(x=lon, y=lat),
                 shape = 1, size=1.6, colour = 'greenyellow', show.legend = F)
  }
  map <- aloha(spn=spn, basemap=basemap, dleg=T, axes.leg=F)
  return(map)
}

# FUNCTION 8 -------------------------------------
# Creating a map from a categorical raster

# Parameters:
# spn == character string with species name
# bmodel == raster file containing the output of the model in binary mode
# pnts == table (matrix/data.frame/tibble) with columns lon,lat containing the 
#      coordinates of presence data, and a third column named 'cats' with categories
# col.pnts == vector of colors, with as many column as types of presences

bin.map12 <- function(spn, bmodel, pnts=NULL, col.pnts=NULL){
  # common colors
  seamap <- "aliceblue"
  framemap <- 'grey5'
  # Using same color schem as in 'bin.map' function
  m12 <-  viridis_pal(option = "H")(4)[4]
  
  # Convert raster to data frame for plots
  bm.df <- as.data.frame(bmodel, xy=T)
  colnames(bm.df) <- c("Lon","Lat","Value")
  
  # Map for a single species
  basemap <- ggplot(data = bm.df, aes(x=Lon, y=Lat)) +
    geom_tile(aes(fill=as.factor(Value))) +
    scale_colour_manual(values=c("grey80",m12), aesthetics="fill",
                        guide = guide_legend(order = 1), name="Cell status",
                        labels = c('Unsuitable', 'Suitable (Models 1-2)'))+
    theme_void()
  if(!is.null(pnts)){
    if(!is.null(col.pnts)){
      basemap <- basemap +
        new_scale("colour") +
        geom_point(data = pnts, aes(x=lon, y=lat, colour=as.factor(cats)),
                 size=1.4, shape=8, alpha=0.7) +
        scale_colour_manual(values=col.pnts, aesthetics="colour",
                            guide = guide_legend(order = 2), name="Occurrences",
                            labels = c(paste0("*",spn,"*"), "Non-native species"))
    }
  }
  
  map <- aloha(spn="", basemap=basemap, dleg=T, axes.leg=F)
  return(map)
}

# FUNCTION 9 --------------------------------------------
# Matching the extent and projection of two rasters so we can create a new raster
# by adding them

# Parameters:
# r1 == first raster file, in our case a land cover raster
# r2 == second raster file from which the projection will be taken, in our case
#      a raster with the suitability values from a niche model

add.rmatch <- function(r1, r2){
  # match projection of land-cover and model output
  lc.proj <- terra::project(r1, r2, mask=T, align=T, method="near")
  # match extents so we can combine the rasters
  m1 <- terra::crop(r2, ext(lc.proj))
  lc.crop <- terra::crop(lc.proj, ext(m1))
  # combine the species suitability with the land cover layer
  lc.add <- m1 + lc.crop
  return(lc.add)
}

# END