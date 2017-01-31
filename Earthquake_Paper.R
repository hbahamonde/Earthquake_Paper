cat("\014")
rm(list=ls())


############################
#### EARTHQUAKES PLOTS
############################


## CHILE


# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign,ggplot2)

dat.chile <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Chile_Data_Earthquake.csv")

# Transformations
dat.chile$ln.Magnitude = log(dat.chile$Magnitude)
dat.chile$W.Deaths = (dat.chile$Deaths/dat.chile$Population)*100


eq.chile.p = ggplot(dat.chile, aes(x = year, y = Magnitude, size = W.Deaths)) +
        geom_point(shape = 21) +
        scale_x_continuous(name='Years', limits=c(1900, 2010), breaks = seq(1900, 2010, 10)) +
        scale_y_continuous(name='Magnitude', limits=c(3, 10), breaks = seq(3, 10, 1)) +
        theme_bw() +
        ggtitle("Chile") +
        scale_size("Weighted Deaths")



## PERU


# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign,ggplot2)

dat.peru <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Peru_Data_Earthquake.csv")


dat.peru$Magnitude = as.numeric(as.character(dat.peru$Magnitude))
dat.peru$Deaths = as.numeric(as.character(dat.peru$Deaths))

# Transformations
dat.peru$ln.Magnitude = log(dat.peru$Magnitude)
dat.peru$W.Deaths = (dat.peru$Deaths/dat.peru$Population)*100


eq.peru.p =ggplot(dat.peru, aes(x = year, y = Magnitude, size = W.Deaths)) +
        geom_point(shape = 21) +
        scale_x_continuous(name='Years', limits=c(1900, 2010), breaks = seq(1900, 2010, 10)) +
        scale_y_continuous(name='Magnitude', limits=c(3, 10), breaks = seq(3, 10, 1)) +
        theme_bw() +
        ggtitle("Peru") +
        scale_size("Weighted Deaths")




### plot sharing the same legend
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra,grid)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}



### plot both countries
grid_arrange_shared_legend(eq.chile.p, eq.peru.p, ncol = 1, nrow = 2)




############################
#### MAPS
############################


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(GISTools,rgdal,RColorBrewer,maptools,ggplot2,plyr,rgeos)

municipalities <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/DPA_INE", layer = "Comuna")

municipalities <- gSimplify(municipalities, tol=10000, topologyPreserve=T)

# municipalities <- spTransform(municipalities, CRS("+proj=longlat +datum=WGS84"))
municipalities <- fortify(municipalities)
municipalities <- municipalities[!(municipalities$long <= -76),]


ggplot() + 
        geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=municipalities, alpha=1/2) +
        theme_bw() +
        ggtitle("Chile")



###

## provincias
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(GISTools,rgdal,RColorBrewer,maptools,ggplot2,plyr)


# load shape file
provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/division_provincial", layer = "division_provincial")

# load eq data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)
dat.chile <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Chile_Data_Earthquake.csv")


provinces <- gSimplify(provinces, tol=10000, topologyPreserve=T)
provinces <- spTransform(provinces, CRS("+proj=longlat +datum=WGS84"))
provinces <- fortify(provinces)
provinces <- provinces[!(provinces$long <= -76),]

ggplot() + 
        geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=provinces, alpha=1/2) +
        theme_bw() +
        ggtitle("Chile") +
        geom_point(aes(x = Longitude, y = Latitude, size=Magnitude), shape=21, data = subset(dat.chile, year<1900) , alpha = .5, color="red")




####











####
p_load(rgdal,raster,sp,rgeos)
chile<-getData('GADM', country="CHL", level=0)

poly2df <- function(poly) {
        # Convert the polygons into data frames so we can make lines
        # Number of regions
        n_regions <- length(poly@polygons)
        
        # Get the coords into a data frame
        poly_df <- c()
        for(i in 1:n_regions) {
                # Number of polygons for first region
                n_poly <- length(poly@polygons[[i]]@Polygons)
                print(paste("There are",n_poly,"polygons"))
                # Create progress bar
                pb <- txtProgressBar(min = 0, max = n_poly, style = 3)
                for(j in 1:n_poly) {
                        poly_df <- rbind(poly_df, NA, 
                                         poly@polygons[[i]]@Polygons[[j]]@coords)
                        # Update progress bar
                        setTxtProgressBar(pb, j)
                }
                close(pb)
                print(paste("Finished region",i,"of",n_regions))
        }
        poly_df <- data.frame(poly_df)
        names(poly_df) <- c('lon','lat')
        return(poly_df)
}

chile.mun = poly2df(municipalities)

library(ggplot2)
ggplot() + geom_polygon(data=chile.2, aes(x=lon, y=lat))


data(tornados)
head(as.data.frame(torn))



