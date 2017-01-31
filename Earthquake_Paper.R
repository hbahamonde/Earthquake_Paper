cat("\014")
rm(list=ls())


# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)
dat <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Chile_Data_Earthquake.csv")






# map // working version

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(GISTools,rgdal,RColorBrewer,maptools,ggplot2,plyr)

municipalities <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/division_comunal", layer = "division_comunal")

municipalities <- spTransform(municipalities, CRS("+proj=longlat +datum=WGS84"))

municipalities <- fortify(municipalities)

ggplot() + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=municipalities, alpha=0)


## eliminar outliers // eastern island, etc. 


## provincias
provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/division_provincial", layer = "division_provincial")


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(GISTools,rgdal,RColorBrewer,maptools,ggplot2,plyr)

provinces <- spTransform(provinces, CRS("+proj=longlat +datum=WGS84"))

provinces <- fortify(provinces)

ggplot() + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', color='green', data=provinces, alpha=.4)




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



