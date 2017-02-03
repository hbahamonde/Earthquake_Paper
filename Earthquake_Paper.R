cat("\014")
rm(list=ls())

############################
#### Loadings
############################

# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)


###### CHILE ##############
dat.chile <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Chile_Data_Earthquake.csv")

# Transformations
dat.chile$ln.Magnitude = log(dat.chile$Magnitude)
dat.chile$W.Deaths = (dat.chile$Deaths/dat.chile$Population)*100

# save dataset
save(dat.chile, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")


###### PERU ##############
dat.peru <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Peru_Data_Earthquake.csv")


dat.peru$Magnitude = as.numeric(as.character(dat.peru$Magnitude))
dat.peru$Deaths = as.numeric(as.character(dat.peru$Deaths))
dat.peru$W.Deaths = (dat.peru$Deaths/dat.peru$Population)*100


# save dataset
save(dat.peru, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")


############################
#### EARTHQUAKES PLOTS
############################


## CHILE


# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")

eq.chile.p = ggplot(dat.chile, aes(x = year, y = Magnitude, size = W.Deaths)) +
        geom_point(shape = 21) +
        scale_x_continuous(name='Years', limits=c(1900, 2010), breaks = seq(1900, 2010, 10)) +
        scale_y_continuous(name='Magnitude', limits=c(3, 10), breaks = seq(3, 10, 1)) +
        theme_bw() +
        ggtitle("Chile") +
        scale_size("Weighted Deaths") +
        stat_smooth(show.legend = F)



## PERU
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")

# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)


eq.peru.p =ggplot(dat.peru, aes(x = year, y = Magnitude, size = W.Deaths)) +
        geom_point(shape = 21) +
        scale_x_continuous(name='Years', limits=c(1900, 2010), breaks = seq(1900, 2010, 10)) +
        scale_y_continuous(name='Magnitude', limits=c(3, 10), breaks = seq(3, 10, 1)) +
        theme_bw() +
        ggtitle("Peru") +
        scale_size("Weighted Deaths") +
        stat_smooth(show.legend = F)

### plot sharing the same legend
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra,grid)



# grid_arrange_shared_legend function
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

#### MAPS: CHILE


## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rgdal, foreign, rgeos, ggplot2)


# load shape file
chile.provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/CHL_adm_shp", layer = "CHL_adm2")

# load eq data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")
dat.chile <- dat.chile[!(dat.chile$Longitude <= -76),]


#chile.provinces <- gSimplify(chile.provinces, tol=10000, topologyPreserve=T)
chile.provinces <- spTransform(chile.provinces, CRS("+proj=longlat +datum=WGS84"))
chile.provinces <- fortify(chile.provinces)
chile.provinces <- chile.provinces[!(chile.provinces$long <= -76),]


chile.map = ggplot() +  
        geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=chile.provinces, alpha=1/2) +
        theme_bw() +
        ggtitle("Chile") + 
        geom_point(data=subset(dat.chile, year>=1900), aes(x=Longitude, y=Latitude, size=Magnitude), color="red", shape=21)


#### MAPS: Peru

## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rgdal, foreign, rgeos, ggplot2)

# load eq data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")

# load shape file
peru.provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/PER_adm_shp", layer = "PER_adm2")


#peru.provinces <- gSimplify(peru.provinces, tol=10000, topologyPreserve=T)
peru.provinces <- spTransform(peru.provinces, CRS("+proj=longlat +datum=WGS84"))
peru.provinces <- fortify(peru.provinces)
#peru.provinces <- peru.provinces[!(peru.provinces$long <= -76),]


peru.map = ggplot() + 
        geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=peru.provinces, alpha=1/2) +
        theme_bw() +
        ggtitle("Peru") + 
        geom_point(data=subset(dat.peru, year>=1900), aes(x=Longitude, y=Latitude, size=Magnitude), color="red", shape=21)


# grid_arrange_shared_legend function
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
grid_arrange_shared_legend(chile.map, peru.map, ncol = 1, nrow = 2)


### plot both countries
p_load(grid)
grid.draw(cbind(ggplotGrob(chile.map), ggplotGrob(peru.map), size="last"))


############################
#### Models
############################

# Merging

## loading EQ datasets 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")

## subseting EQ datasets
dat.chile <- dat.chile[c("country", "location", "year", "Magnitude", "Deaths", "Latitude", "Longitude", "Population", "Urban", "Sector")]
dat.peru <- dat.peru[c("country", "location", "year", "Magnitude", "Deaths", "Latitude", "Longitude", "Population", "Urban", "Sector")]

## adding rows EQ dataset
earthquakes.d <- rbind(dat.chile,dat.peru)



# Output dataset
## loading output dataset
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE
## subseting EQ datasets (Only Chile and Peru)
logitgee <- logitgee[which(logitgee$country=='Chile' | logitgee$country=='Peru'), ]


# Merging 
eq.output.d <- merge(earthquakes.d, logitgee,by=c("country", "year"), all.x = T) # all.x = T // keeps repeated years.


# recode sector variable
## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)


eq.output.d$Sector <- recode(as.factor(eq.output.d$Sector), "1 = 'Industry' ; 2 = 'Mining' ; 3 = 'Agriculture' ; '1 y 2' = 'Ind and Min' ; '1 y 3' = 'Ind and Agr' ; '2 y 3' = 'Min and Agr' ")


# save dataset
save(eq.output.d, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d.RData")



######################################################################
# Models
######################################################################
cat("\014")
rm(list=ls())

# loading data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d.RData")
dat = eq.output.d # rename dataset
dat <- dat[which(dat$year >= 1900), ] # drop early earthquakes

# dropping NAs
dat = dat[!is.na(dat$Magnitude),]
dat = dat[!is.na(dat$Deaths),]
dat = dat[!is.na(dat$Sector),]
dat = dat[!is.na(dat$Population),]

# rounding lattitude/longitude
dat$r.lat = round(dat$Latitude,1)
dat$r.long = round(dat$Longitude,1)

# formula
fm = as.formula(Deaths ~ constmanufact + constagricult + factor(country) + factor(year) + Magnitude)

# frequentist multilevel model
pvars <- c("constagricult","constmanufact","Magnitude")
datsc <- dat
datsc[pvars] <- lapply(datsc[pvars],scale)


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(lme4,arm)
model <- glmer(Deaths ~ constmanufact + constagricult + Magnitude + (1 | country) + (1 | year), data = datsc, family=poisson)
display(model)

