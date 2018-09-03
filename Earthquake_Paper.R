cat("\014")
rm(list=ls())
graphics.off()


## ---- number:of:simulations ----
# Hazard Rate simulations 
nsim = 100 # original: 10000
qi = "Hazard Rate" # original: Hazard Rate
ci = 0.95

# Bayesian: Sectoral Model
n.iter.sectoral = 100  # n.iter.sectoral = 200000 // this is for working model
n.burnin.sectoral = 10 # n.burnin.sectoral = 20000 // this is for working model
n.chains.sectoral = 1 # n.chains.sectoral = 4 for the working model

# Bayesian: Tax Model
n.iter.tax = 100  # n.iter.tax = 200000 // this is for working model
n.burnin.tax = 10 # n.burnin.tax = 20000 // this is for working model
n.chains.tax = 1 # n.chains.tax = 4 for the working model
## ---- 


############################
#### Loadings: Chile
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




############################
#### Loadings: Peru
############################
dat.peru <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Peru_Data_Earthquake.csv")


dat.peru$Magnitude = as.numeric(as.character(dat.peru$Magnitude))
dat.peru$Deaths = as.numeric(as.character(dat.peru$Deaths))
dat.peru$W.Deaths = (dat.peru$Deaths/dat.peru$Population)*100


# save dataset
save(dat.peru, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")



############################
#### Loadings: Merging Chile and Peru
############################


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


eq.output.d$Sector2 <- recode(as.factor(eq.output.d$Sector), "1 = 'Industry' ; 2 = 'Mining' ; 3 = 'Agriculture' ; '1 y 2' = 'Ind and Min' ; '1 y 3' = 'Ind and Agr' ; '2 y 3' = 'Min and Agr' ")


## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr)


dat$Sector = as.factor(recode(as.character(dat$Sector), 
                               "1" = "Industry", 
                               "2" = "Industry",
                               "3" = "Agriculture",
                               "1 y 2" = "Industry",
                               "1 y 3" = "Mixed",
                               "2 y 3" = "Mixed"))


# income tax variables
incometax.d.chile = data.frame(ifelse(eq.output.d$year>=1924 & eq.output.d$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.d.peru  = data.frame(ifelse(eq.output.d$year>=1934 & eq.output.d$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.d = incometax.d.chile + incometax.d.peru
eq.output.d <- subset(eq.output.d, select = - incometax.d)

eq.output.d <- data.frame(c(incometax.d,eq.output.d))
colnames(eq.output.d)[1] = "incometax.d"



# save dataset
save(eq.output.d, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d.RData")




############################
#### EARTHQUAKES PLOTS
############################

cat("\014")
rm(list=ls())

## CHILE


## ---- earthquake:ts:plot:chile:data ----
# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")


# subsetting: 1500s-2010
dat.chile.complete = dat.chile


# subsetting: 1900-2010
dat.chile.post1900 <- dat.chile[which(dat.chile$year >= 1900 & dat.chile$country == "Chile"), ] # drop early earthquakes 

dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Magnitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Deaths),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Sector),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Population),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Latitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Longitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Urban),] 


## stores all EQs (without concerns of data availability; i.e. population, etc.)
cases.all = nrow(dat.chile.complete)
cases.post1900 = nrow(dat.chile.post1900)

dat.chile.complete$Included = factor(ifelse(
  dat.chile.complete$year >= min(dat.chile.post1900$year) &
    !is.na(dat.chile.complete$Magnitude) &
    !is.na(dat.chile.complete$Deaths) & 
    !is.na(dat.chile.complete$Sector) &
    !is.na(dat.chile.complete$Population) &
    !is.na(dat.chile.complete$Latitude) &
    !is.na(dat.chile.complete$Longitude) & 
    !is.na(dat.chile.complete$Urban), 1, 0),
  levels = c(0,1),labels = c("No", "Yes"))




# time-series plot
time.series.plot <- ggplot(dat.chile.complete, aes(x = year, y = Magnitude)) +
  geom_point(shape = 21, aes(fill = dat.chile.complete$Included)) +
  theme_bw() +
  xlab("Year") +
  ggtitle(NULL) +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        legend.position="bottom") +
  guides(fill=guide_legend(title="Included in Analyses"))#+
#stat_smooth(show.legend = F,  method = 'loess')
## ----



## ---- earthquake:ts:plot:chile:plot ----
# dropping NAs 
time.series.plot

time.series.plot.note <- paste(
  paste("{\\bf Earthquakes in Chile: 1500-2010}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste(paste("{\\bf Note}: Figure shows earthquakes available in the \\href{https://data.noaa.gov/dataset}{NOAA dataset} (N=", cases.all,").", sep = ""), "Due to data availability at the local level (local population, for example), however, it was only possible to include in the analyses the earthquakes that took place begining in", paste(min(dat.chile.post1900$year), ".", sep="")), " \\autoref{fig:earthquake:map:plot:chile} shows the geographical location of the actual observations used in the analyses", sep = ""), paste("(",paste("N=",cases.post1900, sep = ""),").", sep = "")),
  "\n")
## ----



## bar plot
Deaths = data.frame(dat.chile$Deaths); colnames(Deaths)[1] <- "Deaths"
ggplot(na.omit(Deaths), aes(factor(Deaths))) + 
  geom_bar(width=.8) + 
  scale_x_discrete(name='Deaths') +
  geom_hline(yintercept = c(1,10,20),  colour= "red", linetype = "dashed", size=1.4) +
  scale_y_continuous(name='Count') +
  theme_bw() + 
  ggtitle("") + #Death Tolls Associated to Earthquakes: Chile 1500-2010
  coord_flip() +
  ggsave("/Users/hectorbahamonde/RU/Dissertation/Presentation/Resources/count_plot.pdf", dpi = 1000, width = 128, height = 96, units = c("mm"))



# another kind-of TS plot.
# ggplot(na.omit(dat.chile), aes(x=year, y=Deaths)) + geom_line() + theme_bw()                    

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






## ---- earthquake:map:data:chile ----
## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rgdal, foreign, rgeos, ggplot2)


# load shape file
chile.provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Data/shape_files/division_provincial", 
                           layer = "division_provincial",
                           verbose = FALSE)

#chile.provinces <- gSimplify(chile.provinces, tol=10000, topologyPreserve=T)
chile.provinces <- spTransform(chile.provinces, CRS("+proj=longlat +datum=WGS84"))
chile.provinces <- fortify(chile.provinces)
# chile.provinces <- chile.provinces[!(chile.provinces$long <= -76),]



# load eq data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

# rename df
dat.chile = dat

#dat.chile <- dat.chile[!(dat.chile$Longitude <= -76),]

dat.chile = dat.chile[!is.na(dat.chile$Magnitude),] 
dat.chile = dat.chile[!is.na(dat.chile$Deaths),] 
dat.chile = dat.chile[!is.na(dat.chile$Sector),] 
dat.chile = dat.chile[!is.na(dat.chile$Population),] 
dat.chile = dat.chile[!is.na(dat.chile$Latitude),] 
dat.chile = dat.chile[!is.na(dat.chile$Longitude),] 
dat.chile = dat.chile[!is.na(dat.chile$Urban),] 


chile.map.plot.d <- data.frame(
  Longitude = na.omit(dat.chile$Longitude),
  Latitude = na.omit(dat.chile$Latitude),
  Magnitude = na.omit(dat.chile$Magnitude),
  Sector = na.omit(dat.chile$Sector),
  Year = na.omit(dat.chile$year)) 



# chile.map.plot.d$Sector<- as.character(chile.map.plot.d$Sector)
# chile.map.plot.d$Sector[chile.map.plot.d$Sector==""] <- NA
# chile.map.plot.d$Sector <- as.factor(chile.map.plot.d$Sector)


# prepare DF for plot
chile.map.plot.d = data.frame(na.omit(chile.map.plot.d));rownames(chile.map.plot.d) <- NULL

# plot
earthquake.map.plot.chile = ggplot() +  
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=chile.provinces, alpha=1/2) +
  theme_bw() +
  ggtitle(NULL) + 
  geom_point(data=chile.map.plot.d, aes(x=Longitude, y=Latitude,colour=Sector, shape=as.factor(round(chile.map.plot.d$Magnitude,0)))) +#shape=21, 
  scale_y_continuous(name='Latitude') +
  scale_x_continuous(name='Longitude') +
  scale_shape_discrete(name="Rounded\nMagnitude") +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7))
## ----






## ---- earthquake:map:plot:chile ----
# plot
earthquake.map.plot.chile
earthquake.map.note.chile <- paste(
  paste("{\\bf Data Used in the Analyses: Geographical Distribution of Earthquakes in Chile,", paste(paste(min(chile.map.plot.d$Year), max(chile.map.plot.d$Year), sep="-"), "}.", sep = "" ), sep=" "),
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}:", paste(paste(paste("Using a combination of archival information and external sources, the figure shows a total of", nrow(chile.map.plot.d), ""), "earthquakes.", sep = ""), "Each quake was colorized according to the predominant economic sector at the municipal level. In total, there were", as.numeric(table(chile.map.plot.d$Sector)["Agriculture"]), "earthquakes that took place in agricultural localities,", as.numeric(table(chile.map.plot.d$Sector)["Industry"]), "in industrial, and", as.numeric(table(chile.map.plot.d$Sector)["Mixed"]), "in mixed municipalities. \\autoref{fig:earthquake:ts:plot:chile:plot} shows the overtime variation.",   sep = " "), sep=" "),
  "\n")
## ----



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



###################################################################### 
# Models
###################################################################### 


# ---- loading:datasets:models ----
# Prepping Chilean data
cat("\014") 
rm(list=ls()) 

# loading data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")

# subsetting: 1900-2010
dat.chile.post1900 <- dat.chile[which(dat.chile$year >= 1900 & dat.chile$country == "Chile"), ] # drop early earthquakes 

dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Magnitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Deaths),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Sector),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Population),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Latitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Longitude),] 
dat.chile.post1900 = dat.chile.post1900[!is.na(dat.chile.post1900$Urban),] 

dat = dat.chile.post1900

# rounding lattitude/longitude 
dat$r.lat = round(dat$Latitude,1) 
dat$r.long = round(dat$Longitude,1) 

# 
dat$country <- factor(dat$country)
dat$country <- droplevels(dat$country)
dat$country <- as.integer(dat$country)

# year as a counter variable
# dat$year = 1:nrow(dat) # year as a counting variable

# weight population 
dat$w.Deaths = round((dat$Deaths/dat$Population),5)*10


# recode sector
dat$Sector = as.factor(recode(as.character(dat$Sector), 
                              "1" = "Industry", 
                              "2" = "Industry",
                              "3" = "Agriculture",
                              "1 y 2" = "Industry",
                              "1 y 3" = "Mixed",
                              "2 y 3" = "Mixed"))

# proportion of Population 
dat$p.Population = dat$Population/1000 
save(dat, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData")
# ----







###################################################################### 
cat("\014") 
rm(list=ls()) 

load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 


# overdispersion
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(AER)
o.Deaths <- glm(Deaths ~ ., data = dat, family = poisson)
dispersiontest(o.Deaths,alternative = c("greater"), trafo=1) # overdispersion is -0.5


###################################################################### 










###############################
# Sectoral Contestation Model
###############################

# cat("\014")
# rm(list=ls())
# graphics.off()



# How to set up JAGS and R on a Mac
# Install the most recent version of R from the CRAN website.
# Download and install RStudio™.
# Install Clang (clang-6.0.0.pkg) and GNU Fortran (gfortran-6.1.pkg.dmg) from the CRAN tools directory.
# Now install JAGS version 4.3.0 (JAGS-4.3.0.dmg) from Martyn Plummer's repository. Detailed instructions quoted from the JAGS readme file:
# Download the disk image from the JAGS website.
# Ensure that the 'Allow apps downloaded from anywhere' box is selected in the Security and Privacy (General) pane of System Preferences.
# Double click on the disk image to mount (this may not be required).
# Double click on the 'JAGS-4.3.0.mpkg' file within the mounted disk image.
# Follow the instructions in the installer.
# Authenticate as the administrative user. The first user you create when setting up Mac OS X has administrator privileges by default.
# Start the Terminal and type jags to see if you receive the message: Welcome to JAGS 4.3.0.
# from: http://www.jkarreth.net/bayes-icpsr.html



## ---- sectoral:model:and:data:not:run ----

# load data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData")


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)







# specify the model
model.jags.sectoral <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        #b.propagrmanu[Sector[i]]*propagrmanu[i] + # multi-level part: allow national output to vary at the local/sector level
                        b.Magnitude[Sector[i]]*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        b.year[yearID[i]] + # year fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] +
                        mu ## intercept
        }
        b.r.lat ~ dnorm(0,1)
        b.r.long ~ dnorm(0,1)
        mu  ~ dnorm(0,1) ## intercept
        b.p.Population ~ dnorm(0,1)
        b.Urban ~ dnorm(0,1)
        
        
        for (t in 1:yearN){ # fixed effects
                b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t])
                
                m.b.year[t] ~ dnorm(0,1)
                tau.b.year[t] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
        ## Varying Slopes for Sector (unmodeled)
        for (k in 1:NSector){ # 
                b.Magnitude[k] ~ dnorm(m.Magnitude[k], tau.Magnitude[k])
                m.Magnitude[k] ~ dnorm(0,1)
                tau.Magnitude[k] ~ dgamma(0.5, 0.001) # uninformative prior
        }
}



# define the vectors of the data matrix for JAGS.
w.Deaths <- as.vector(dat$w.Deaths)
Deaths <- as.vector(dat$Deaths)
constmanufact <- as.vector(dat$constmanufact)
constagricult <- as.vector(dat$constagricult)
Magnitude <- as.vector(dat$Magnitude)
p.Population <- as.vector(dat$p.Population)
country <- as.numeric(as.ordered(dat$country))
Ncountry <-  as.numeric(as.vector(length(unique(as.numeric(dat$country)))))
N <-  as.numeric(nrow(dat))
year = as.vector(dat$year)
yearID = as.factor(as.ordered(dat$year))
yearN = length(unique(dat$year))
Sector = as.vector(as.numeric(factor(dat$Sector)))
NSector = as.numeric(as.vector(length(unique(as.numeric(dat$Sector)))))
NIncometax = as.vector(length(unique(as.numeric(dat$incometax.d))))
incometax.d = as.vector(as.numeric(dat$incometax.d))
NIncometax.y = as.vector(length(unique(as.numeric(dat$incometax.y))))
incometax.y = as.vector(as.numeric(dat$incometax.y))
Urban = as.vector(as.numeric(dat$Urban))
customtax = as.vector(as.numeric(dat$customtax))/100
r.long = as.vector(as.numeric(dat$r.long))
r.lat = as.vector(as.numeric(dat$r.lat))





jags.data.sectoral <- list(Deaths = Deaths,
                           #propagrmanu = propagrmanu, # constagricult/constmanufact
                           Magnitude = Magnitude^2,
                           Sector = Sector,
                           NSector = NSector,
                           p.Population = p.Population,
                           Urban = Urban,
                           r.long = r.long,
                           r.lat = r.lat,
                           yearID = yearID,
                           yearN = yearN,
                           N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.sectoral <- c("b.Magnitude", "b.p.Population", "b.year", "b.r.long", "b.r.lat", "b.Urban", "lambda")
## ----





## ---- sectoral:model:and:data:does:run ----
# run the model
# n.iter.sectoral = 200000  # n.iter.sectoral = 200000 // this is for working model
# n.burnin.sectoral = 20000 # n.burnin.sectoral = 5000 // this is for working model
# n.chains.sectoral = 4 # n.chains.sectoral = 4 for the working model

earthquakefit.sectoral <- jags(
  data=jags.data.sectoral,
  inits=NULL,
  parameters.to.save = eq.params.sectoral,
  n.chains=n.chains.sectoral,
  n.iter=n.iter.sectoral,
  n.burnin=n.burnin.sectoral, 
  model.file=model.jags.sectoral,
  progress.bar = "none"
  )





#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc.sectoral <- as.mcmc(earthquakefit.sectoral)
bayes.mod.fit.gg.sectoral <- ggs(fit.mcmc.sectoral)
ggmcmc(bayes.mod.fit.gg.sectoral, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Sectoral_Competition.pdf")
graphics.off()
## ----





###############################
# Sectoral Contestation Plot
###############################

## ---- sectoral:model:plot:not:run ----
## passing fitted model as mcmc object
sect.contest.mcmc <- as.mcmc(earthquakefit.sectoral)
sect.contest.mcmc.mat <- as.matrix(sect.contest.mcmc)
sect.contest.mcmc.dat <- as.data.frame(sect.contest.mcmc.mat)

## range of interest
prop.range <- seq(min(propagrmanu), max(propagrmanu), by = 0.01)


#############################
### Agricultural Subnational
sect.contest.sim.prop.agr <- matrix(rep(NA, nrow(sect.contest.mcmc.dat)*length(prop.range)), nrow = nrow(sect.contest.mcmc.dat))

for(i in 1:length(prop.range)){
  sect.contest.sim.prop.agr[, i] <- sect.contest.mcmc.dat$'b.propagrmanu[1]'*prop.range[i]
}

## credible intervals
bayes.c.eff.mean.prop.agr <- apply(sect.contest.sim.prop.agr, 2, mean)
bayes.c.eff.lower.prop.agr <- apply(sect.contest.sim.prop.agr, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.agr <- apply(sect.contest.sim.prop.agr, 2, function(x) quantile(x, probs = c(0.8)))

# create DF
plot.dat.prop.agr <- data.frame(prop.range, bayes.c.eff.mean.prop.agr, bayes.c.eff.lower.prop.agr, bayes.c.eff.upper.prop.agr)
colnames(plot.dat.prop.agr) <- c("prop.range", "mean", "lower", "upper")


#############################
### Industrial Subnational
plot.dat.prop.ind <- matrix(rep(NA, nrow(sect.contest.mcmc.dat)*length(prop.range)), nrow = nrow(sect.contest.mcmc.dat))
for(i in 1:length(prop.range)){
  plot.dat.prop.ind[, i] <- sect.contest.mcmc.dat$'b.propagrmanu[2]'*prop.range[i]
}

## credible intervals
bayes.c.eff.mean.prop.ind <- apply(plot.dat.prop.ind, 2, mean)
bayes.c.eff.lower.prop.ind <- apply(plot.dat.prop.ind, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.ind <- apply(plot.dat.prop.ind, 2, function(x) quantile(x, probs = c(0.8)))

# create DF
plot.dat.prop.ind <- data.frame(prop.range, bayes.c.eff.mean.prop.ind, bayes.c.eff.lower.prop.ind, bayes.c.eff.upper.prop.ind)
colnames(plot.dat.prop.ind) <- c("prop.range", "mean", "lower", "upper")


# create final DF
sectoral.competition.plot = as.data.frame(rbind(
  as.data.frame(cbind(plot.dat.prop.agr, `Subnational sector\nmostly:`= rep("Agricultural", nrow(plot.dat.prop.ind)))),
  as.data.frame(cbind(plot.dat.prop.ind, `Subnational sector\nmostly:`= rep("Industrial", nrow(plot.dat.prop.agr))))))

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)
## ----



# plot
## ---- sectoral:model:plot:run ----
ggplot() + 
  geom_line(data = sectoral.competition.plot, aes(x = prop.range, y = mean, colour = `Subnational sector\nmostly:`), alpha = 0.8, size = 0.5) + 
  geom_ribbon(data = sectoral.competition.plot, aes(x = prop.range, ymin = lower, ymax = upper, fill = `Subnational sector\nmostly:`), alpha = 0.2) + 
  xlab("National Proportion Agriculture/Industry Output\n[National Contestation]") + ylab("Death-Toll\n(Posterior Predictions)") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        legend.position="bottom") +
  scale_fill_manual(values=c("red", "green")) +
  scale_color_manual(values=c("red", "green"))
## ---- 







###############################
# Sectoral Contestation Table
###############################

## ---- sectoral:model:regression:table:not:run ----

# R function for summarizing MCMC output in a regression-style table
# Johannes Karreth, thanks for the function!
## SOURCE: https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R

# I use this function mainly for teaching.

# The function produces a table with means, SDs, credible intervals, and
# the % of posterior draws below/above 0 from MCMC output from 
# R2jags, rjags, R2WinBUGS, R2OpenBUGS, and MCMCpack

# Depends on packages: coda, rstan (if working with rstan objects)

# Arguments: 
# sims: output from R2jags, rjags, R2WinBUGS, R2OpenBUGS, MCMCpack, rstan
# ci: desired credible interval, default: 0.95
# digits: desired number of digits in the table, default: 2


ci.number.sectoral = .95 # modify this parameter to get desired credible intervals.

mcmctab <- function(sims, ci = ci.number.sectoral, digits = 2){
  
  require(coda) 
  
  if(class(sims) == "jags" | class(sims) == "rjags"){
    sims <- as.matrix(as.mcmc(sims))
  }
  if(class(sims) == "bugs"){
    sims <- sims$sims.matrix
  }  
  if(class(sims) == "mcmc"){
    sims <- as.matrix(sims)
  }    
  if(class(sims) == "mcmc.list"){
    sims <- as.matrix(sims)
  }      
  if(class(sims) == "stanfit"){
    stan_sims <- rstan::As.mcmc.list(sims)
    sims <- as.matrix(stan_sims)
  }      
  
  
  dat <- t(sims)
  
  mcmctab <- apply(dat, 1,
                   function(x) c(Mean = round(mean(x), digits = digits), # Posterior mean
                                 SD = round(sd(x), digits = 3), # Posterior SD
                                 Lower = as.numeric(
                                   round(quantile(x, probs = c((1 - ci) / 2)), 
                                         digits = digits)), # Lower CI of posterior
                                 Upper = as.numeric(
                                   round(quantile(x, probs = c((1 + ci) / 2)), 
                                         digits = digits)), # Upper CI of posterior
                                 Pr. = round(
                                   ifelse(mean(x) > 0, length(x[x > 0]) / length(x),
                                          length(x[x < 0]) / length(x)), 
                                   digits = digits) # Probability of posterior >/< 0
                   ))
  return(t(mcmctab))
}



reg.results.table.sectoral = data.frame(mcmctab(earthquakefit.sectoral)[1:10,]) # Posterior distributions // Year FE excluded


reg.results.table.sectoral = data.frame(rbind( # re order df by name of the rowname according to what I have and define in 'var.labels.sectoral.'
  #reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[1]"),],
  #reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[2]"),],
  #reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[3]"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[1]"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[2]"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[3]"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.r.lat"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.r.long"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.p.Population"),],
  reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Urban"),]
))

var.labels.sectoral = c(#"Agr/Ind [Agr]", 
  #"Agr/Ind [Ind]", 
  #"Agr/Ind [Mixed]", 
  "Magnitude [Agr]", 
  "Magnitude [Ind]", 
  "Magnitude [Mixed]", 
  "Latitude", 
  "Longitude",
  "Population", 
  "Urban")

rownames(reg.results.table.sectoral) <- var.labels.sectoral



# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(xtable)

note.sectoral <- paste0(
  "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", format(round(as.numeric(n.iter.sectoral), 0), nsmall=0, big.mark=","), " iterations with a burn-in period of n = ", format(round(as.numeric(n.burnin.sectoral), 0), nsmall=0, big.mark=","), " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.sectoral*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects were omitted in the table.}\\\\", 
  "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains.sectoral, " chains were run. Detailed diagnostic plots available  \\href{https://github.com/hbahamonde/Earthquake_Paper/raw/master/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Sectoral_Competition.pdf}{\\texttt here}.} \\\\")
## ----


## ---- sectoral:model:regression:table:run ----
print.xtable(xtable(
  reg.results.table.sectoral, 
  caption = "{\\bf Simulated Posterior Predictions (Hierarchical Poisson Regression, \\autoref{model:1})}.",
  label = "sectoral:model:regression:table:run"), 
  auto = TRUE,
  hline.after=c(-1, 0),
  add.to.row = list(pos = list(length(var.labels.sectoral)), 
                    command = note.sectoral),
  floating=TRUE,
  type="latex",
  table.placement = "H"
)
## ----











###############################
# Income Tax Adoption Model
###############################

# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations


# HERE
model.jags.tax <- function() {
        for(i in 1:N){ # loop through all data points
                for(i in 1:N){
                        Deaths[i] ~ dpois(mu[i])
                        mu[i] <- lambda[i]*z[i] + 0.00001 ## hack required for Rjags -- otherwise 'incompatibl
                        z[i] ~ dbern(psi)
                        log(lambda[i]) <- 
                                b.Magnitude * Magnitude[i] + 
                                b.incometax.d * incometax.d[i] +
                                b.interaction * Magnitude[i] + incometax.d[i] +
                                b.p.Population * p.Population[i] +
                                # b.Urban*Urban[i] +
                                b.r.long * r.long[i] +
                                b.r.lat * r.lat[i] + 
                                #b.Sector*Sector[i] +
                                b.year[yearID[i]] #+ 
                        + alpha # alpha is overall intercept
                }
                # priors:
                alpha ~ dnorm(0, 0.01) # overall model intercept 
                psi ~ dunif(0, 1) # proportion of non-zeros
                }
}


# cat("\014")
# rm(list=ls())
# graphics.off()

## ---- income:tax:model:and:data:not:run ----
# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
# options(scipen=10000)
set.seed(602)

# specify the model
model.jags.tax <- function() {
        for(i in 1:N){ # loop through all data points
                for(i in 1:N){
                        Deaths[i] ~ dpois(lambda.hacked[i])
                        lambda.hacked[i] <- lambda[i]*(1-zero[i]) + 1e-10*zero[i] ## hack required for Rjags -- otherwise 'incompatibl
                        
                        
                        log(lambda[i]) <- 
                                b.Magnitude * Magnitude[i] + 
                                b.incometax.d * incometax.d[i] +
                                b.interaction * Magnitude[i] * incometax.d[i] +
                                b.p.Population * p.Population[i] +
                                b.Urban * Urban[i] +
                                b.r.long * r.long[i] +
                                b.r.lat * r.lat[i] + 
                                #b.Sector*Sector[i] +
                                b.year[yearID[i]] + 
                                alpha # alpha is overall intercept
                        
                        z[i] ~ dbern(psi)
                       
                }
                # priors:
               alpha ~ dnorm(0, 0.01) # overall model intercept 
                psi ~ dunif(0, 1) # proportion of non-zeros

                b.Magnitude ~ dnorm(0,1e6)
                b.incometax.d ~ dnorm(0,1e6)
                b.interaction ~ dnorm(0,1e6)
                b.p.Population ~ dnorm(0,1e6)
                b.Urban ~ dnorm(0,1e6)
                b.r.long ~ dnorm(0,1e6)
                b.r.lat ~ dnorm(0,1e6)

                
                # year fixed effects 
                for (t in 1:yearN){
                        b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t]) 
                        
                        m.b.year[t] ~ dnorm(0,1e6)
                        tau.b.year[t] ~ dgamma(1,1) # uninformative prior 
                } 
        }
}



## notas:
## ZIP:  
        ### https://georgederpa.github.io/teaching/countModels.html
        ### https://biometry.github.io/APES/LectureNotes/2016-JAGS/ZeroInflation/ZeroInflation_JAGS.pdf




# define the vectors of the data matrix for JAGS.
w.Deaths <- as.vector(dat$w.Deaths)
Deaths <- as.vector(dat$Deaths)
constmanufact <- as.vector(dat$constmanufact)
constagricult <- as.vector(dat$constagricult)
Magnitude <- as.vector(dat$Magnitude)
ln.Magnitude <- as.vector(dat$ln.Magnitude)
p.Population <- as.vector(dat$p.Population)
Population <- as.vector(dat$Population)
country <- as.numeric(as.ordered(dat$country))
Ncountry <-  as.numeric(as.vector(length(unique(as.numeric(dat$country)))))
N <-  as.numeric(nrow(dat))
year = as.vector(dat$year)
yearID = as.factor(as.ordered(dat$year)) #as.numeric(as.ordered(dat$year))
yearN = length(unique(dat$year))
Sector = as.vector(as.numeric(factor(dat$Sector)))
NSector = as.numeric(as.vector(length(unique(as.numeric(dat$Sector)))))
NIncometax = as.vector(length(unique(as.numeric(dat$incometax.d))))
incometax.d = as.vector(as.numeric(ifelse(dat$year>=1924,1,0)))
NIncometax.y = as.vector(length(unique(as.numeric(dat$incometax.y))))
incometax.y = as.vector(as.numeric(dat$incometax.y))
Urban = as.vector(as.numeric(dat$Urban))
customtax = as.vector(as.numeric(dat$customtax))/100
r.long = as.vector(as.numeric(dat$r.long))
r.lat = as.vector(as.numeric(dat$r.lat))





jags.data.tax <- list(Deaths = Deaths,
                      Magnitude = Magnitude^2,
                      incometax.d = incometax.d,
                      p.Population = p.Population,
                      Urban = Urban,
                      r.long = r.long,
                      r.lat = r.lat,
                      Sector = Sector,
                      yearID = yearID,
                      yearN = yearN,
                      N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.tax <- c(
  "b.Magnitude", 
  "b.incometax.d",
  "b.interaction",
  "b.p.Population", 
  #"b.Urban", 
  "b.r.long", 
  "b.r.lat", 
  "b.year",
  "lambda"
  )
## ----


## ---- income:tax:model:and:data:run ----
# run the model
# n.iter.tax = 1000  # n.iter.tax = 200000 // this is for working model
# n.burnin.tax = 400 # n.burnin.tax = 5000 // this is for working model
# n.chains.tax = 2 # n.chains.tax = 4 for the working model

earthquakefit.tax <- jags(
  data=jags.data.tax,
  inits=NULL,
  parameters.to.save = eq.params.tax,
  n.chains = n.chains.tax,
  n.iter = n.iter.tax,
  n.burnin = n.burnin.tax, 
  #n.thin = 10,
  model.file=model.jags.tax#,
  #progress.bar = "none"
  )


plot(earthquakefit.tax)



#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc.tax <- as.mcmc(earthquakefit.tax)
bayes.mod.fit.gg.tax <- ggs(fit.mcmc.tax)
ggmcmc(bayes.mod.fit.gg.tax, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Income_Tax_Model.pdf")
graphics.off()
## ----


###############################
# Income Tax Adoption Plot
###############################
tax.mcmc <- as.mcmc(earthquakefit.tax)
tax.mcmc.mat <- as.matrix(tax.mcmc)
tax.mcmc.dat <- as.data.frame(tax.mcmc.mat)

# HERE
x2.sim <- seq(min(jags.data.tax$Magnitude), max(jags.data.tax$Magnitude), by = 0.1)

# Simulate the range of the moderating variable
x2.sim <- seq(min(jags.data.tax$incometax.d), max(jags.data.tax$incometax.d), by = 0.01)

## Calculate conditional effect of X1 across the range of X2
int.sim <- matrix(rep(NA, nrow(tax.mcmc.dat)*length(x2.sim)), nrow = nrow(tax.mcmc.dat))

for(i in 1:length(x2.sim)){
  int.sim[, i] <- tax.mcmc.dat$b.Magnitude + tax.mcmc.dat$b.interaction * x2.sim[i]
}

## Note: the variance now comes from the posterior, not the vcov matrix

bayes.c.eff.mean <- apply(int.sim, 2, mean)
bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))


plot.dat <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)


library(ggplot2)
ggplot(plot.dat, aes(x = x2.sim, y = bayes.c.eff.mean)) + geom_line(color = "blue", alpha = 0.8, size = 0.5) + 
  xlab("X2") + ylab("Conditional effect of X1") + theme_bw() + 
  geom_ribbon(aes(ymin = bayes.c.eff.lower, ymax = bayes.c.eff.upper), fill = "blue", alpha = 0.2)  + 
  geom_line(aes(x = x2.sim, y = bayes.c.eff.lower), color = "blue", alpha = 0.8, size = 0.5) + geom_line(aes(x = x2.sim, y = bayes.c.eff.upper), color = "blue", alpha = 0.8, size = 0.5)





## ---- income:tax:model:plot:not:run ----
## passing fitted model as mcmc object
tax.mcmc <- as.mcmc(earthquakefit.tax)
tax.mcmc.mat <- as.matrix(tax.mcmc)
tax.mcmc.dat <- as.data.frame(tax.mcmc.mat)

# define confidence interval
CI.level.income.tax.ts.plot = c(0.05, 0.5, 0.95) # 95%
# CI.level.income.tax.ts.plot = c(0.2, 0.5, 0.8) # 80%



### No Income Tax
year.range = unique(dat$year)

# simulation
sim.no.income.tax <- matrix(rep(NA, nrow(tax.mcmc.dat)*length(year.range)), nrow = nrow(tax.mcmc.dat)) 



for(i in 1:length(year.range)){
  sim.no.income.tax[, i] <- tax.mcmc.dat$b.p.Population*p.Population[i] + 
    tax.mcmc.dat$b.r.lat*r.lat[i] + 
    tax.mcmc.dat$b.r.long*r.long[i] + 
    tax.mcmc.dat$b.Urban*Urban[i] +
    tax.mcmc.dat$b.Magnitude*Magnitude[i] +
    incometax.d[i]*0 
  }


## credible intervals
sim.no.income.tax.d = data.frame(apply(sim.no.income.tax, 2, function(x) quantile(x, probs = CI.level.income.tax.ts.plot)))[,1:9]  

bayes.c.eff.mean.no.income.tax = as.numeric(sim.no.income.tax.d[2,])
bayes.c.eff.lower.no.income.tax = as.numeric(sim.no.income.tax.d[1,])
bayes.c.eff.upper.no.income.tax = as.numeric(sim.no.income.tax.d[3,])

# create DF
plot.dat.no.income.tax <- data.frame(year.range[1:9], bayes.c.eff.mean.no.income.tax, bayes.c.eff.lower.no.income.tax, bayes.c.eff.upper.no.income.tax); colnames(plot.dat.no.income.tax) <- c("year.range", "mean", "lower", "upper")

plot.dat.no.income.tax = plot.dat.no.income.tax[ which(plot.dat.no.income.tax$year.range<= 1924), ]



### Income Tax
year.range = unique(dat$year)

# simulation
sim.income.tax <- matrix(rep(NA, nrow(tax.mcmc.dat)*length(year.range)), nrow = nrow(tax.mcmc.dat))
for(i in 1:length(year.range)){
  sim.income.tax[, i] <-
    tax.mcmc.dat$b.p.Population*p.Population[i] + 
    tax.mcmc.dat$b.r.lat*r.lat[i] + 
    tax.mcmc.dat$b.r.long*r.long[i] + 
    tax.mcmc.dat$b.Urban*Urban[i] +
    tax.mcmc.dat$b.Magnitude*Magnitude[i] +
    tax.mcmc.dat$b.incometax.d*incometax.d[i]
  
}

## credible intervals
sim.income.tax.d = data.frame(apply(sim.income.tax, 2, function(x) quantile(x, probs = CI.level.income.tax.ts.plot)))[,10:59]  

bayes.c.eff.mean.income.tax = as.numeric(sim.income.tax.d[2,])
bayes.c.eff.lower.income.tax = as.numeric(sim.income.tax.d[1,])
bayes.c.eff.upper.income.tax = as.numeric(sim.income.tax.d[3,])



# create DF
plot.dat.income.tax <- data.frame(year.range[10:59], bayes.c.eff.mean.income.tax, bayes.c.eff.lower.income.tax, bayes.c.eff.upper.income.tax); colnames(plot.dat.income.tax) <- c("year.range", "mean", "lower", "upper")

plot.dat.income.tax = plot.dat.income.tax[ which(plot.dat.income.tax$year.range>= 1924), ]



# income tax adoption plot DF
income.tax.adoption.plot = as.data.frame(rbind(
  as.data.frame(cbind(plot.dat.no.income.tax, 'Income Tax'= rep("No", nrow(plot.dat.no.income.tax)))),
  as.data.frame(cbind(plot.dat.income.tax, 'Income Tax'= rep("Yes", nrow(plot.dat.income.tax))))))

death.toll.before.tax = round(mean(income.tax.adoption.plot$mean[income.tax.adoption.plot$year.range <= 1924]),0)
death.toll.after.tax = round(mean(income.tax.adoption.plot$mean[income.tax.adoption.plot$year.range >= 1924]), 0)


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

# plot
income.tax.model.plot = ggplot() + 
  geom_smooth(data = income.tax.adoption.plot, aes(x = year.range, y = mean, colour = `Income Tax`), alpha = 0.8, size = 0.5, se = F, method = 'loess') +
  geom_ribbon(data = income.tax.adoption.plot, aes(x = year.range, ymin = lower, ymax = upper, fill = `Income Tax`), alpha = 0.2) + 
  geom_vline(xintercept = 1924, linetype=2, colour="blue") + 
  xlab("Year") + ylab("Death-Toll\n(Posterior Predictions)") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        legend.position="bottom") +
  scale_fill_manual(values=c("red", "green")) +
  scale_color_manual(values=c("red", "green"))
## ----

## ---- income:tax:model:plot:run ----
income.tax.model.plot
income.tax.model.plot.note <- paste(
  "{\\bf Income Taxation and State Capacity in Chile: An Overtime Approach}.",
  "\\\\\\hspace{\\textwidth}",
  paste(paste(paste(paste("{\\bf Note}: Using the estimations from \\autoref{income:tax:model:regression:table:run} (\\autoref{model:2}), the figure shows predicted death-tolls before and after the implementation of the income tax in 1924. In average, the death-toll decreases from"), death.toll.before.tax, "to", sep = " "), death.toll.after.tax, sep= " "), ".", sep=""), paste(paste(paste("The figure suggests that implementing the income tax law had positive effects on state-capacity overtime. The figure also shows credible intervals at the", CI.level.income.tax.ts.plot[3]*100, sep = " "), "\\%", sep = ""), "level.", sep = " "), sep = " ")
## ----

###############################
# Income Tax Adoption Table
###############################


## ---- income:tax:model:regression:table:not:run ----
# R function for summarizing MCMC output in a regression-style table
# Johannes Karreth, thanks for the function!
## SOURCE: https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R

# I use this function mainly for teaching.

# The function produces a table with means, SDs, credible intervals, and
# the % of posterior draws below/above 0 from MCMC output from 
# R2jags, rjags, R2WinBUGS, R2OpenBUGS, and MCMCpack

# Depends on packages: coda, rstan (if working with rstan objects)

# Arguments: 
# sims: output from R2jags, rjags, R2WinBUGS, R2OpenBUGS, MCMCpack, rstan
# ci: desired credible interval, default: 0.95
# digits: desired number of digits in the table, default: 2


ci.number.tax = .95 # modify this parameter to get desired credible intervals.

mcmctab <- function(sims, ci = ci.number.tax, digits = 2){
  
  require(coda) 
  
  if(class(sims) == "jags" | class(sims) == "rjags"){
    sims <- as.matrix(as.mcmc(sims))
  }
  if(class(sims) == "bugs"){
    sims <- sims$sims.matrix
  }  
  if(class(sims) == "mcmc"){
    sims <- as.matrix(sims)
  }    
  if(class(sims) == "mcmc.list"){
    sims <- as.matrix(sims)
  }      
  if(class(sims) == "stanfit"){
    stan_sims <- rstan::As.mcmc.list(sims)
    sims <- as.matrix(stan_sims)
  }      
  
  
  dat <- t(sims)
  
  mcmctab <- apply(dat, 1,
                   function(x) c(Mean = round(mean(x), digits = digits), # Posterior mean
                                 SD = round(sd(x), digits = 3), # Posterior SD
                                 Lower = as.numeric(
                                   round(quantile(x, probs = c((1 - ci) / 2)), 
                                         digits = digits)), # Lower CI of posterior
                                 Upper = as.numeric(
                                   round(quantile(x, probs = c((1 + ci) / 2)), 
                                         digits = digits)), # Upper CI of posterior
                                 Pr. = round(
                                   ifelse(mean(x) > 0, length(x[x > 0]) / length(x),
                                          length(x[x < 0]) / length(x)), 
                                   digits = digits) # Probability of posterior >/< 0
                   ))
  return(t(mcmctab))
}



reg.results.table.tax = data.frame(mcmctab(earthquakefit.tax)[1:6,]) # Posterior distributions // Year FE excluded


reg.results.table.tax = data.frame(rbind( # re order df by name of the rowname according to what I have and define in 'var.labels.tax.'
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.incometax.d"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Magnitude"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.r.lat"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.r.long"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.p.Population"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Urban"),]
))

var.labels.tax = c("Income Tax", 
                   "Magnitude", 
                   "Latitude", 
                   "Longitude",
                   "Population", 
                   "Urban")

rownames(reg.results.table.tax) <- var.labels.tax



# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(xtable)

note.tax <- paste0(
  "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), " iterations with a burn-in period of n = ", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=",") , " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.tax*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects were omitted in the table.}\\\\", 
  "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains.tax, " chains were run. Detailed diagnostic plots available \\href{https://github.com/hbahamonde/Earthquake_Paper/raw/master/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Income_Tax_Model.pdf}{\\texttt here}.} \\\\")
## ----

## ---- income:tax:model:regression:table:run ----
print.xtable(xtable(
  reg.results.table.tax, 
  caption = "{\\bf Income Tax Adoption Model: Simulated Posterior Predictions (Poisson Regression, \\autoref{model:2})}.",
  label = "income:tax:model:regression:table:run"), 
  auto = TRUE,
  hline.after=c(-1, 0),
  add.to.row = list(pos = list(length(var.labels.tax)), 
                    command = note.tax),
  floating=TRUE,
  type="latex",
  table.placement = "H"
)
## ----


















##########################
# Model Checking
##########################


## ---- predicted:observed:plot:data ----
# Tax
eq.out.tax <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax)))

pred.eq.tax <- eq.out.tax[, grep("lambda[", colnames(eq.out.tax), fixed = T)]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(gtools)

pred.eq.tax <- pred.eq.tax[, c(mixedsort(names(pred.eq.tax)))]

median <- apply(pred.eq.tax, 2, median) # median of the column
lower <- apply(pred.eq.tax, 2, function(x) quantile(x, probs = c(0.05))) # quantile of the column
upper <- apply(pred.eq.tax, 2, function(x) quantile(x, probs = c(0.95))) # quantile of the column

eq.pred.tax <- data.frame(
  #Model = rep("Income Tax Adoption", nrow(dat)),
  id = 1:nrow(dat),
  Deaths.observed = dat$Deaths,
  median = median,
  lower = lower,
  upper = upper)

# Consolidating Both DF's
model.checking.plot.df = as.data.frame(eq.pred.tax)





if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

predicted.observed.plot = ggplot(data = model.checking.plot.df, 
                                 aes(x = Deaths.observed, y = reorder(id, Deaths.observed))) + 
  geom_point(aes(
    x = median, 
    y = reorder(id,Deaths.observed))) +
  geom_segment(aes(
    x = lower, 
    xend = upper, 
    y = reorder(id, Deaths.observed), 
    yend = reorder(id, Deaths.observed)), 
    alpha = 0.5) + 
  geom_point(shape = 21, colour = "red") + 
  ylab("Observation") + xlab("Deaths") + 
  theme_bw() +
  coord_flip() +
  theme(
    axis.text.y = element_text(size=7), 
    axis.text.x = element_text(size=5,angle = 90, hjust = 1), 
    axis.title.y = element_text(size=7), 
    axis.title.x = element_text(size=7), 
    legend.text=element_text(size=7), 
    legend.title=element_text(size=7),
    plot.title = element_text(size=7)#,
    #legend.position="bottom"
  )
## ----


## ---- predicted:observed:plot:plot ----
predicted.observed.plot
predicted.observed.plot.note <- paste(
  "{\\bf Assessing Model Fit}.",
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}: The figure assesses the goodness of fit of \\autoref{model:1} (\\autoref{income:tax:model:regression:table:run}). Since the model deals with the \\underline{count} of casualties associated with earthquakes (Y-axis), a ``good'' model should minimize the distance between the predicted count (black dots, with credible intervals), and the actual count (red dots). The figure shows that the model does a good job in predicting the actual death-toll."),
  "\n")

## ----

##########################
##########################



## ---- traplot:plot:sectoral ---- 
### traceplots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)


parms.sectoral = c(
  "b.propagrmanu",
  "b.Magnitude",
  "b.p.Population",
  "b.Urban",
  "b.r.lat",
  "b.r.long")

labels.sectoral = c("Agr/Ind [Agr]",
                    "Agr/Ind [Ind]",
                    "Agr/Ind [Mixed]",
                    "Magnitude [Agr]",
                    "Magnitude [Ind]",
                    "Magnitude [Mixed]",
                    "Population",
                    "Urban",
                    "Latitude",
                    "Longitude")

traplot(earthquakefit.sectoral, 
        parms=parms.sectoral, 
        style="plain", 
        auto.layout = T,
        main=labels.sectoral,
        plot.title = paste(n.chains.sectoral, "chains,", format(round(as.numeric(n.iter.sectoral), 0), nsmall=0, big.mark=","), "iterations and burn-in period of", format(round(as.numeric(n.burnin.sectoral), 0), nsmall=0, big.mark=","))
)
## ----




## ---- traplot:plot:tax ---- 
### traceplots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)


parms.tax = c(
  "b.incometax.d",
  "b.Magnitude",
  "b.Population",
  "b.Urban",
  "b.r.lat",
  "b.r.long")

labels.tax = c("Income Tax",
               "Magnitude",
               "Population",
               "Urban",
               "Latitude",
               "Longitude")

traplot(earthquakefit.tax, 
        parms=parms.tax, 
        style="plain", 
        auto.layout = T,
        main=labels.tax,
        plot.title = paste(n.chains.tax, "chains,", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), "iterations and burn-in period of", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=","))
)
## ----




## ---- denplot:plot:tax ---- 
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)

denplot(earthquakefit.tax, 
        parms=parms.tax, 
        style="plain", 
        ci=0.8, 
        #xlim=c(-40,50), 
        auto.layout = T,
        #collapse=T, 
        #col=2, 
        lty=1, 
        main=c(labels.tax),
        plot.title = paste(n.chains.tax, "chains,", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), "iterations, burn-in period of", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=","), "and", ci.number.tax*100, "% credible intervals")
)
## ----



## ---- denplot:plot:sectoral ---- 
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)

denplot(earthquakefit.sectoral, 
        parms=parms.sectoral, 
        style="plain", 
        ci=0.8, 
        #xlim=c(-40,50), 
        auto.layout = T,
        #collapse=T, 
        #col=2, 
        lty=1, 
        main=c(labels.sectoral),
        plot.title = paste(n.chains.sectoral, "chains,", format(round(as.numeric(n.iter.sectoral), 0), nsmall=0, big.mark=","), "iterations, burn-in period of", format(round(as.numeric(n.burnin.sectoral), 0), nsmall=0, big.mark=","), "and", ci.number.sectoral*100, "% credible intervals")
)
## ----






## ---- posterior:predictions:plot ---- 
# cater plot 
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(mcmcplots) 

#dev.off();dev.off() 

par(mar=c(5,10,1,1)) # bottom, then left margin, upper and right margins
caterplot(earthquakefit, 
          parms = c("b.propagrmanu", "b.incometax.d", "b.Magnitude", "b.Urban"), 
          collapse = T, 
          quantiles = list(outer=c(0.025,0.975),inner=c(0.025,0.975)),
          reorder = F, 
          cex.labels =0.9,
          labels = var.labels, # calls variables from above
          col = 2, 
          style=c("gray")
);abline(v = 0, col = "gray60")
## ----





## ---- year:fixed:effects:plot:data ----
#### PLOT fixed effects year
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(gtools,dplyr,reshape2,ggplot2)


# Tax
earthquake.out.tax <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax)))

earthquake.year.tax <- earthquake.out.tax[, grep("b.year[", colnames(earthquake.out.tax), fixed=T)]

earthquake.year.tax <- earthquake.year.tax[, c(mixedsort(names(earthquake.year.tax)))]
colnames(earthquake.year.tax) <- paste("Y",unique(sort(dat$year)), sep = "")


earthquake.year.tax <- summarise(group_by(melt(earthquake.year.tax), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))

earthquake.year.tax$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.year.tax$variable))
earthquake.year.tax$Model <- "Income Tax Adoption"


# plot
year.fixed.effects.plot = ggplot(data = earthquake.year.tax, aes(x = variable, y = mean)) + 
  geom_hline(yintercept = 0, col = "blue") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) + 
  xlab("Year") + 
  ylab("Death-Toll") + 
  theme_bw() + 
  stat_smooth(method="loess", level=0.80) +
  theme(
    axis.text.y = element_text(size=7), 
    axis.text.x = element_text(size=7), 
    axis.title.y = element_text(size=7), 
    axis.title.x = element_text(size=7), 
    legend.text=element_text(size=7), 
    legend.title=element_text(size=7),
    plot.title = element_text(size=7),
    legend.position="bottom")
## ----


## ---- year:fixed:effects:plot:plot ----
year.fixed.effects.plot 
year.fixed.effects.plot.note <- paste(
  "{\\bf Year Fixed Effects}.",
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}: Figure shows the estimated posteriors of the year fixed effects (as per \\autoref{income:tax:model:regression:table:run}). Formally, it shows all $\\protect\\beta_{6}$'s from \\autoref{model:1}. Substantively, the figure suggests that, overall, there are no influential years driving the results."),
  "\n")
## ----



# ---- incometax:chile ----
# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,grid,gridExtra)


# Load data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") 

# par(mar=c(3,3,3,3)) # bottom, then left margin, upper and right margins

p1 = ggplot() + 
  geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "dashed") + # Income Tax Law  
  theme_bw() + 
  labs(title="") +
  theme(
    axis.text.y = element_text(size=7), 
    axis.text.x = element_text(size=7), 
    axis.title.y = element_text(size=7), 
    axis.title.x = element_text(size=7), 
    legend.text=element_text(size=7), 
    legend.title=element_text(size=7),
    plot.title = element_text(size=7),
    legend.position="bottom")


#### Proportion Plot
# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

# par(mar=c(3,3,3,3)) # bottom, then left margin, upper and right margins

p2 = ggplot() + 
  geom_line(data=dat, aes(x=year, y=propagrmanu, colour="Agr/Ind Proportion"), fill=NA, size=1) +
  xlab("Year") +
  ylab("Agr/Ind Proportion") +
  labs(colour = "") +
  scale_y_continuous(breaks= seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "dashed") + # Income Tax Law  
  theme_bw() +
  labs(title="") +
  theme(
    axis.text.y = element_text(size=7), 
    axis.text.x = element_text(size=7), 
    axis.title.y = element_text(size=7), 
    axis.title.x = element_text(size=7), 
    legend.text=element_text(size=7), 
    legend.title=element_text(size=7),
    plot.title = element_text(size=7),
    legend.position="bottom")

grid.arrange(p1, p2, ncol = 1)
# ----


########################################
# Robustness Checks: Rolling Regression
########################################

## ---- income:tax:model:and:data:not:run:rolling ----
# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags)

# lower tolerance
options(scipen=10000)
set.seed(602)




# define the vectors of the data matrix for JAGS.
Deaths <- as.vector(dat$Deaths)
Magnitude <- as.vector(dat$Magnitude)
incometax.d = as.vector(as.numeric(ifelse(dat$year>=1924,1,0)))
p.Population <- as.vector(dat$p.Population)
Urban = as.vector(as.numeric(dat$Urban))
r.long = as.vector(as.numeric(dat$r.long))
r.lat = as.vector(as.numeric(dat$r.lat))
N <-  as.numeric(nrow(dat))


# specify the model
model <- function() {
  for (i in 1:N){ # number of earthquakes
    Deaths[i] ~ dpois(lambda[i])
    
    log(lambda[i]) <- 
      b.Magnitude*Magnitude[i] +
      b.incometax.d*incometax.d[i] +
      b.p.Population*p.Population[i] +
      b.Urban*Urban[i] +
      b.r.long*r.long[i] +
      b.r.lat*r.lat[i] + 
      mu ## intercept
  }
  
  b.r.lat ~ dnorm(0,1)
  b.r.long ~ dnorm(0,1)
  mu  ~ dnorm(0,1) ## intercept
  b.p.Population ~ dnorm(0,1)
  b.Urban ~ dnorm(0,1)
  b.incometax.d ~ dnorm(0,1)
  b.Magnitude ~ dnorm(0,1)
  
  
}




# list elements
data.list <- list() # create empty list to fill in next line


# fill list with one data set for each step, with one row excluded per step
for(i in 1:N){
  data.list[[i]] <- list(
    N = N-1, 
    Deaths = Deaths[-i],
    Magnitude = Magnitude[-i], 
    incometax.d = incometax.d[-i],
    p.Population = p.Population[-i],
    Urban = Urban[-i],
    r.long = r.long[-i],
    r.lat = r.lat[-i]
  )
}



# Starting value for reproducibility
model.inits <- function(){
  list("b.incometax.d" = 0)
}

# run model
model.fit <- list() # again, create empty list first

for(i in 1:N){  # use loop here to fit one model per data set
  model.fit[[i]] <- jags(
    data=data.list[[i]],
    inits=NULL,
    parameters.to.save = c("b.incometax.d"),
    n.chains = n.chains.tax,  # 4
    n.iter = n.iter.tax, # 200000
    n.burnin = n.burnin.tax, # 5000
    model.file=model,
    progress.bar = "none")
}


# helper function for output
ci.rolling = .95 # modify this parameter to get desired credible intervals.

mcmctab <- function(sims, ci = ci.rolling, digits = 2){
  
  require(coda) 
  
  if(class(sims) == "jags" | class(sims) == "rjags"){
    sims <- as.matrix(as.mcmc(sims))
  }
  if(class(sims) == "bugs"){
    sims <- sims$sims.matrix
  }  
  if(class(sims) == "mcmc"){
    sims <- as.matrix(sims)
  }    
  if(class(sims) == "mcmc.list"){
    sims <- as.matrix(sims)
  }      
  if(class(sims) == "stanfit"){
    stan_sims <- rstan::As.mcmc.list(sims)
    sims <- as.matrix(stan_sims)
  }      
  
  
  dat <- t(sims)
  
  mcmctab <- apply(dat, 1,
                   function(x) c(Mean = round(mean(x), digits = digits), # Posterior mean
                                 SD = round(sd(x), digits = 3), # Posterior SD
                                 Lower = as.numeric(
                                   round(quantile(x, probs = c((1 - ci) / 2)), 
                                         digits = digits)), # Lower CI of posterior
                                 Upper = as.numeric(
                                   round(quantile(x, probs = c((1 + ci) / 2)), 
                                         digits = digits)), # Upper CI of posterior
                                 Pr. = round(
                                   ifelse(mean(x) > 0, length(x[x > 0]) / length(x),
                                          length(x[x < 0]) / length(x)), 
                                   digits = digits) # Probability of posterior >/< 0
                   ))
  return(t(mcmctab))
}

# create empty data frame to be filled with estimation results per data set
tab <- data.frame(index = c(1:N), IncomeTax = rep(NA, N), lower = rep(NA, N), upper = rep(NA, N))

# save(tab, file = "//Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/rolling_poison.RData")

# fill with estimates, using mcmctab to extract mean & lower & upper CIs
for(i in 1:N){
  tab[i, 2] <- mcmctab(model.fit[[i]])[1, 1]
  tab[i, 3] <- mcmctab(model.fit[[i]])[1, 3]
  tab[i, 4] <- mcmctab(model.fit[[i]])[1, 4]
}
# plot results
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


income.tax.model.rolling.plot = ggplot(data = tab, aes(x = IncomeTax, y = index)) + 
  geom_point() + 
  geom_segment(aes(x = lower, xend = upper, yend = index)) + 
  geom_vline(xintercept = 0, linetype=2, colour="red") + 
  xlab("Estimated Coefficient of Implementing the Income Tax on Death Tolls") + ylab("Observation being Excluded") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        legend.position="bottom")
## ----




## ---- income:tax:model:and:data:run:rolling ----
income.tax.model.rolling.plot
income.tax.model.rolling.note <- paste(
  "{\\bf Rolling Bayesian Poisson Regression}.",
  "\\\\\\hspace{\\textwidth}", 
  paste(paste("{\\bf Note}: Figure shows the estimates of implementing the income tax on death-tolls of", nrow(tab), sep = " "), "models which correspond to fully estimating \\autoref{model:2}, but excluding one observation at a time.", paste(paste(ci.rolling*100, "\\%", sep=""), "credible intervals were included.", sep=" "), "The figure  suggest that the negative results of income taxation and death-tolls are not driven by wealthy municipalities, but to the capacity of the state of enforcing building codes.", sep = " "),
  "\n")
## ----



########################################
# Cox Models
########################################




# ---- incometax:data ----
# Load data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra)



# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
  require(ggplot2)
  require(gridExtra)
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### plots

##### Chile
chile.p = ggplot() + 
  geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law  
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Chile") +
  ggsave("Chile_Income_Tax.pdf", width = 40, height = 20, units = "cm")



##### Peru
peru.p = ggplot() + 
  geom_line(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Peru"), aes(xintercept = 1934, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Peru") +
  ggsave("Peru_Income_Tax.pdf", width = 40, height = 20, units = "cm")



##### Colombia
colombia.p = ggplot() + 
  geom_line(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Colombia"), aes(xintercept = 1935, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Colombia") +
  ggsave("Colombia_Income_Tax.pdf", width = 40, height = 20, units = "cm")



##### Ecuador
ecuador.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Ecuador"), aes(xintercept = 1945, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Ecuador") +
  ggsave("Ecuador_Income_Tax.pdf", width = 40, height = 20, units = "cm")


##### Venezuela
venezuela.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Venezuela"), aes(xintercept = 1943, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Venezuela") +
  ggsave("Venezuela_Income_Tax.pdf", width = 40, height = 20, units = "cm")


##### Nicaragua
nicaragua.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Legend") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Nicaragua"), aes(xintercept = 1974, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Nicaragua") +
  ggsave("Nicaragua_Income_Tax.pdf", width = 40, height = 20, units = "cm")


##### Guatemala
guatemala.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Income Tax (ln)") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Guatemala"), aes(xintercept = 1963, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Guatemala")  +
  ggsave("Guatemala_Income_Tax.pdf", width = 40, height = 20, units = "cm")

##### Argentina
argentina.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Argentina"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Argentina"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Income Tax (ln)") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Argentina"), aes(xintercept = 1933, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Argentina") +
  ggsave("Argentina_Income_Tax.pdf", width = 40, height = 20, units = "cm")

##### Mexico
mexico.p= ggplot() + 
  geom_line(data=subset(dissertation, country=="Mexico"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
  geom_line(data=subset(dissertation, country=="Mexico"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
  xlab("Year") +
  ylab("GDP Output (ln)") +
  labs(colour = "Income Tax (ln)") +
  scale_x_continuous(limits=c(1890,2010)) + 
  geom_vline(data=subset(dissertation, country=="Mexico"), aes(xintercept = 1925, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
  theme_bw() + 
  theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
  labs(title="Mexico")  +
  ggsave("Mexico_Income_Tax.pdf", width = 40, height = 20, units = "cm")
# ----



# ---- incometax:plot ----

##### All
grid_arrange_shared_legend(
  chile.p, 
  ecuador.p, 
  nicaragua.p, 
  venezuela.p, 
  peru.p, 
  colombia.p, 
  guatemala.p, 
  argentina.p, 
  mexico.p,
  ncol = 3, nrow = 3)

outputstitle <- paste(
  "{\\bf Industrial and Agricultural Outputs, and The Passage of the Income Tax Law}.",
  "\\\\\\hspace{\\textwidth}", 
  "{\\bf Note}: Figure shows historical sectoral outputs, and year of the passage of the income tax law. Following convention, the figure shows logged values.",
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Source}: \\href{http://moxlad-staging.herokuapp.com/home/en?}{MOxLAD}, and other sources compiled by the author (see \\autoref{sample:data:income:tax:tab})."),
  "\n")
# ----


##################################################
##              MODELS 1
## [results:1]
##################################################
rm(list=ls())
cat("\014")


##################################################
### THIS CHUNK GOES BEFORE THE MODELS





## ---- texreg-extractor-geeglm ----
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(texreg, methods)

extract.geepack <- function(model) {
  s <- summary(model)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  n <- nrow(model.frame(model))
  nclust <- length(s$geese$clusz)
  
  gof = c(n, nclust)
  gof.names = c("Num. obs.", "Num. clust.")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = rep(FALSE, length(gof))
  )
  return(tr)
}

setMethod("extract", signature = className("geeglm", "geepack"),
          definition = extract.geepack)
## ---- 



##################################################






## ---- results:data ----
# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE
#load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/l_clogit.RData") # Lagged CONSTANT AGR MANUFACT for clogit  (fixed effects)



if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)


# this is the model I use for simulation
cox2 <- coxph(Surv(year, year2, incometax.s) ~ 
                L_constmanufact +
                L_constagricult +
                totpop +
                cluster(country),
              data = cox)

## logit GEE
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(geepack)

logitgee.1 = geeglm(incometax.d ~ log(constmanufact) + log(constagricult) + log(totpop), 
                    family = binomial, 
                    id = country, 
                    corstr = "independence",
                    std.err = "san.se",
                    data = logitgee)
logitgee.1 = extract(logitgee.1)


# conditional logit
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)

clogit.1 = clogit(
  incometax.d ~  
    log(constmanufact) + 
    log(constagricult) + 
    log(totpop) +
    strata(country), 
  method= "efron", data = data)
## ----



## ---- results:table:cox ----
# screenreg / texreg
texreg(
  list(cox2, logitgee.1, clogit.1), # it needs to be texreg for knitr
  caption = "{\\bf Sectoral Origins of Income Taxation: Income Tax Law and Industrial Development}.",
  custom.coef.names = c(
    "Manufacture Output$_{t-1}$",
    "Agricultural Output$_{t-1}$",
    "Total Population",
    #
    # "intercept",
    #
    "Manufacture Output (ln)",
    "Agricultural Output (ln)",        
    "Total Population (ln)"),
  custom.model.names = c(
    "(1) Cox (1 lag)",# Base Model
    "(2) Logit GEE", # GEE
    "(3) Conditional Logit (FE)" # Fixed Effects model
  ),
  label = "results:table:cox",
  custom.note = "%stars. Robust standard errors in all models. Intercept omitted.",
  fontsize = "small",
  center = TRUE,
  use.packages = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  omit.coef = "(Intercept)",
  #longtable = TRUE,
  digits = 3,
  table = TRUE,
  stars = c(0.001, 0.01, 0.05, 0.1),
  #sideways = TRUE,
  no.margin = TRUE, 
  float.pos = "!htbp"
)
## ---- 



########################################################
#### Simulation: Relative Hazard
#### [simulation:1] [simulation:2]
########################################################







## ---- simulation ----
########################  

## industrial sector
# simulate qi's

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(simPH)

# quantities
# nsim = 10000 # original: 10000
# qi = "Hazard Rate" # original: Hazard Rate
# ci = 0.95


set.seed(602)
sim.m.ind <- coxsimLinear(cox2, 
                          b = "L_constmanufact", 
                          qi = qi, 
                          ci = ci,
                          #spin = T,
                          extremesDrop = T,
                          nsim = nsim,
                          Xj = c(min(cox$L_constmanufact), max(cox$L_constmanufact))
)


### agricultural sector
# simulate qi's
set.seed(602)
sim.m.agr <- coxsimLinear(cox2, 
                          b = "L_constagricult", 
                          qi = qi, 
                          ci = ci,
                          #spin = T,
                          extremesDrop = T,
                          nsim = nsim,
                          Xj = c(min(cox$L_constagricult), max(cox$L_constagricult))
)


simtitle <- paste(
  paste0("{\\bf ",qi, " of Implementing the Income Tax Law}."),
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}:", "Using estimations of Model 1 in \\autoref{results:table:cox} (\\autoref{cox:eq}), figure shows", formatC(nsim, format="d", big.mark=","), "simulations with different sectoral growth speeds. ``Slow'' is the minimum value, while ``rapid'' is the maximum value for each sectoral output."),
  paste(paste("The figure also shows the ", paste(ci*100, "\\%", sep = ""), sep = ""), "confidence intervals."), 
  "\n")
## ----






## ---- simulation:plots ----
# install.packages("devtools")
# library(devtools)
# devtools::install_github('christophergandrud/simPH')
# library(simPH)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,simPH)


## IMPORTANT! A relative hazard for a unit at zero is always one, as it is a ratio of the hazards with itself. Gandrud2015 p. 10

# Plot 1
options(scipen=10000)
sim.p.ind = simGG(sim.m.ind, type = 'lines',# type = 'points' // 'lines'
                  xlab = "Year", 
                  ylab = "Hazard Rate", 
                  ribbons = F, alpha = 0.25) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8),
        title = element_text(size=9)
  ) +
  labs(title = "Industrial Output") +
  scale_color_manual(labels = c("Slow", "Rapid"), values = c("red", "blue")) +
  guides(color=guide_legend("Sectoral Output"))


# Plot 2
options(scipen=10000)
sim.p.agr = simGG(sim.m.agr, type = 'lines',# type = 'points' // 'lines'
                  xlab = "Year", 
                  ylab = "Hazard Rate", 
                  ribbons = F, alpha = 0.25) + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8),
        title = element_text(size=9)
  ) +
  labs(title = "Agriculture Output") +
  scale_color_manual(labels = c("Slow", "Rapid"), values = c("red", "blue")) +
  guides(color=guide_legend("Sectoral Output"))


# combine both plots
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggpubr)
ggarrange(sim.p.ind, sim.p.agr, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
## ----








## http://rstudio-pubs-static.s3.amazonaws.com/12451_53fc5e6bd80744b99158a12975c31cbf.html
## overdispersion
### epsilon (ϵ) represents overdispersion
### tau (τ) is a measure of the amount of overdispersion
### mu (μ) represents the intercept


#### Predicted Probabilities.
#### FROM: https://github.com/jkarreth/Bayes/blob/master/logit.pp.plot.instructions.R // line:52
