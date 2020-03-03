cat("\014")
rm(list=ls())
graphics.off()


## ---- number:of:simulations ----
# Hazard Rate simulations 
nsim = 10000 # original: 10000
qi = "Hazard Rate" # original: Hazard Rate
ci = 0.95

# Bayesian: Sectoral Model
n.iter.sectoral = 300000  # n.iter.sectoral = 300000 // this is for working model
n.burnin.sectoral = 30000 # n.burnin.sectoral = 30000 // this is for working model
n.chains.sectoral = 5 # n.chains.sectoral = 5 for the working model

# Bayesian: Tax Model
n.iter.tax = 300000  # n.iter.tax = 300000 // this is for working model
n.burnin.tax = 30000 # n.burnin.tax = 30000 // this is for working model
n.chains.tax = 5 # n.chains.tax = 5 for the working model


chain.chains.word = ifelse(n.chains.tax==1 ,"chain","chains")  # introduces right word into the text
iteration.iterations.word = ifelse(n.iter.tax==1 ,"iteration","iterations")  # introduces right word into the text

# function to convert numbers to word (for paper)
#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
        ## Function by John Fox found here: 
        ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
        ## Tweaks by AJH to add commas and "and"
        helper <- function(x){
                
                digits <- rev(strsplit(as.character(x), "")[[1]])
                nDigits <- length(digits)
                if (nDigits == 1) as.vector(ones[digits])
                else if (nDigits == 2)
                        if (x <= 19) as.vector(teens[digits[1]])
                else trim(paste(tens[digits[2]],
                                Recall(as.numeric(digits[1]))))
                else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                                  Recall(makeNumber(digits[2:1]))))
                else {
                        nSuffix <- ((nDigits + 2) %/% 3) - 1
                        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
                        trim(paste(Recall(makeNumber(digits[
                                nDigits:(3*nSuffix + 1)])),
                                suffixes[nSuffix],"," ,
                                Recall(makeNumber(digits[(3*nSuffix):1]))))
                }
        }
        trim <- function(text){
                #Tidy leading/trailing whitespace, space before comma
                text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
                #Clear any trailing " and"
                text=gsub(" and$","",text)
                #Clear any trailing comma
                gsub("\ *,$","",text)
        }  
        makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
        #Disable scientific notation
        opts <- options(scipen=100) 
        on.exit(options(opts)) 
        ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine") 
        names(ones) <- 0:9 
        teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                   "sixteen", " seventeen", "eighteen", "nineteen")
        names(teens) <- 0:9 
        tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                  "ninety") 
        names(tens) <- 2:9 
        x <- round(x)
        suffixes <- c("thousand", "million", "billion", "trillion")     
        if (length(x) > 1) return(trim(sapply(x, helper)))
        helper(x)
}
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
save(dat.chile, file = "/Users/hectorbahamonde/RU/research/Earthquake_Chile_Peru/Chile_Data_Earthquake.RData")



############################
#### Loadings: Peru
############################
dat.peru <- read.csv("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Data/Peru_Data_Earthquake.csv")


dat.peru$Magnitude = as.numeric(as.character(dat.peru$Magnitude))
dat.peru$Deaths = as.numeric(as.character(dat.peru$Deaths))
dat.peru$W.Deaths = (dat.peru$Deaths/dat.peru$Population)*100


# save dataset
save(dat.peru, file = "/Users/hectorbahamonde/RU/research/Earthquake_Chile_Peru/Peru_Data_Earthquake.RData")



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
save(eq.output.d, file = "/Users/hectorbahamonde/RU/research/Earthquake_Chile_Peru/eq_output_d.RData")




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


## leaves out everything that's not included
dat.chile.complete <- dat.chile.complete[which(dat.chile.complete$Included == "Yes"), ] # drop early earthquakes 

## dummy for pre/post implementation of the income tax
dat.chile.complete$IncomeTax = factor(
    ifelse(
      dat.chile.complete$year >= 1924,
      1,0),
  levels = c(0,1),
  labels = c("No", "Yes"))

# time-series plot
time.series.plot = ggplot(dat.chile.complete, aes(x = year, y = Magnitude)) +
  geom_point(shape = 21, aes(fill = dat.chile.complete$IncomeTax)) +
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
  guides(fill=guide_legend(title="Income Tax"))+
stat_smooth(show.legend = F,  method = 'loess')
## ----



## ---- earthquake:ts:plot:chile:plot ----
# dropping NAs 
time.series.plot
time.series.plot.note <- paste(
  paste(paste("{\\bf Earthquakes in Chile: ", paste(min(dat.chile.complete$year),max(dat.chile.complete$year), sep = "-"), ".}", sep = ""),
        "\\\\\\hspace{\\textwidth}", 
        paste(paste("{\\bf Note}: Figure shows earthquakes over time (N=", nrow(dat.chile.complete),").", sep = ""),
              " Additionally, the figure shows earthquakes before and after the implementation of the income tax in 1924. A smoothing function was added to show that there are not statistically significant different decreases/increases in magnitudes over time.", 
              sep = "")),
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

# shrink df to northern part of Chile.
chile.map.plot.d.t = chile.map.plot.d[with(chile.map.plot.d, which(Latitude>=-30 & chile.map.plot.d$Longitude>=-80, arr.ind=TRUE)), ]
chile.provinces.t = chile.provinces[with(chile.provinces, which(lat>=-30 & long>=-80, arr.ind=TRUE)), ]
chile.map.plot.d.t$Magnitude = as.factor(round(chile.map.plot.d.t$Magnitude,0))


# plot
chile.plot = ggplot() +  
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=chile.provinces.t, alpha=1/2) +
  theme_bw() +
  ggtitle("Chile") + 
  geom_point(data=chile.map.plot.d.t, aes(x=Longitude, y=Latitude, colour=Magnitude)) +#shape=21, 
  scale_y_continuous(name='Latitude') +
  scale_x_continuous(name='Longitude') +
        scale_fill_discrete(name="Rounded\nMagnitude") +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=7),
        legend.position="bottom")
## ----





## ---- earthquake:map:plot:chile ----
# plot
earthquake.map.plot.chile
earthquake.map.note.chile <- paste(
  paste("{\\bf Data Used in the Analyses: Geographical Distribution of Earthquakes in Chile,", paste(paste(min(chile.map.plot.d$Year), max(chile.map.plot.d$Year), sep="-"), "}.", sep = "" ), sep=" "),
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}:", paste(paste(paste("The figure shows a total of", nrow(chile.map.plot.d), ""), "earthquakes", sep = ""), "using a combination of archival information and external sources. Each quake was colorized according to the predominant economic sector at the municipal level. In total, there were", as.numeric(table(chile.map.plot.d$Sector)["Agriculture"]), "earthquakes that took place in agricultural localities,", as.numeric(table(chile.map.plot.d$Sector)["Industry"]), "in industrial, and", as.numeric(table(chile.map.plot.d$Sector)["Mixed"]), "in mixed municipalities.",   sep = " "), sep=" "),
  "\n")
## ----



#### MAPS: Peru

## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rgdal, foreign, rgeos, ggplot2)

# load eq data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Peru_Data_Earthquake.RData")

# load shape file
peru.provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Data/shape_files/peru_provinces/Limite_provincial", layer = "BAS_LIM_PROVINCIA")


#peru.provinces <- gSimplify(peru.provinces, tol=10000, topologyPreserve=T)
peru.provinces <- spTransform(peru.provinces, CRS("+proj=longlat +datum=WGS84"))
peru.provinces <- fortify(peru.provinces)

dat.peru = dat.peru[complete.cases(dat.peru), ] ## excludes NAs from dat.peru.t
dat.peru.t = dat.peru[with(dat.peru, which(Latitude <=-10, arr.ind=TRUE)), ] ## only geographical portion we need
peru.provinces.t = peru.provinces[with(peru.provinces, which(lat <=-10, arr.ind=TRUE)), ] ## only geographical portion we need
dat.peru.t$Magnitude = as.factor(round(dat.peru.t$Magnitude,0))

# here
peru.plot = ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=peru.provinces.t, alpha=1/2) +
  theme_bw() +
  ggtitle("Peru") + 
  geom_point(data=subset(dat.peru.t, year>=1900), aes(x=Longitude, y=Latitude, colour=Magnitude)) +
        scale_y_continuous(name='Latitude') +
        scale_x_continuous(name='Longitude') +
        #scale_fill_discrete(name="Rounded\nMagnitude") +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=12), 
              legend.title=element_text(size=12),
              plot.title = element_text(size=7),
              legend.position="bottom")


### plot both countries
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(gridExtra,cowplot)

plot_grid(peru.plot, chile.plot, ncol = 2, nrow = 1, align = "h", rel_heights = c(1/2, 1/2))

#save
chile_peru_map <- arrangeGrob(peru.plot, chile.plot, ncol = 2, nrow = 1) #generates plot
ggsave(file="/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/chile_peru_map2.pdf", 
       width = 11, height = 8.5, units = "cm", chile_peru_map) #saves chile_peru_map




################################################################
######## USING ANOTHER LIBRARY
################################################################

if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(rgeos,cowplot,googleway,ggplot2,ggrepel,ggspatial,rnaturalearth,rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf")

# grafico basico del mundo
ggplot(data = world) + geom_sf() + theme_bw()


# tratemos de plotear el Sudamerica...para eso, cambiamos las coordenadas.
ggplot(data = world) + geom_sf() + coord_sf(xlim = c(-30, -80), ylim = c(-30, 15)) 






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
  "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", format(round(as.numeric(n.iter.sectoral), 0), nsmall=0, big.mark=","), " iterations with a burn-in period of n = ", format(round(as.numeric(n.burnin.sectoral), 0), nsmall=0, big.mark=","), " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.sectoral*100 ,"\\% credible intervals (lower/upper bounds). All R-Hat statistics are below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects were omitted in the table.}\\\\", 
  "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains.sectoral, " chains were run.} \\\\")
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
# Income Tax Model
###############################

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
options(scipen=100000)
set.seed(602)

# specify the model
model.jags.tax <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.Magnitude * Magnitude[i] +
                        b.incometax.d * incometax.d[i] +
                        b.interaction * Magnitude[i] * incometax.d[i] +
                        b.p.Population * p.Population[i] +
                        b.r.long * r.long[i] +
                        b.r.lat * r.lat[i] + 
                        b.Sector[SectorID[i]] +
                        #b.year[yearID[i]] + # year fixed-effects 
                        mu ## intercept
        }
        
        b.Magnitude ~ dnorm(0,0.0001)
        b.incometax.d ~ dnorm(0,0.0001)
        b.interaction ~ dnorm(0,0.0001)
        b.p.Population ~ dnorm(0,0.0001)
        b.r.long ~ dnorm(0,0.0001)
        b.r.lat ~ dnorm(0,0.0001)

        mu  ~ dnorm(0,0.0001) ## intercept
        
        for (t in 1:NSector){ # fixed effects
                b.Sector[t] ~ dnorm(m.b.Sector[t], tau.b.Sector[t]) 
                
                m.b.Sector[t] ~ dnorm(0,0.0001)
                tau.b.Sector[t] ~ dgamma(1, 1)
        }
        
        #for (t in 1:yearN){ # fixed effects 
        #        b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t]) 
        #        
        #        m.b.year[t] ~ dnorm(0,0.0001)
        #        tau.b.year[t] ~ dgamma(1, 1)
                
        #}
                

      }

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
SectorID = as.factor(as.ordered(dat$Sector))
NIncometax = as.vector(length(unique(as.numeric(dat$incometax.d))))
incometax.d = as.vector(as.numeric(ifelse(dat$year>=1924,1,0)))
NIncometax.y = as.vector(length(unique(as.numeric(dat$incometax.y))))
incometax.y = as.vector(as.numeric(dat$incometax.y))
Urban = as.vector(as.numeric(dat$Urban))
customtax = as.vector(as.numeric(dat$customtax))/100
r.long = as.vector(as.numeric(dat$r.long))
r.lat = as.vector(as.numeric(dat$r.lat))





jags.data.tax <- list(Deaths = Deaths,
                      Magnitude = Magnitude,
                      incometax.d = incometax.d,
                      p.Population = p.Population,
                      r.long = r.long,
                      r.lat = r.lat,
                      NSector = NSector,
                      SectorID = SectorID,
                      #yearID = yearID,
                      #yearN = yearN,
                      N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.tax <- c(
  "b.Magnitude", 
  "b.incometax.d",
  "b.interaction",
  "b.p.Population", 
  "b.r.long", 
  "b.r.lat", 
  "b.Sector",
  #"b.year",
  "lambda"
  )
## ----


## ---- income:tax:model:and:data:run ----
# run the model
# n.iter.tax = 800000  # n.iter.tax = 200000 // this is for working model
# n.burnin.tax = 80000 # n.burnin.tax = 5000 // this is for working model
# n.chains.tax = 5 # n.chains.tax = 4 for the working model

earthquakefit.tax <- jags(
  data=jags.data.tax,
  inits=NULL,
  parameters.to.save = eq.params.tax,
  n.chains = n.chains.tax,
  n.iter = n.iter.tax,
  n.burnin = n.burnin.tax, 
  #n.thin = 10,
  model.file=model.jags.tax,
  progress.bar = "none"
  )


# plot(earthquakefit.tax)

#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc.tax <- as.mcmc(earthquakefit.tax)
bayes.mod.fit.gg.tax <- ggs(fit.mcmc.tax)
ggmcmc(bayes.mod.fit.gg.tax, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Income_Tax_Model.pdf")
graphics.off()
## ----


###############################
# Income Tax Plot
###############################

## ---- income:tax:model:plot:not:run ----
tax.mcmc <- as.mcmc(earthquakefit.tax)
tax.mcmc.mat <- as.matrix(tax.mcmc)
tax.mcmc.dat <- as.data.frame(tax.mcmc.mat)

# Simulate the range of the moderating variable
x2.sim <- seq(min(jags.data.tax$incometax.d), max(jags.data.tax$incometax.d), by = 1)

## Calculate conditional effect of X1 across the range of X2
int.sim <- matrix(rep(NA, nrow(tax.mcmc.dat)*length(x2.sim)), nrow = nrow(tax.mcmc.dat))
#int.sim = data.frame(int.sim); colnames(int.sim) <- c("No Income Tax","Income Tax")


for(i in 1:length(x2.sim)){
        int.sim[, i] <- tax.mcmc.dat$b.Magnitude + tax.mcmc.dat$b.interaction * x2.sim[i]
}


int.sim = data.frame(
        x = c(int.sim[,1],int.sim[,2]),
        'Income Tax' = c(rep("No", nrow(int.sim)),rep("Yes", nrow(int.sim)))
)

# calculate/round QIs
death.toll.before.tax = round(mean(int.sim$x[int.sim$Income.Tax=="No"]),0)
death.toll.after.tax  = round(mean(int.sim$x[int.sim$Income.Tax=="Yes"]),0)

# plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

income.tax.model.plot = ggplot(int.sim, aes(x=x, fill= Income.Tax, y=..scaled..)) + 
  geom_density(alpha=.3) + 
  xlab("Death-Toll (posterior)") + ylab("Density") + 
  theme_bw() + 
  guides(fill=guide_legend(title="Income Tax")) + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        legend.position="bottom") #+ 
#ggtitle("Conditional Effect of Earthquake Magnitudes on Implementing the Income Tax")
## ----

## ---- income:tax:model:plot:run ----
income.tax.model.plot
income.tax.model.plot.note <- paste(
  "{\\bf Conditional Effects of Earthquake Magnitudes on Implementing the Income Tax Over Time in Chile}.",
  "\\\\\\hspace{\\textwidth}",
  paste("{\\bf Note}: Using the estimations from \\autoref{income:tax:model:regression:table:run} (\\autoref{model:2}), and following the advice of \\textcite{Brambor2006}, the figure shows the conditional effect of earthquake magnitudes on implementing the income tax in Chile in 1924 ($\\beta_{1}+\\beta_{3}\\times\\text{Income Tax}_{i}$). Particularly, by implementing the income tax, the baseline propensity of the earthquake's magnitude of increasing the death toll \\emph{decreases} from an estimated overtime average of", numbers2words(death.toll.before.tax), "to an estimated overtime average of", numbers2words(death.toll.after.tax), "\\unskip. Hence, the figure suggests that implementing the income tax law had positive effects on state capacity over time.", "Both distributions were computed via a MCMC routine, particularly the iteration of", numbers2words(n.chains.tax), "chains", "with", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), "iterations per chain. Considering the Monte Carlo Markov Chain properties, the first", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=","), "observations of every chain were discarded.")
  )
## ----




###############################
# Income Tax Table
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



reg.results.table.tax = data.frame(mcmctab(earthquakefit.tax)[1:9,]) # Posterior distributions


reg.results.table.tax = data.frame(rbind( # re order df by name of the rowname according to what I have and define in 'var.labels.tax.'
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.incometax.d"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Magnitude"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.interaction"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.r.lat"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.r.long"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.p.Population"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Sector[1]"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Sector[2]"),],
  reg.results.table.tax[rownames(reg.results.table.tax)==("b.Sector[3]"),]
))

var.labels.tax = c("Income Tax", 
                   "Magnitude", 
                   "Income Tax * Magnitude",
                   "Latitude", 
                   "Longitude",
                   "Population", 
                   "Sector[Agriculture]",
                   "Sector[Industry]",
                   "Sector[Mixed]") 

rownames(reg.results.table.tax) <- var.labels.tax



# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(xtable)

note.tax <- paste0(
  "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), " iterations with a burn-in period of n = ", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=",") , " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.tax*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics are below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", numbers2words(n.chains.tax), " chains were run.} \\\\")
## ----

## ---- income:tax:model:regression:table:run ----
print.xtable(xtable(
  reg.results.table.tax, 
  caption = "{\\bf The Spillover Effects of Income Taxation in Chile: Simulated Posterior Predictions (Poisson Regression, \\autoref{model:2})}.",
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
  paste("{\\bf Note}: The figure assesses the goodness of fit of \\autoref{model:2} (\\autoref{income:tax:model:regression:table:run}). Since the model deals with the \\underline{count} of casualties associated with earthquakes (Y-axis), a ``good'' model should minimize the distance between the predicted count (black dots, with credible intervals), and the actual count (red dots). The figure shows that the model does a good job in predicting the actual death-toll."),
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


parms.tax = c("b.Magnitude", 
              "b.Sector[1]", 
              "b.Sector[2]", 
              "b.Sector[3]", 
              "b.incometax.d", 
              "b.interaction", 
              "b.p.Population", 
              "b.r.lat", 
              "b.r.long")


labels.tax = c("Magnitude",
               "Sector[Agriculture]",
               "Sector[Industry]",
               "Sector[Mixed]", 
               "Income Tax",
               "Magnitude * Income Tax",
               "Population",
               "Latitude",
               "Longitude")

traplot(earthquakefit.tax, 
        parms=parms.tax, 
        style="plain", 
        auto.layout = T,
        main=labels.tax,
        plot.title = paste(numbers2words(n.chains.tax), "chains,", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), "iterations and burn-in period of", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=","))
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
        plot.title = paste(numbers2words(n.chains.tax), "chains,", format(round(as.numeric(n.iter.tax), 0), nsmall=0, big.mark=","), "iterations, burn-in period of", format(round(as.numeric(n.burnin.tax), 0), nsmall=0, big.mark=","), "and", ci.number.tax*100, "% credible intervals")
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
  "{\\bf Year Fixed-Effects}.",
  "\\\\\\hspace{\\textwidth}", 
  paste("{\\bf Note}: Figure shows the estimated posteriors of the year fixed-effects (as per \\autoref{income:tax:model:regression:table:run}). Formally, it shows all $\\protect\\beta_{8}$'s from \\autoref{model:2}. Substantively, the figure suggests that, overall, there are no influential years driving the results."),
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

















## http://rstudio-pubs-static.s3.amazonaws.com/12451_53fc5e6bd80744b99158a12975c31cbf.html
## overdispersion
### epsilon (ϵ) represents overdispersion
### tau (τ) is a measure of the amount of overdispersion
### mu (μ) represents the intercept


#### Predicted Probabilities.
#### FROM: https://github.com/jkarreth/Bayes/blob/master/logit.pp.plot.instructions.R // line:52
