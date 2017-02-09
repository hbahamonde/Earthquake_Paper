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


###### MERGING ##############

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
dat = dat[!is.na(dat$constmanufact),] 
dat = dat[!is.na(dat$constagricult),] 


# rounding lattitude/longitude 
dat$r.lat = round(dat$Latitude,1) 
dat$r.long = round(dat$Longitude,1) 

# weight population 
dat$w.Deaths = round((dat$Deaths/dat$Population),5)*100 

# proportion of Population 
dat$p.Population = dat$Population/1000 

# scaling 
pvars <- c("constagricult","constmanufact","Magnitude", "p.Population") 
datsc <- dat 
datsc[pvars] <- lapply(datsc[pvars],scale) 


# formula 
fm = as.formula(Deaths ~ constmanufact + constagricult + factor(country) + factor(year) + Magnitude) 
# Frequentist model
###################################################################### 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lme4,texreg) 

# fit the model
model = glmer(
  Deaths ~ constmanufact + constagricult + Magnitude + p.Population + (1 | country), 
  data = datsc, family=poisson, 
  glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)) # increases the number of possible iterations to avoid convergence problem 
)

# Comments:
  # I am already weighting by population by including p.Population in the model.
  # Only FE by country: If year is included, it is collinerar with growth, which increases year by year.


# table
screenreg(model) 

# predictions
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(effects) 

# obtain a fit at different estimates of the predictor
ef.1=effect(c("constmanufact"),model) ; df.ef=data.frame(ef.1)
ef.2=effect(c("constagricult"),model) ; df.ef=data.frame(ef.2)

dev.off();dev.off();dev.off()
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

plot(effect(c("constmanufact"),model),grid=TRUE)
plot(effect(c("constagricult"),model),grid=TRUE)


## convergence test 
# model = glmer(Deaths ~ constmanufact + constagricult + Magnitude + p.Population + (1 | country) + (1 | year), data = datsc, family=poisson) 
# relgrad <- with(model@optinfo$derivs,solve(Hessian,gradient)) 
# max(abs(relgrad)) 
# http://stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge // not less than .0001, but it is very small (0.02397465) 


# Bayesian model
###################################################################### 



cat("\014") 
rm(list=ls()) 

# loading data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d.RData") 
dat = eq.output.d # rename dataset 
dat <- dat[which(dat$year >= 1900 & dat$country == "Chile"), ] # drop early earthquakes 

# dropping NAs 
dat = dat[!is.na(dat$Magnitude),] 
dat = dat[!is.na(dat$Deaths),] 
dat = dat[!is.na(dat$Sector),] 
dat = dat[!is.na(dat$Population),] 
dat = dat[!is.na(dat$constmanufact),] 
dat = dat[!is.na(dat$constagricult),] 


# rounding lattitude/longitude 
dat$r.lat = round(dat$Latitude,1) 
dat$r.long = round(dat$Longitude,1) 

# 
dat$country <- factor(dat$country)
dat$country <- droplevels(dat$country)
dat$country <- as.integer(dat$country)

# year as a counter variable
dat$year = 1:nrow(dat)

# weight population 
dat$w.Deaths = round((dat$Deaths/dat$Population),5)*10

# proportion of Population 
dat$p.Population = dat$Population/1000 

# scaling 
pvars <- c("constagricult","constmanufact","Magnitude", "p.Population") 
datsc <- dat 

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)






# model
options(scipen=10000)
set.seed(602)


model.jags.beta <- function() {
  for(i in 1:N) {
    w.Deaths[i] ~ dbeta(alpha[i], beta[i])
    #
    alpha[i] <- mu[i] * phi
    beta[i]  <- (1-mu[i]) * phi
    
    logit(mu[i]) <- 
      a +
      b.constmanufact[Sector[i]]*constmanufact[i] + 
      b.constagricult[Sector[i]]*constagricult[i] + 
      b.Magnitude*Magnitude[i]
  }

  #phiinv ~ dgamma(0.1,0.01)
  phi <- 1/0.08892867 
  a ~ dnorm(0,.001)
  b.Magnitude ~ dnorm(0, 0.001)

for (j in 1:NSector){ # Priors for varying coefficients
  b.constmanufact[j] ~ dnorm(0, 0.001)
  b.constagricult[j] ~ dnorm(0, 0.001)
  }
}






model.jags <- function() {
        for (i in 1:N){
                Deaths[i] ~ dpois(lambda[i])
          
          log(lambda[i]) <- 
            mu + 
            b.constmanufact[Sector[i]]*constmanufact[i] + 
            b.constagricult[Sector[i]]*constagricult[i] + 
            b.Magnitude*Magnitude[i] +
            b.p.Population*p.Population[i] + 
            epsilon[i]
          
          epsilon[i] ~ dnorm(0, tau.epsilon)
        }

    b.p.Population ~ dnorm (0, 0.001)
    b.Magnitude ~ dnorm (0, 0.001)
  
    mu ~ dnorm(0, .001)
  
  tau.epsilon <- pow(sigma.epsilon, -2)
  sigma.epsilon ~ dunif(0, 100)
  
  for (j in 1:NSector){ # Priors for varying coefficients
    b.constmanufact[j] ~ dnorm (0, 0.001)
    b.constagricult[j] ~ dnorm (0, 0.001)
    }
  
  }

# define the vectors of the data matrix for JAGS.
w.Deaths <- as.vector(datsc$w.Deaths)
Deaths <- as.vector(datsc$Deaths)
constmanufact <- as.vector(datsc$constmanufact)
constagricult <- as.vector(datsc$constagricult)
Magnitude <- as.vector(datsc$Magnitude)
p.Population <- as.vector(datsc$p.Population)
country <- as.numeric(as.ordered(datsc$country))
Ncountry <-  as.numeric(as.vector(length(unique(as.numeric(datsc$country)))))
N <-  as.numeric(nrow(datsc))
year = as.vector(datsc$year)
Nyear = nrow(datsc)
Sector = as.vector(as.numeric(factor(datsc$Sector)))
NSector = as.numeric(as.vector(length(unique(as.numeric(datsc$Sector)))))
NIncometax = as.vector(length(unique(as.numeric(datsc$incometax.d))))
incometax.d = as.vector(as.numeric(datsc$incometax.d))+1

jags.data <- list(#Deaths = Deaths,
                  w.Deaths = w.Deaths,
                  constmanufact = constmanufact,
                  constagricult = constagricult,
                  Magnitude = Magnitude,
                  Sector = Sector,
                  NSector = NSector,
                  #p.Population = p.Population,
                  # NIncometax = NIncometax,
                  #incometax.d = incometax.d,
                  # country = country,
                  # Ncountry = Ncountry,
                  N = N)


# Define and name the parameters so JAGS monitors them.
eq.params <- c("b.constmanufact", "b.constagricult", "b.Magnitude", "b.p.Population")


# run the model
earthquakefit <- jags(
        data=jags.data,
        inits=NULL,
        parameters.to.save = eq.params,
        n.chains=4,
        n.iter=100000,
        n.burnin=40000,
        model.file=model.jags)


devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(earthquakefit)


plot(earthquakefit)




# Levels
## 1: Agriculture
## 2: Ind and Min
## 3: Industry
## 4: Min and Agr
## 5: Mining



mu <- earthquakefit$BUGSoutput$summary[grep("mu", rownames(earthquakefit$BUGSoutput$summary)), 1]
sigma <- angell.fit$BUGSoutput$summary[grep("sigma", rownames(angell.fit$BUGSoutput$summary)), 1]
n.sims <- 1000
n.obs <- nrow(dat)
y.pred2 <- matrix(data = NA, nrow = n.sims, ncol = n.obs) 

set.seed(123)

for (obs in 1:n.obs){
  for(sim in 1:n.sims){
    y.pred2[sim, obs] <- rnorm(n = 1, mean = mu[obs], sd = sigma)
  }
}




