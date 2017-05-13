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


eq.output.d$Sector2 <- recode(as.factor(eq.output.d$Sector), "1 = 'Industry' ; 2 = 'Mining' ; 3 = 'Agriculture' ; '1 y 2' = 'Ind and Min' ; '1 y 3' = 'Ind and Agr' ; '2 y 3' = 'Min and Agr' ")


eq.output.d$Sector <- recode(as.factor(eq.output.d$Sector), 
                             "1 = 'Industry' ; 
                             2 = 'Industry' ; 
                             3 = 'Agriculture' ; 
                             '1 y 2' = 'Industry' ; 
                             '1 y 3' = 'Mixed' ; 
                             '2 y 3' = 'Mixed' ")

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





## ---- earthquake:ts:plot:chile ----
# Load the earthquake data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")


# time-series plot
ggplot(dat.chile, aes(x = year, y = Magnitude)) +
        geom_point(shape = 21) +
        theme_bw() +
        ggtitle(NULL) +
        stat_smooth(show.legend = F,  method = 'loess')
## ----







## bar plot
Deaths = data.frame(dat.chile$Deaths); colnames(Deaths)[1] <- "Deaths"

ggplot(na.omit(Deaths), aes(factor(Deaths))) + 
        geom_bar(width=.8) + 
        scale_x_discrete(name='Deaths') +
        scale_y_discrete(name='Count') +
        theme_bw() + 
        ggtitle("") + #Death Tolls Associated to Earthquakes: Chile 1500-2010
        coord_flip()



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






## ---- earthquake:map:plot:chile ----
## packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(rgdal, foreign, rgeos, ggplot2)


# load shape file
chile.provinces <- readOGR(dsn = "/Users/hectorbahamonde/RU/Data/shape_files/division_provincial", 
                           layer = "division_provincial",
                           verbose = FALSE)

# load eq data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Chile_Data_Earthquake.RData")
dat.chile <- dat.chile[!(dat.chile$Longitude <= -76),]


#chile.provinces <- gSimplify(chile.provinces, tol=10000, topologyPreserve=T)
chile.provinces <- spTransform(chile.provinces, CRS("+proj=longlat +datum=WGS84"))
chile.provinces <- fortify(chile.provinces)
chile.provinces <- chile.provinces[!(chile.provinces$long <= -76),]



chile.map.plot.d <- data.frame(
        Longitude = na.omit(dat.chile$Longitude), # 184
        Latitude = na.omit(dat.chile$Latitude), # 184
        Magnitude = dat.chile$Magnitude) # 132 // I think I am going to exclude magnitudes, and just show frequency of earthquakes.

chile.map.plot.d = data.frame(na.omit(chile.map.plot.d));rownames(chile.map.plot.d) <- NULL

ggplot() +  
        geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.05, color='black', data=chile.provinces, alpha=1/2) +
        theme_bw() +
        ggtitle(NULL) + 
        geom_point(data=chile.map.plot.d, aes(x=Longitude, y=Latitude, shape=as.factor(round(chile.map.plot.d$Magnitude,0))), color='red') +#shape=21, 
        scale_y_continuous(name='Latitude') +
        scale_x_continuous(name='Longitude') +
        scale_shape_discrete(name="Rounded\nMagnitude")
## ----



#geom_point(data=dat.chile, aes(x=Longitude, y=Latitude), shape=21, color='red') +



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
dat$r.lat = round(dat$Latitude) 
dat$r.long = round(dat$Longitude) 

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
# Prepping Chilean data

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
#dat = dat[!is.na(dat$incometax.y),]  # switch this one off if I am using incometax.d


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

# proportion of Population 
dat$p.Population = dat$Population/1000 

# scaling 
pvars <- c("constagricult","constmanufact","Magnitude", "p.Population") 
datsc <- dat 
save(datsc, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData")


###################################################################### 
cat("\014") 
rm(list=ls()) 

load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 


# overdispersion
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(AER)
o.Deaths <- glm(Deaths ~ ., data = datsc, family = poisson)
dispersiontest(o.Deaths,alternative = c("greater"), trafo=1) # overdispersion is -0.5


###################################################################### 
cat("\014")
rm(list=ls())
graphics.off()

## ---- model:and:data:not:run ----

# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 




# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)

# specify the model
model.jags <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.propagrmanu[Sector[i]]*propagrmanu[i] + # multi-level part: allow national output to vary at the local/sector level
                        b.Magnitude[Sector[i]]*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.incometax.d*incometax.d[i] +
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        b.year[yearID[i]] + # year fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] +
                        b.interaction[Sector[i]]*propagrmanu[i]*incometax.d[i] + # interaction term
                        mu ## intercept
        }
        
        b.r.lat ~ dnorm(0, 0.01)
        b.r.long ~ dnorm(0, 0.01)
        mu  ~ dnorm(0, 0.01) ## intercept
        b.p.Population ~ dnorm(0, 0.01)
        b.Urban ~ dnorm(0, 0.01)
        # b.propagrmanu ~ dnorm(0, 0.001)
        b.incometax.d ~ dnorm(0, 0.01)
        # b.interaction ~ dnorm(0, 0.01)
        
        
        for (t in 1:yearN){ # fixed effects
                b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t])
                
                m.b.year[t] ~ dnorm(0, 0.01)
                tau.b.year[t] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
        ## Varying Slopes for Sector (unmodeled)
        for (k in 1:NSector){ # 
                b.Magnitude[k] ~ dnorm(m.Magnitude[k], tau.Magnitude[k])
                m.Magnitude[k] ~ dnorm(0, 0.01)
                tau.Magnitude[k] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
        for (k in 1:NSector){ # 
                b.propagrmanu[k] ~ dnorm(m.b.propagrmanu[k], tau.b.propagrmanu[k])
                m.b.propagrmanu[k] ~ dnorm(0, 0.01)
                tau.b.propagrmanu[k] ~ dgamma(0.5, 0.001) # uninformative prior
                }
        
        ## for the beta associated with the interaction, one slope per sector
        for (k in 1:NSector){ # 
                b.interaction[k] ~ dnorm(m.b.interaction[k], tau.b.interaction[k])
                m.b.interaction[k] ~ dnorm(0, 0.01)
                tau.b.interaction[k] ~ dgamma(0.5, 0.001) # uninformative prior
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
# Nyear = nrow(datsc) # for year as counting variable
yearID = as.numeric(as.ordered(datsc$year))
yearN = length(unique(datsc$year))
Sector = as.vector(as.numeric(factor(datsc$Sector)))
NSector = as.numeric(as.vector(length(unique(as.numeric(datsc$Sector)))))
NIncometax = as.vector(length(unique(as.numeric(datsc$incometax.d))))
incometax.d = as.vector(as.numeric(datsc$incometax.d))
NIncometax.y = as.vector(length(unique(as.numeric(datsc$incometax.y))))
incometax.y = as.vector(as.numeric(datsc$incometax.y))
Urban = as.vector(as.numeric(datsc$Urban))
customtax = as.vector(as.numeric(datsc$customtax))/100
propagrmanu = as.vector(as.numeric(datsc$propagrmanu))
r.long = as.vector(as.numeric(datsc$r.long))
r.lat = as.vector(as.numeric(datsc$r.lat))

propagrmanu.seq = seq(
        from = min(as.numeric(datsc$propagrmanu)),
        to = max(as.numeric(datsc$propagrmanu)),
        length.out = as.numeric(nrow(datsc)))

N.sim = length(seq(
        from = min(as.numeric(datsc$propagrmanu)),
        to = max(as.numeric(datsc$propagrmanu)),
        length.out = as.numeric(nrow(datsc))))




jags.data <- list(Deaths = Deaths,
                  #w.Deaths = w.Deaths,
                  # constmanufact = constmanufact,
                  # constagricult = constagricult,
                  propagrmanu = propagrmanu, # constagricult/constmanufact
                  Magnitude = Magnitude^2,
                  Sector = Sector,
                  NSector = NSector,
                  p.Population = p.Population,
                  # NIncometax = NIncometax,
                  incometax.d = incometax.d,
                  # incometax.y = incometax.y,
                  # customtax = customtax,
                  # NIncometax.y = NIncometax.y,
                  # country = country,
                  # Ncountry = Ncountry,
                  Urban = Urban,
                  r.long = r.long,
                  r.lat = r.lat,
                  yearID = yearID,
                  yearN = yearN,
                  # N.sim = N.sim, # for predictions
                  # propagrmanu.seq = propagrmanu.seq, # for predictions
                  N = N)


# Define and name the parameters so JAGS monitors them.
eq.params <- c("b.propagrmanu", "b.Magnitude", "b.p.Population", "b.year", "b.r.long", "b.r.lat", "b.incometax.d", "b.Urban", "b.interaction", "lambda")
## ----







## ---- model:and:data:does:run ----
# run the model

n.iter = 3  # n.iter = 200000 // this is for working model
n.burnin = 0 # n.burnin = 5000 // this is for working model
n.chains = 1 # n.chains = 4 for the working model

earthquakefit <- jags(
        data=jags.data,
        inits=NULL,
        parameters.to.save = eq.params,
        n.chains=n.chains,
        n.iter=n.iter,
        n.burnin=n.burnin, 
        #n.thin=1,
        model.file=model.jags,
        progress.bar = "none")

#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc <- as.mcmc(earthquakefit)
bayes.mod.fit.gg <- ggs(fit.mcmc)
ggmcmc(bayes.mod.fit.gg, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots.pdf")
graphics.off()

## ----












## ---- interaction:plots:not:run ----

##########################################
# conditional effect of income tax
##########################################

# Interaction Term // Simulation

## passing fitted model as mcmc object
int.mcmc <- as.mcmc(earthquakefit)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- as.data.frame(int.mcmc.mat)

## range of interest
prop.range <- seq(min(propagrmanu), max(propagrmanu), by = 0.01)

##########################################
## Bayes: b.incometax.d NO


### Agricultural Subnational
int.sim.prop.0.agr <- matrix(rep(NA, nrow(int.mcmc.dat)*length(prop.range)), nrow = nrow(int.mcmc.dat))
for(i in 1:length(prop.range)){
        int.sim.prop.0.agr[, i] <- int.mcmc.dat$'b.propagrmanu[1]'*prop.range[i]
}
## credible intervals
### Note: the variance now comes from the posterior, not the vcov matrix
bayes.c.eff.mean.prop.0.agr <- apply(int.sim.prop.0.agr, 2, mean)
bayes.c.eff.lower.prop.0.agr <- apply(int.sim.prop.0.agr, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.0.agr <- apply(int.sim.prop.0.agr, 2, function(x) quantile(x, probs = c(0.8)))
## create df
plot.dat.prop.0.agr <- data.frame(prop.range, bayes.c.eff.mean.prop.0.agr, bayes.c.eff.lower.prop.0.agr, bayes.c.eff.upper.prop.0.agr); colnames(plot.dat.prop.0.agr) <- c("prop.range", "mean", "lower", "upper")



### Industrial Subnational
int.sim.prop.0.ind <- matrix(rep(NA, nrow(int.mcmc.dat)*length(prop.range)), nrow = nrow(int.mcmc.dat))
for(i in 1:length(prop.range)){
        int.sim.prop.0.ind[, i] <- int.mcmc.dat$'b.propagrmanu[2]'*prop.range[i]
}
## credible intervals
### Note: the variance now comes from the posterior, not the vcov matrix
bayes.c.eff.mean.prop.0.ind <- apply(int.sim.prop.0.ind, 2, mean)
bayes.c.eff.lower.prop.0.ind <- apply(int.sim.prop.0.ind, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.0.ind <- apply(int.sim.prop.0.ind, 2, function(x) quantile(x, probs = c(0.8)))
## create df
plot.dat.prop.0.ind <- data.frame(prop.range, bayes.c.eff.mean.prop.0.ind, bayes.c.eff.lower.prop.0.ind, bayes.c.eff.upper.prop.0.ind); colnames(plot.dat.prop.0.ind) <- c("prop.range", "mean", "lower", "upper")



##########################################
## Bayes: b.incometax.d YES

### Agricultural Subnational
int.sim.prop.1.agr <- matrix(rep(NA, nrow(int.mcmc.dat)*length(prop.range)), nrow = nrow(int.mcmc.dat))
for(i in 1:length(prop.range)){
        int.sim.prop.1.agr[, i] <- int.mcmc.dat$b.incometax.d + (int.mcmc.dat$'b.propagrmanu[1]'+int.mcmc.dat$'b.interaction[1]')*prop.range[i]
}


## credible intervals
### Note: the variance now comes from the posterior, not the vcov matrix
bayes.c.eff.mean.prop.1.agr <- apply(int.sim.prop.1.agr, 2, mean)
bayes.c.eff.lower.prop.1.agr <- apply(int.sim.prop.1.agr, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.1.agr <- apply(int.sim.prop.1.agr, 2, function(x) quantile(x, probs = c(0.8)))
## create df
plot.dat.prop.1.agr <- data.frame(prop.range, bayes.c.eff.mean.prop.1.agr, bayes.c.eff.lower.prop.1.agr, bayes.c.eff.upper.prop.1.agr); colnames(plot.dat.prop.1.agr) <- c("prop.range", "mean", "lower", "upper")


### Industrial Subnational
int.sim.prop.1.ind <- matrix(rep(NA, nrow(int.mcmc.dat)*length(prop.range)), nrow = nrow(int.mcmc.dat))
for(i in 1:length(prop.range)){
        int.sim.prop.1.ind[, i] <- int.mcmc.dat$b.incometax.d + (int.mcmc.dat$'b.propagrmanu[2]'+int.mcmc.dat$'b.interaction[2]')*prop.range[i]
}
## credible intervals
### Note: the variance now comes from the posterior, not the vcov matrix
bayes.c.eff.mean.prop.1.ind <- apply(int.sim.prop.1.ind, 2, mean)
bayes.c.eff.lower.prop.1.ind <- apply(int.sim.prop.1.ind, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.prop.1.ind <- apply(int.sim.prop.1.ind, 2, function(x) quantile(x, probs = c(0.8)))
## create df
plot.dat.prop.1.ind <- data.frame(prop.range, bayes.c.eff.mean.prop.1.ind, bayes.c.eff.lower.prop.1.ind, bayes.c.eff.upper.prop.1.ind); colnames(plot.dat.prop.1.ind) <- c("prop.range", "mean", "lower", "upper")


##########################################
## Use blue for Bayesian, red for frequentist estimates. Transparency to allow overlay; purple indicates complete overlay. Take a close look at the upper and lower limits of the CI for each estimate.
## Foundation for the plot & line for the posterior mean of the Bayesian conditional effect

## Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

# industrial subnational
ind.plot = as.data.frame(rbind(
        as.data.frame(cbind(plot.dat.prop.1.ind, 'Tax'= rep("Yes", nrow(plot.dat.prop.1.ind)))),
        as.data.frame(cbind(plot.dat.prop.0.ind, 'Tax'= rep("No", nrow(plot.dat.prop.0.ind))))))
ind.ggplot = ggplot() + 
        geom_line(data = ind.plot, aes(x = prop.range, y = mean, colour = Tax), alpha = 0.8, size = 0.5) + 
        geom_ribbon(data = ind.plot, aes(x = prop.range, ymin = lower, ymax = upper, fill = Tax), alpha = 0.2) + 
        xlab("Proportion Agr/Ind Output\nNational Contestation") + ylab("Casualties") + 
        theme_bw() + 
        labs(title = "Subnational Level: Industrial") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7)) +
        scale_fill_manual(values=c("green", "red")) +
        scale_color_manual(values=c("green", "red"))


# agricultural subnational
agr.plot = as.data.frame(rbind(
        as.data.frame(cbind(plot.dat.prop.1.agr, 'Tax'= rep("Yes", nrow(plot.dat.prop.1.agr)))),
        as.data.frame(cbind(plot.dat.prop.0.agr, 'Tax'= rep("No", nrow(plot.dat.prop.0.agr))))))
agr.ggplot = ggplot() + 
        geom_line(data = agr.plot, aes(x = prop.range, y = mean, colour = Tax), alpha = 0.8, size = 0.5) + 
        geom_ribbon(data = agr.plot, aes(x = prop.range, ymin = lower, ymax = upper, fill = Tax), alpha = 0.2) + 
        xlab("Proportion Agr/Ind Output\nNational Contestation") + ylab("Casualties") + 
        theme_bw() + 
        labs(title = "Subnational Level: Agricultural") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7)) +
        scale_fill_manual(values=c("green", "red")) +
        scale_color_manual(values=c("green", "red"))


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(gridExtra,grid,ggplot2)


# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
        
        plots <- list(...)
        position <- match.arg(position)
        g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
        gl <- c(gl, nrow = nrow, ncol = ncol)
        
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
## ----




## ---- interaction:plots:run ----
grid_arrange_shared_legend(agr.ggplot, ind.ggplot, ncol = 2, nrow = 1)
## ----





# Agr Subnational
# Income Tax: IMPLEMENTED
## National Agr Level : LOW
abs(round(max(agr.plot$mean[agr.plot$Tax=="Yes" & agr.plot$prop.range==min(agr.plot$prop.range)])))
## National Agr Level : High
abs(round(max(agr.plot$mean[agr.plot$Tax=="Yes" & agr.plot$prop.range==max(agr.plot$prop.range)])))


# Agr Subnational
# Income Tax: NOT IMPLEMENTED
# National Agr Level : LOW
abs(round(max(agr.plot$mean[agr.plot$Tax=="No" & agr.plot$prop.range==min(agr.plot$prop.range)])))
## National Agr Level : High
abs(round(max(agr.plot$mean[agr.plot$Tax=="No" & agr.plot$prop.range==max(agr.plot$prop.range)])))



# Ind Subnational
# Income Tax: IMPLEMENTED
## National Agr Level : LOW
abs(round(max(ind.plot$mean[ind.plot$Tax=="Yes" & ind.plot$prop.range==min(ind.plot$prop.range)])))
## National Agr Level : High
abs(round(max(ind.plot$mean[ind.plot$Tax=="Yes" & ind.plot$prop.range==max(ind.plot$prop.range)])))


# Ind Subnational
# Income Tax: NOT IMPLEMENTED
# National Agr Level : LOW
abs(round(max(ind.plot$mean[ind.plot$Tax=="No" & ind.plot$prop.range==min(ind.plot$prop.range)])))
## National Agr Level : High
abs(round(max(ind.plot$mean[ind.plot$Tax=="No" & ind.plot$prop.range==max(ind.plot$prop.range)])))







## ---- regression:table:not:run ----

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


ci.number = .8 # modify this parameter to get desired credible intervals.

mcmctab <- function(sims, ci = ci.number, digits = 2){
        
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



reg.results.table = data.frame(mcmctab(earthquakefit)[1:14,]) # Posterior distributions // Year FE excluded


reg.results.table = data.frame(rbind( # re order df by name of the rowname according to what I have and define in 'var.labels.'
        reg.results.table[rownames(reg.results.table)==("b.propagrmanu[1]"),],
        reg.results.table[rownames(reg.results.table)==("b.propagrmanu[2]"),],
        reg.results.table[rownames(reg.results.table)==("b.propagrmanu[3]"),],
        reg.results.table[rownames(reg.results.table)==("b.incometax.d"),],
        reg.results.table[rownames(reg.results.table)==("b.interaction[1]"),],
        reg.results.table[rownames(reg.results.table)==("b.interaction[2]"),],
        reg.results.table[rownames(reg.results.table)==("b.interaction[3]"),],
        reg.results.table[rownames(reg.results.table)==("b.Magnitude[1]"),],
        reg.results.table[rownames(reg.results.table)==("b.Magnitude[2]"),],
        reg.results.table[rownames(reg.results.table)==("b.Magnitude[3]"),],
        reg.results.table[rownames(reg.results.table)==("b.p.Population"),],
        reg.results.table[rownames(reg.results.table)==("b.Urban"),]
))

var.labels = c("Agr/Ind [Agr]", "Agr/Ind [Ind]", "Agr/Ind [Mixed]", "Income Tax", "Agr/Ind * Income Tax [Agr]",  "Agr/Ind * Income Tax [Ind]",  "Agr/Ind * Income Tax [Mixed]", "Magnitude [Agr]", "Magnitude [Ind]", "Magnitude [Mixed]","Population", "Urban")

rownames(reg.results.table) <- var.labels



# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(xtable)

note <- paste0(
        "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", n.iter, " iterations with a burn-in period of n = ", n.burnin , " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects, latitude and longitude were omitted in the table.}\\\\", 
        "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains, " chains were run. Detailed diagnostic plots available \\href{https://github.com/hbahamonde/Earthquake_Paper/raw/master/Bahamonde_Earthquake_Paper_Diagnostic_Plots.pdf}{\\texttt here}.} \\\\")
## ----


## ---- regression:table:run ----
print.xtable(xtable(
        reg.results.table, 
        caption = "Poisson Regression: Simulated Posterior Predictions",
        label = "regression:table"), 
        auto = TRUE,
        hline.after=c(-1, 0),
        add.to.row = list(pos = list(12),command = note)
)
## ----






###
plot(earthquakefit)

# Sectors
## 1 Agriculture 
## 2 Industry 
## 3 Mixed


##########################
# Model Checking
##########################


## ---- predicted:observed:plot ----
eq.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit)))
pred.eq <- eq.out[, grep("lambda[", colnames(eq.out), fixed = T)]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(gtools)

pred.eq <- pred.eq[, c(mixedsort(names(pred.eq)))]

pred.eq.median <- apply(pred.eq, 2, median) # median of the column
pred.eq.lower <- apply(pred.eq, 2, function(x) quantile(x, probs = c(0.05))) # quantile of the column
pred.eq.upper <- apply(pred.eq, 2, function(x) quantile(x, probs = c(0.95))) # quantile of the column

eq.pred <- data.frame(
        id = 1:nrow(datsc),
        Deaths.observed = datsc$Deaths,
        pred.eq.median = pred.eq.median,
        pred.eq.lower = pred.eq.lower,
        pred.eq.upper = pred.eq.upper)


if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(data = eq.pred, 
       aes(x = Deaths.observed, y = reorder(id, Deaths.observed))) + 
        geom_point(aes(
                x = pred.eq.median, 
                y = reorder(id,Deaths.observed))) +
        geom_segment(aes(
                x = pred.eq.lower, 
                xend = pred.eq.upper, 
                y = reorder(id, Deaths.observed), 
                yend = reorder(id, Deaths.observed)), 
                alpha = 0.5) + 
        geom_point(shape = 21, colour = "red") + 
        ylab("Observation") + xlab("Deaths") + theme_bw() +
        theme(axis.text.y = element_text(size=8)
        )
## ----

##########################
##########################






### summary of results
fit.mcmc <- as.mcmc(earthquakefit)
# summary(fit.mcmc)

## ---- traplot:plot ---- 
### traceplots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)


parms = c(
        #"b.propagrmanu",
        #"b.incometax.d", 
        "b.interaction")

traplot(earthquakefit, parms = parms)
## ----


## ---- denplot:plot ---- 
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)

denplot(earthquakefit, 
        parms = "b.interaction", 
        style="plain", 
        ci=0.8, 
        xlim=c(-40,50), 
        collapse=T, 
        col=2, 
        lty=2, 
        main=c("Agriculture", 
               "Industry",
               "Mixed"))

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





## ---- year:fixed:effects:plot ----
#### PLOT fixed effects year
p_load(gtools,dplyr,reshape2,ggplot2)

earthquake.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit)))

earthquake.year <- earthquake.out[, grep("b.year[", colnames(earthquake.out), fixed=T)]

earthquake.year <- earthquake.year[, c(mixedsort(names(earthquake.year)))]
colnames(earthquake.year) <- paste("Y",unique(sort(datsc$year)), sep = "")


earthquake.year <- summarise(group_by(melt(earthquake.year), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))
earthquake.year$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.year$variable))

ggplot(data = earthquake.year, aes(x = variable, y = mean)) + 
  geom_hline(yintercept = 0, col = "blue") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) + 
  xlab("Year") + 
  ylab("Fixed Effects: Year") + 
  theme_bw() + 
  stat_smooth(method="loess", level=0.80)
## ----




# ---- incometax ----
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


#### Logs Plot

# Load data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") 

par(mar=c(10,10,1,1)) # bottom, then left margin, upper and right margins

p1 = ggplot() + 
        geom_smooth(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_smooth(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law  
        theme_bw() + 
        theme(
                axis.text.y = element_text(size=10), 
                axis.text.x = element_text(size=10), 
                axis.title.y = element_text(size=10), 
                axis.title.x = element_text(size=10), 
                legend.text=element_text(size=10), 
                legend.title=element_text(size=0),
                legend.position="bottom")  + 
        labs(title="") 

#### Proportion Plot
# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

par(mar=c(10,10,1,1)) # bottom, then left margin, upper and right margins

p2 = ggplot() + 
        geom_smooth(data=datsc, aes(x=year, y=propagrmanu), fill=NA, size=1) +
        xlab("Year") +
        ylab("Agr/Ind Proportion") +
        labs(colour = "Legend") +
        scale_y_continuous(breaks= seq(0, 1, by = 0.2)) +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924), linetype = "longdash", colour= "#00BA38") + # Income Tax Law
        theme_bw() + 
        theme(
                axis.text.y = element_text(size=10), 
                axis.text.x = element_text(size=10), 
                axis.title.y = element_text(size=10), 
                axis.title.x = element_text(size=10), 
                legend.text=element_text(size=10), 
                legend.title=element_text(size=0),
                legend.position="bottom")  + 
        labs(title="")



grid_arrange_shared_legend(p1, p2, ncol = 1, nrow = 2)
# ----














## http://rstudio-pubs-static.s3.amazonaws.com/12451_53fc5e6bd80744b99158a12975c31cbf.html
## overdispersion
### epsilon (ϵ) represents overdispersion
### tau (τ) is a measure of the amount of overdispersion
### mu (μ) represents the intercept


#### Predicted Probabilities.
#### FROM: https://github.com/jkarreth/Bayes/blob/master/logit.pp.plot.instructions.R // line:52

