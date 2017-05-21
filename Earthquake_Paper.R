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
        xlab("Year") +
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










###############################
# Sectoral Contestation Model
###############################

# cat("\014")
# rm(list=ls())
# graphics.off()




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
                        b.propagrmanu[Sector[i]]*propagrmanu[i] + # multi-level part: allow national output to vary at the local/sector level
                        b.Magnitude[Sector[i]]*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        b.year[yearID[i]] + # year fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] +
                        mu ## intercept
        }
        
        b.r.lat ~ dnorm(0, 0.01)
        b.r.long ~ dnorm(0, 0.01)
        mu  ~ dnorm(0, 0.01) ## intercept
        b.p.Population ~ dnorm(0, 0.01)
        b.Urban ~ dnorm(0, 0.01)
        
        
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




jags.data.sectoral <- list(Deaths = Deaths,
                           propagrmanu = propagrmanu, # constagricult/constmanufact
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
eq.params.sectoral <- c("b.propagrmanu", "b.Magnitude", "b.p.Population", "b.year", "b.r.long", "b.r.lat", "b.Urban", "lambda")
## ----





## ---- sectoral:model:and:data:does:run ----
# run the model
n.iter.sectoral = 200000  # n.iter.sectoral = 200000 // this is for working model
n.burnin.sectoral = 5000 # n.burnin.sectoral = 5000 // this is for working model
n.chains.sectoral = 4 # n.chains.sectoral = 4 for the working model

earthquakefit.sectoral <- jags(
        data=jags.data.sectoral,
        inits=NULL,
        parameters.to.save = eq.params.sectoral,
        n.chains=n.chains.sectoral,
        n.iter=n.iter.sectoral,
        n.burnin=n.burnin.sectoral, 
        model.file=model.jags.sectoral,
        progress.bar = "none")

#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc.sectoral <- as.mcmc(earthquakefit.sectoral)
bayes.mod.fit.gg.sectoral <- ggs(fit.mcmc.sectoral)
# ggmcmc(bayes.mod.fit.gg.sectoral, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Sectoral_Competition.pdf")
graphics.off()
## ----





###############################
# Sectoral Contestation Plot
###############################

## ---- sectoral:model:plot ----
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

# plot
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
              plot.title = element_text(size=7)) +
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


ci.number.sectoral = .8 # modify this parameter to get desired credible intervals.

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
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[1]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[2]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.propagrmanu[3]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[1]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[2]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Magnitude[3]"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.r.lat"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.r.long"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.p.Population"),],
        reg.results.table.sectoral[rownames(reg.results.table.sectoral)==("b.Urban"),]
))

var.labels.sectoral = c("Agr/Ind [Agr]", 
                        "Agr/Ind [Ind]", 
                        "Agr/Ind [Mixed]", 
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
        "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", n.iter.sectoral, " iterations with a burn-in period of n = ", n.burnin.sectoral , " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.sectoral*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects, latitude and longitude were omitted in the table.}\\\\", 
        "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains.sectoral, " chains were run. Detailed diagnostic plots available \\href{https://github.com/hbahamonde/Earthquake_Paper/raw/master/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Sectoral_Competition.pdf}{\\texttt here}.} \\\\")
## ----


## ---- sectoral:model:regression:table:run ----
print.xtable(xtable(
        reg.results.table.sectoral, 
        caption = "Sectoral Competition Model: Simulated Posterior Predictions (Poisson Regression)",
        label = "sectoral:model:regression:table"), 
        auto = TRUE,
        hline.after=c(-1, 0),
        add.to.row = list(pos = list(length(var.labels.sectoral)), command = note.sectoral)
)
## ----













###############################
# Income Tax Adoption Model
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
options(scipen=10000)
set.seed(602)

# specify the model
model.jags.tax <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.Magnitude*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.incometax.d*incometax.d[i] +
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        b.year[yearID[i]] + # year fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] + 
                        mu ## intercept
        }
        
        b.r.lat ~ dnorm(0, 0.01)
        b.r.long ~ dnorm(0, 0.01)
        mu  ~ dnorm(0, 0.01) ## intercept
        b.p.Population ~ dnorm(0, 0.01)
        b.Urban ~ dnorm(0, 0.01)
        b.incometax.d ~ dnorm(0, 0.01)
        b.interaction ~ dnorm(0, 0.01)
        b.Magnitude ~ dnorm(0, 0.01)
        
        for (t in 1:yearN){ # fixed effects
                b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t])
                
                m.b.year[t] ~ dnorm(0, 0.01)
                tau.b.year[t] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
}


#

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




jags.data.tax <- list(Deaths = Deaths,
                      Magnitude = Magnitude^2,
                      p.Population = p.Population,
                      incometax.d = incometax.d,
                      Urban = Urban,
                      r.long = r.long,
                      r.lat = r.lat,
                      yearID = yearID,
                      yearN = yearN,
                      N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.tax <- c("b.Magnitude", "b.p.Population", "b.year", "b.r.long", "b.r.lat", "b.incometax.d", "b.Urban", "lambda")
## ----


## ---- income:tax:model:and:data:run ----
# run the model
n.iter.tax = 200000  # n.iter.tax = 200000 // this is for working model
n.burnin.tax = 5000 # n.burnin.tax = 5000 // this is for working model
n.chains.tax = 4 # n.chains.tax = 4 for the working model

earthquakefit.tax <- jags(
        data=jags.data.tax,
        inits=NULL,
        parameters.to.save = eq.params.tax,
        n.chains = n.chains.tax,
        n.iter = n.iter.tax,
        n.burnin = n.burnin.tax, 
        model.file=model.jags.tax,
        progress.bar = "none")

#### Generates Diagnostic Plots - this links to a link in the Output Table.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggmcmc)

fit.mcmc.tax <- as.mcmc(earthquakefit.tax)
bayes.mod.fit.gg.tax <- ggs(fit.mcmc.tax)
# ggmcmc(bayes.mod.fit.gg.tax, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Income_Tax_Model.pdf")
graphics.off()
## ----


###############################
# Income Tax Adoption Plot
###############################

## ---- income:tax:model:plot ----
## passing fitted model as mcmc object
tax.mcmc <- as.mcmc(earthquakefit.tax)
tax.mcmc.mat <- as.matrix(tax.mcmc)
tax.mcmc.dat <- as.data.frame(tax.mcmc.mat)


### No Income Tax
year.range = unique(datsc$year)

sim.no.income.tax <- matrix(rep(NA, nrow(tax.mcmc.dat)*length(year.range)), nrow = nrow(tax.mcmc.dat))
for(i in 1:length(year.range)){
        sim.no.income.tax[, i] <- 
                tax.mcmc.dat$b.p.Population*p.Population[i] + 
                tax.mcmc.dat$b.r.lat*r.lat[i] + 
                tax.mcmc.dat$b.r.long*r.long[i] + 
                tax.mcmc.dat$b.Urban*Urban[i] +
                tax.mcmc.dat$b.Magnitude*Magnitude[i] +
                incometax.d[i]*0
        
        
}


## credible intervals
bayes.c.eff.mean.no.income.tax <- apply(sim.no.income.tax, 2, mean)
bayes.c.eff.lower.no.income.tax <- apply(sim.no.income.tax, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.no.income.tax <- apply(sim.no.income.tax, 2, function(x) quantile(x, probs = c(0.8)))

# create DF
plot.dat.no.income.tax <- data.frame(year.range, bayes.c.eff.mean.no.income.tax, bayes.c.eff.lower.no.income.tax, bayes.c.eff.upper.no.income.tax); colnames(plot.dat.no.income.tax) <- c("year.range", "mean", "lower", "upper")

plot.dat.no.income.tax = plot.dat.no.income.tax[ which(plot.dat.no.income.tax$year.range<= 1924), ]



### Income Tax
year.range = unique(datsc$year)

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
bayes.c.eff.mean.income.tax <- apply(sim.income.tax, 2, mean)
bayes.c.eff.lower.income.tax <- apply(sim.income.tax, 2, function(x) quantile(x, probs = c(0.1)))
bayes.c.eff.upper.income.tax <- apply(sim.income.tax, 2, function(x) quantile(x, probs = c(0.8)))

# create DF
plot.dat.income.tax <- data.frame(year.range, bayes.c.eff.mean.income.tax, bayes.c.eff.lower.income.tax, bayes.c.eff.upper.income.tax); colnames(plot.dat.income.tax) <- c("year.range", "mean", "lower", "upper")

plot.dat.income.tax = plot.dat.income.tax[ which(plot.dat.income.tax$year.range>= 1924), ]



# income tax adoption plot DF
income.tax.adoption.plot = as.data.frame(rbind(
        as.data.frame(cbind(plot.dat.no.income.tax, 'Tax'= rep("No", nrow(plot.dat.no.income.tax)))),
        as.data.frame(cbind(plot.dat.income.tax, 'Tax'= rep("Yes", nrow(plot.dat.income.tax))))))

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

# plot
ggplot() + 
        geom_smooth(data = income.tax.adoption.plot, aes(x = year.range, y = mean, colour = Tax), alpha = 0.8, size = 0.5, se = F, method = 'loess') +
        geom_ribbon(data = income.tax.adoption.plot, aes(x = year.range, ymin = lower, ymax = upper, fill = Tax), alpha = 0.2) + 
        geom_vline(xintercept = 1924, linetype=2, colour="blue") + 
        xlab("Year") + ylab("Casualties") + 
        theme_bw() + 
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7)) +
        scale_fill_manual(values=c("red", "green")) +
        scale_color_manual(values=c("red", "green"))
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


ci.number.tax = .8 # modify this parameter to get desired credible intervals.

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
        "\\hline \n \\multicolumn{6}{l}", "{ \\scriptsize {\\bf Note}: ", n.iter.tax, " iterations with a burn-in period of n = ", n.burnin.tax , " iterations discarded.}\\\\", "\n \\multicolumn{6}{l}", "{ \\scriptsize ", ci.number.tax*100 ,"\\% credible intervals (upper/lower bounds). All R-Hat statistics below critical levels.}\\\\" ,"\n \\multicolumn{6}{l}", "{ \\scriptsize Standard convergence diagnostics suggest good mixing and convergence.}\\\\","\n \\multicolumn{6}{l}", "{ \\scriptsize Year fixed effects, latitude and longitude were omitted in the table.}\\\\", 
        "\n \\multicolumn{6}{l}","{ \\scriptsize A total of ", n.chains.tax, " chains were run. Detailed diagnostic plots available \\href{https://github.com/hbahamonde/Earthquake_Paper/raw/master/Bahamonde_Earthquake_Paper_Diagnostic_Plots_Income_Tax_Model.pdf}{\\texttt here}.} \\\\")
## ----

## ---- income:tax:model:regression:table:run ----
print.xtable(xtable(
        reg.results.table.tax, 
        caption = "Income Tax Adoption Model: Simulated Posterior Predictions (Poisson Regression)",
        label = "regression:table:income:tax:model"), 
        auto = TRUE,
        hline.after=c(-1, 0),
        add.to.row = list(pos = list(length(var.labels.tax)), command = note.tax)
)
## ----














# Agr Subnational
# Income Tax: IMPLEMENTED
## National Agr Level : LOW
abs(round(max(agr.plot$mean[agr.plot$Tax=="Yes" & agr.plot$prop.range==min(agr.plot$prop.range)])))
## National Agr Level : High
abs(round(max(agr.plot$mean[agr.plot$Tax=="Yes" & agr.plot$prop.range==max(agr.plot$prop.range)])))







##########################
# Model Checking
##########################


## ---- predicted:observed:plot ----
# Sectoral
eq.out.sectoral <- as.data.frame(as.matrix(as.mcmc(earthquakefit.sectoral)))

pred.eq.sectoral <- eq.out.sectoral[, grep("lambda[", colnames(eq.out.sectoral), fixed = T)]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(gtools)

pred.eq.sectoral <- pred.eq.sectoral[, c(mixedsort(names(pred.eq.sectoral)))]

median <- apply(pred.eq.sectoral, 2, median) # median of the column
lower <- apply(pred.eq.sectoral, 2, function(x) quantile(x, probs = c(0.05))) # quantile of the column
upper <- apply(pred.eq.sectoral, 2, function(x) quantile(x, probs = c(0.95))) # quantile of the column

eq.pred.sectoral <- data.frame(
        Model = rep("Sectoral Competition", nrow(datsc)),
        id = 1:nrow(datsc),
        Deaths.observed = datsc$Deaths,
        median = median,
        lower = lower,
        upper = upper)



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
        Model = rep("Income Tax Adoption", nrow(datsc)),
        id = 1:nrow(datsc),
        Deaths.observed = datsc$Deaths,
        median = median,
        lower = lower,
        upper = upper)

# Consolidating Both DF's
model.checking.plot.df = as.data.frame(rbind(eq.pred.tax,eq.pred.sectoral))





if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(data = model.checking.plot.df, 
       aes(x = Deaths.observed, y = reorder(id, Deaths.observed))) + 
        geom_point(aes(
                colour = Model,
                x = median, 
                y = reorder(id,Deaths.observed))) +
        geom_segment(aes(
                colour = Model,
                x = lower, 
                xend = upper, 
                y = reorder(id, Deaths.observed), 
                yend = reorder(id, Deaths.observed)), 
                alpha = 0.5) + 
        geom_point(shape = 21, colour = "red") + 
        ylab("Observation") + xlab("Deaths") + 
        theme_bw() +
        theme(axis.text.y = element_text(size=8))
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
        plot.title = paste(n.chains.sectoral, "chains,", n.iter.sectoral, "iterations and burn-in period of", n.burnin.sectoral)
)
## ----




## ---- traplot:plot:tax ---- 
### traceplots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(mcmcplots)


parms.tax = c(
        "b.incometax.d",
        "b.Magnitude",
        "b.p.Population",
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
        plot.title = paste(n.chains.tax, "chains,", n.iter.tax, "iterations and burn-in period of", n.burnin.tax)
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
        plot.title = paste(n.chains.tax, "chains,", n.iter.tax, "iterations, burn-in period of", n.burnin.tax, "and", ci.number.tax*100, "% credible intervals")
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
        plot.title = paste(n.chains.sectoral, "chains,", n.iter.sectoral, "iterations, burn-in period of", n.burnin.sectoral, "and", ci.number.sectoral*100, "% credible intervals")
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





## ---- year:fixed:effects:plot ----
#### PLOT fixed effects year
if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(gtools,dplyr,reshape2,ggplot2)


# Tax
earthquake.out.tax <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax)))

earthquake.year.tax <- earthquake.out.tax[, grep("b.year[", colnames(earthquake.out.tax), fixed=T)]

earthquake.year.tax <- earthquake.year.tax[, c(mixedsort(names(earthquake.year.tax)))]
colnames(earthquake.year.tax) <- paste("Y",unique(sort(datsc$year)), sep = "")


earthquake.year.tax <- summarise(group_by(melt(earthquake.year.tax), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))

earthquake.year.tax$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.year.tax$variable))
earthquake.year.tax$Model <- "Income Tax Adoption"

# Sectoral Conflict
earthquake.out.sectoral <- as.data.frame(as.matrix(as.mcmc(earthquakefit.sectoral)))

earthquake.year.sectoral <- earthquake.out.sectoral[, grep("b.year[", colnames(earthquake.out.sectoral), fixed=T)]

earthquake.year.sectoral <- earthquake.year.sectoral[, c(mixedsort(names(earthquake.year.sectoral)))]
colnames(earthquake.year.sectoral) <- paste("Y",unique(sort(datsc$year)), sep = "")


earthquake.year.sectoral <- summarise(group_by(melt(earthquake.year.sectoral), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))

earthquake.year.sectoral$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.year.sectoral$variable))
earthquake.year.sectoral$Model <- "Sectoral Competition"

# Combine Two DF's
year.fixed.effects.plot.df = rbind(earthquake.year.sectoral, earthquake.year.tax)

# plot
ggplot(data = year.fixed.effects.plot.df, aes(x = variable, y = mean, fill = Model, colour = Model)) + 
        geom_hline(yintercept = 0, col = "blue") +
        geom_pointrange(aes(ymin = lo, ymax = hi, fill = Model, colour = Model)) + 
        xlab("Year") + 
        ylab("Death-Toll") + 
        theme_bw() + 
        stat_smooth(method="loess", level=0.80)
## ----






# ---- incometax ----
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
        geom_line(data=datsc, aes(x=year, y=propagrmanu, colour="Agr/Ind Proportion"), fill=NA, size=1) +
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

