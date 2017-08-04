cat("\014")
rm(list=ls())


################################
## MODEL: SECTORAL (FE)
################################


# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 




# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)

# specify the model
model.jags.sectoral.fe <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.propagrmanu.observation[Sector[i]]*propagrmanu[i] + # multi-level part: allow national output to vary at the local/sector level
                        b.Magnitude[Sector[i]]*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        # b.year[yearID[i]] + # year fixed-effects
                        b.observation.comp.model[i] + # observation fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] +
                        mu ## intercept
        }
        
        b.r.lat ~ dnorm(0, 0.01)
        b.r.long ~ dnorm(0, 0.01)
        mu  ~ dnorm(0, 0.01) ## intercept
        b.p.Population ~ dnorm(0, 0.01)
        b.Urban ~ dnorm(0, 0.01)
        
        
        # for (t in 1:yearN){ # year fixed effects
        #        b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t])
        #        
        #        m.b.year[t] ~ dnorm(0, 0.01)
        #        tau.b.year[t] ~ dgamma(0.5, 0.001) # uninformative prior
        #}
        
        for (i in 1:N){ # observation fixed effects
                b.observation.comp.model[i] ~ dnorm(m.b.observation.comp.model[i], tau.b.observation.comp.model[i])
                
                m.b.observation.comp.model[i] ~ dnorm(0, 0.01)
                tau.b.observation.comp.model[i] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
        ## Varying Slopes for Sector (unmodeled)
        for (k in 1:NSector){ # 
                b.Magnitude[k] ~ dnorm(m.Magnitude[k], tau.Magnitude[k])
                m.Magnitude[k] ~ dnorm(0, 0.01)
                tau.Magnitude[k] ~ dgamma(0.5, 0.001) # uninformative prior
        }
        
        for (k in 1:NSector){ # 
                b.propagrmanu.observation[k] ~ dnorm(m.b.propagrmanu.observation[k], tau.b.propagrmanu.observation[k])
                m.b.propagrmanu.observation[k] ~ dnorm(0, 0.01)
                tau.b.propagrmanu.observation[k] ~ dgamma(0.5, 0.001) # uninformative prior
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



jags.data.sectoral.fe <- list(Deaths = Deaths,
                           propagrmanu = propagrmanu, # constagricult/constmanufact
                           Magnitude = Magnitude^2,
                           Sector = Sector,
                           NSector = NSector,
                           p.Population = p.Population,
                           Urban = Urban,
                           r.long = r.long,
                           r.lat = r.lat,
                           #yearID = yearID,
                           #yearN = yearN,
                           N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.sectoral.fe <- c("b.propagrmanu.observation", "b.observation.comp.model")



# run the model
n.iter.sectoral = 200000  # n.iter.sectoral = 200000 // this is for working model
n.burnin.sectoral = 5000 # n.burnin.sectoral = 5000 // this is for working model
n.chains.sectoral = 4 # n.chains.sectoral = 4 for the working model

earthquakefit.sectoral.fe <- jags(
        data=jags.data.sectoral.fe,
        inits=NULL,
        parameters.to.save = eq.params.sectoral.fe,
        n.chains=n.chains.sectoral,
        n.iter=n.iter.sectoral,
        n.burnin=n.burnin.sectoral, 
        model.file=model.jags.sectoral.fe,
        progress.bar = "none")





################################
## MODEL: INCOME TAX ADOPTION (FE)
################################



# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)

# specify the model
model.jags.tax.fe <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.Magnitude*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.incometax.d.observation*incometax.d[i] +
                        b.p.Population*p.Population[i] +
                        b.Urban*Urban[i] +
                        b.observation.tax.model[i] + # observation fixed-effects
                        #b.year[yearID[i]] + # year fixed-effects
                        b.r.long*r.long[i] +
                        b.r.lat*r.lat[i] + 
                        mu ## intercept
        }
        
        b.r.lat ~ dnorm(0, 0.01)
        b.r.long ~ dnorm(0, 0.01)
        mu  ~ dnorm(0, 0.01) ## intercept
        b.p.Population ~ dnorm(0, 0.01)
        b.Urban ~ dnorm(0, 0.01)
        b.incometax.d.observation ~ dnorm(0, 0.01)
        b.interaction ~ dnorm(0, 0.01)
        b.Magnitude ~ dnorm(0, 0.01)
        
        #for (t in 1:yearN){ # fixed effects
        #        b.year[t] ~ dnorm(m.b.year[t], tau.b.year[t])
        #        
        #        m.b.year[t] ~ dnorm(0, 0.01)
        #        tau.b.year[t] ~ dgamma(0.5, 0.001) # uninformative prior
        #}


        for (i in 1:N){ # observation fixed effects
        b.observation.tax.model[i] ~ dnorm(m.b.observation.tax.model[i], tau.b.observation.tax.model[i])
        
        m.b.observation.tax.model[i] ~ dnorm(0, 0.01)
        tau.b.observation.tax.model[i] ~ dgamma(0.5, 0.001) # uninformative prior
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



jags.data.tax.fe <- list(Deaths = Deaths,
                      Magnitude = Magnitude^2,
                      p.Population = p.Population,
                      incometax.d = incometax.d,
                      Urban = Urban,
                      r.long = r.long,
                      r.lat = r.lat,
                      #yearID = yearID,
                      #yearN = yearN,
                      N = N)


# Define and name the parameters so JAGS monitors them.
eq.params.tax.fe <- c("b.incometax.d.observation", "b.observation.tax.model")



# run the model
n.iter.tax = 200000  # n.iter.tax = 200000 // this is for working model
n.burnin.tax = 5000 # n.burnin.tax = 5000 // this is for working model
n.chains.tax = 4 # n.chains.tax = 4 for the working model

earthquakefit.tax.fe <- jags(
        data=jags.data.tax.fe,
        inits=NULL,
        parameters.to.save = eq.params.tax.fe,
        n.chains = n.chains.tax,
        n.iter = n.iter.tax,
        n.burnin = n.burnin.tax, 
        model.file=model.jags.tax.fe,
        progress.bar = "none")




###############################
# Income Tax Adoption Model NORMAL
###############################

# cat("\014")
# rm(list=ls())
# graphics.off()

# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)

# specify the model
model.jags.tax.normal <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.Magnitude*Magnitude[i] + #  multi-level part: allow national output to vary at the local/sector level
                        b.incometax.d.normal*incometax.d[i] +
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
        b.incometax.d.normal ~ dnorm(0, 0.01)
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




jags.data.tax.normal <- list(Deaths = Deaths,
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
eq.params.tax.normal.tax <- c("b.incometax.d.normal")


# run the model
n.iter.tax = 200000  # n.iter.tax = 200000 // this is for working model
n.burnin.tax = 5000 # n.burnin.tax = 5000 // this is for working model
n.chains.tax = 4 # n.chains.tax = 4 for the working model

earthquakefit.tax.normal.tax <- jags(
        data=jags.data.tax.normal,
        inits=NULL,
        parameters.to.save = eq.params.tax.normal.tax,
        n.chains = n.chains.tax,
        n.iter = n.iter.tax,
        n.burnin = n.burnin.tax, 
        model.file=model.jags.tax.normal,
        progress.bar = "none")






##########################################
# SECTORAL MODEL NORMAL
##########################################

# load data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/Earthquake_Paper/eq_output_d_Chile.RData") 


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(R2jags, coda, R2WinBUGS, lattice, rjags, runjags)

# lower tolerance
options(scipen=10000)
set.seed(602)

# specify the model
model.jags.sectoral.normal <- function() {
        for (i in 1:N){ # number of earthquakes
                Deaths[i] ~ dpois(lambda[i])
                
                log(lambda[i]) <- 
                        b.propagrmanu.normal[Sector[i]]*propagrmanu[i] + # multi-level part: allow national output to vary at the local/sector level
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
                b.propagrmanu.normal[k] ~ dnorm(m.b.propagrmanu.normal[k], tau.b.propagrmanu.normal[k])
                m.b.propagrmanu.normal[k] ~ dnorm(0, 0.01)
                tau.b.propagrmanu.normal[k] ~ dgamma(0.5, 0.001) # uninformative prior
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


jags.data.sectoral.normal <- list(Deaths = Deaths,
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
eq.params.sectoral.normal <- c("b.propagrmanu.normal")



n.iter.sectoral = 200000  # n.iter.sectoral = 200000 // this is for working model
n.burnin.sectoral = 5000 # n.burnin.sectoral = 5000 // this is for working model
n.chains.sectoral = 4 # n.chains.sectoral = 4 for the working model

earthquakefit.sectoral.normal <- jags(
        data=jags.data.sectoral.normal,
        inits=NULL,
        parameters.to.save = eq.params.sectoral.normal,
        n.chains=n.chains.sectoral,
        n.iter=n.iter.sectoral,
        n.burnin=n.burnin.sectoral, 
        model.file=model.jags.sectoral.normal,
        progress.bar = "none")









##################
###### PLOT fixed effects year data prep
##################

if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(gtools,dplyr,reshape2,ggplot2)


# Tax
earthquake.out.tax.fe <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax.fe)))

earthquake.N.tax.fe <- earthquake.out.tax.fe[, grep("b.observation.tax.model[", colnames(earthquake.out.tax.fe), fixed=T)]

earthquake.N.tax.fe <- earthquake.N.tax.fe[, c(mixedsort(names(earthquake.N.tax.fe)))]


rownames(datsc) <- NULL
datsc$N = as.numeric(c(rownames(datsc)))
colnames(earthquake.N.tax.fe) <- paste("Y",unique(sort(datsc$N)), sep = "")


earthquake.N.tax.fe <- summarise(group_by(melt(earthquake.N.tax.fe), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))

earthquake.N.tax.fe$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.N.tax.fe$variable))
earthquake.N.tax.fe$Model <- "Income Tax Adoption"







# Sectoral Conflict
earthquake.out.sectoral.fe <- as.data.frame(as.matrix(as.mcmc(earthquakefit.sectoral.fe)))

earthquake.N.sectoral.fe <- earthquake.out.sectoral.fe[, grep("b.observation.comp.model[", colnames(earthquake.out.sectoral.fe), fixed=T)]

earthquake.N.sectoral.fe <- earthquake.N.sectoral.fe[, c(mixedsort(names(earthquake.N.sectoral.fe)))]


rownames(datsc) <- NULL
datsc$N = as.numeric(c(rownames(datsc)))
colnames(earthquake.N.sectoral.fe) <- paste("Y",unique(sort(datsc$N)), sep = "")


earthquake.N.sectoral.fe <- summarise(group_by(melt(earthquake.N.sectoral.fe), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))

earthquake.N.sectoral.fe$variable <- as.numeric(gsub(pattern = "Y", replacement = "", x = earthquake.N.sectoral.fe$variable))
earthquake.N.sectoral.fe$Model <- "Sectoral Competition"

# Combine Two DF's
N.fixed.effects.plot.df = rbind(earthquake.N.sectoral.fe, earthquake.N.tax.fe)




##### DENSITY PLOTS




##################
###### PLOT SECTORAL COMPETITION
##################





if (!require("pacman")) install.packages("pacman"); library(pacman)  
p_load(gtools,dplyr,reshape2,ggplot2)


## Sectoral, Normal
earthquakefit.sectoral.normal.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit.sectoral.normal)))
earthquakefit.sectoral.normal.out <- earthquakefit.sectoral.normal.out[, grep("b.propagrmanu.normal[", colnames(earthquakefit.sectoral.normal.out), fixed=T)]
colnames(earthquakefit.sectoral.normal.out) <- c("Agriculture", "Industry", "Mixed")
earthquakefit.sectoral.normal.out <- earthquakefit.sectoral.normal.out[, c(mixedsort(names(earthquakefit.sectoral.normal.out)))]
earthquakefit.sectoral.normal.out <- summarise(group_by(melt(earthquakefit.sectoral.normal.out), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))
earthquakefit.sectoral.normal.out$'Individual Fixed Effects' = as.character("No")
earthquakefit.sectoral.normal.out$"Model" = as.character("Sectoral Competition")




## Sectoral, FE 
earthquakefit.sectoral.fe.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit.sectoral.fe)))
earthquakefit.sectoral.fe.out <- earthquakefit.sectoral.fe.out[, grep("b.propagrmanu.observation[", colnames(earthquakefit.sectoral.fe.out), fixed=T)]
colnames(earthquakefit.sectoral.fe.out) <- c("Agriculture", "Industry", "Mixed")
earthquakefit.sectoral.fe.out <- earthquakefit.sectoral.fe.out[, c(mixedsort(names(earthquakefit.sectoral.fe.out)))]
earthquakefit.sectoral.fe.out <- summarise(group_by(melt(earthquakefit.sectoral.fe.out), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))
earthquakefit.sectoral.fe.out$'Individual Fixed Effects' = as.character("Yes")
earthquakefit.sectoral.fe.out$"Model" = as.character("Sectoral Competition")




## Income Tax, Normal
earthquakefit.tax.normal.tax.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax.normal.tax)))
earthquakefit.tax.normal.tax.out <- earthquakefit.tax.normal.tax.out["b.incometax.d.normal"]
colnames(earthquakefit.tax.normal.tax.out) <- c("Income Tax")
earthquakefit.tax.normal.tax.out <- summarise(group_by(melt(earthquakefit.tax.normal.tax.out), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))
earthquakefit.tax.normal.tax.out$'Individual Fixed Effects' = as.character("No")
earthquakefit.tax.normal.tax.out$"Model" = as.character("Income Tax Adoption")


## Income Tax, FE
earthquakefit.tax.fe.out <- as.data.frame(as.matrix(as.mcmc(earthquakefit.tax.fe)))
earthquakefit.tax.fe.out <- earthquakefit.tax.fe.out["b.incometax.d.observation"]
colnames(earthquakefit.tax.fe.out) <- c("Income Tax")
earthquakefit.tax.fe.out <- summarise(group_by(melt(earthquakefit.tax.fe.out), variable), mean = mean(value), lo = quantile(value, probs = c(0.20)), hi = quantile(value, probs = c(0.80)))
earthquakefit.tax.fe.out$'Individual Fixed Effects' = as.character("Yes")
earthquakefit.tax.fe.out$"Model" = as.character("Income Tax Adoption")



####

# Combine Four DF's
fe.normal.plot.df = rbind(earthquakefit.sectoral.normal.out, earthquakefit.sectoral.fe.out,earthquakefit.tax.normal.tax.out, earthquakefit.tax.fe.out)


##################
###### PLOTS
##################


ggplot(data = fe.normal.plot.df, aes(x = variable, y = mean, colour = `Individual Fixed Effects`)) + 
        geom_hline(yintercept = 0, col = "red", alpha= .5, linetype="dashed") +
        geom_pointrange(aes(ymin = lo, ymax = hi), alpha= .5) + 
        xlab("") + 
        ylab("Predicted Death-Toll") + 
        theme_bw() + 
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="bottom") 




ggplot(data = N.fixed.effects.plot.df, aes(x = variable, y = mean, colour = Model)) + 
        geom_hline(yintercept = 0, col = "red", alpha= .5, linetype="dashed") +
        geom_pointrange(aes(ymin = lo, ymax = hi), alpha= .5) + 
        xlab("Observation") + 
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



