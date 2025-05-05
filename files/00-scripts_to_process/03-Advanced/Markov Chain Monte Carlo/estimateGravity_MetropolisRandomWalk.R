# Use a Metropolis random walk to estimate gravity model parameters
## script adapted from version authored by Kevin Dobbin 
## from Markov Models and Bayesian Computation course
### errors are mine alone


## load data
load("./01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)


## helper functions
source("./02-Scripts/02-Helper-Functions/calculateGravity.R")
source("./02-Scripts/02-Helper-Functions/RMSE.R")






## objective function for Metropolis random walk
### extracts log of commuter flow volume
### estimates commuter flow volume with gravity model given parameters
### calculates root mean square error between observed and estimate values

calibrateGravity <- function(params, oddf){
  
  require(dplyr)
  
  
  flow.obs <- oddf %>% 
    select(log.Workers.in.Commuting.Flow) %>%
    unlist()
  
  
  flow.preds.gravity <- calculateGravity(params=params, oddf=oddf) %>%
    select(gravity.flow.log) %>%
    unlist()
  
  
  RMSE(obs = flow.obs,
       preds = flow.preds.gravity) %>%
    return()
  
}







## Posterior function
### calculates exponential likelihood with pre-specified tau parameter and RMSE
#### tau parameter corresponds to exponential decay rate, needs tuning
mylikelihoodTimesPrior <- function(params, oddf){
  thelikelihood <- exp(-(tau/2)*calibrateGravity(params=params, oddf=oddf));
  theprior <- 1;
  myreturn <- thelikelihood * theprior;
  myreturn
}



## Metropolis Random Walk
### Random seed set for reproducibility
### tau parameter tuned so that acceptance rate =~20-30%
### 10000 Monte Carlo chosen to ensure convergence



### tuning seed
#### as.numeric(Sys.time())
#### 1693781865

### run seed
#### as.numeric(Sys.Date())
##### 13 September 2023
##### 19613

set.seed(19614)
mcruns <- 10000


### tau tuning
tau <- 1.5

#### showing profile may help someone smarter than me to choose tau
##### x values set to approximate scale for RMSE after 100 MC
##### curve(exp(-(tau/2)*x), from = 1, to = 20) 

#### tuned with 100 mc runs
##### acceptance proportion
###### mean(chains.df$accept, na.rm=T)
##### tau = 10, accept prop = 0.09
##### tau = 100, fails
##### tau = 1, accept prop = 0.49
##### tau = 5, accept prop = 0.09
##### tau = 3, accept prop = 0.12
##### tau = 2, accept prop = 0.12
##### tau = 1.5, accept prop = 0.24***chosen
##### tau = 1.5, 2000 MC, accept prop = 0.17***lower to 1.25
##### tau = 1.25, 2000 MC, accept prop = 0.14***lower to 1
##### tau = 1, 2000 MC, accept prop = 0.14***leave at 1, trace plots wandering, adjusted proposal distributions
##### tau = 1, 2000 MC, accept prop = 0.11***leave at 1, trace plots wandering, adjusted proposal distributions





### set up dataframe to hold chain values

param.names <- c(paste0("beta", 0:13), 
                 "dt", 
                 "Pt", 
                 "accept", 
                 "alpha", 
                 "rmse")

chains.df <- matrix(c(NA), nrow = mcruns, ncol = length(param.names)) %>% 
  as.data.frame() %>% 
  setNames(., nm = param.names)



#### input initial values
##### b0-b3 are set to mirror basic gravity, no power
##### b4-b13 are set to zero
##### distance threshold arbitrarily chosen
##### population threshold arbitrarily chosen, set at 75th percentile
chains.df[1,] <- c(1,
                   1,1,
                   -1,
                   rep(0,10),
                   log(100), 
                   quantile(od$log.POPESTIMATE.destination[which(!duplicated(od[,1:3]))], 
                            probs = 0.75), 
                   NA, 
                   NA, 
                   NA)


chains.df$rmse[1] <- calibrateGravity(params = chains.df[1,1:16], oddf = od)




### MCMC

#### track run time and progress
start.time <- Sys.time()
mcmc.progress <- txtProgressBar(min = 0, 
                                max = mcruns, 
                                initial = 1, 
                                style = 3)


for(mc in 2:mcruns){
  
  setTxtProgressBar(mcmc.progress, 
                    value = mc)
  
  #### extract previous mc parameter values
  current.params = unlist(chains.df[mc-1,1:16])
  
  #### proposals 
  
  
  ##### for b0-b13 generated from
  ###### try 1-3
  ###### moving proposal kernel
  ####### normal distributions centered at previous values, standard deviations all 0.5
  ###### try 4
  ###### static proposal kernel
  ####### normal distributions centered at zero, standard deviations all 1
  
  ###### try 5
  ###### static proposal kernel
  ####### b0-b3 generated from
  ####### normal distributions centered at zero, standard deviations all 1
  ####### b4-b13 generated from
  ####### normal distributions centered at zero, standard deviations all 0.5
  
  
  
  
  
  ##### for distance threshold generated from 
  ###### uniform distribution with minimum and maximum values corresponding to 5th and 95th percentiles of observed distances, respectively, arbitrary
  ##### for population threshold generated from 
  ###### uniform distribution with minimum and maximum values corresponding to 50th and 97.5th percentiles of observed population sizes, respectively, arbitrary
  prop.params = c(
                  sapply(unlist(chains.df[mc-1,1:4]),
                         function(prev.value){
                           rnorm(n=1, mean=prev.value, sd=0.2)
                         }),
                  sapply(unlist(chains.df[mc-1,5:14]),
                         function(prev.value){
                           rnorm(n=1, mean=prev.value, sd=0.05)
                         }),
                  # rnorm(n=4, mean=0, sd=1), #b0-b3
                  # rnorm(n=2, mean=0, sd=0.5), #b4-b5 internal
                  # rnorm(n=4, mean=0, sd=0.5), #b6-b9 long dist
                  # rnorm(n=4, mean=0, sd=0.5), #b10-b14 large pop
                  runif(1, 
                        min = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                       probs = 0.05), 
                        max = quantile(od$log.distance.km[which(od$log.distance.km!=0)], 
                                       probs = 0.95)), 
                  runif(1, 
                        min = quantile(od$log.POPESTIMATE.destination[which(!duplicated(od[,1:3]))], 
                                       probs = 0.5), 
                        max = quantile(od$log.POPESTIMATE.destination[which(!duplicated(od[,1:3]))], 
                                       probs = 0.975)))
  
  #### feed to objective function
  alphanum = mylikelihoodTimesPrior(params = prop.params, oddf = od)
  alphaden = mylikelihoodTimesPrior(params = current.params, oddf = od)
  
  
  #### calculate alpha for acceptance choice
  ##### alpha > 1 corresponds to better fit for proposal, proposal always accepted
  ##### alpha < 1 corresponds to worse fit for proposal, proposal *sometimes* accepted
  ###### random based on random variable ~ uniform(0,1)
  alpha = min(1, alphanum/alphaden)
  
  chains.df[mc, 18] = alpha
  myunif = runif(1)
  
  #### store values based on acceptance choice
  if(myunif < alpha){
    chains.df[mc, 17] = 1
    
    chains.df[mc, 1:16] = prop.params
    
    chains.df[mc, 19] = calibrateGravity(params = prop.params, oddf = od)
  }
  if(myunif > alpha){
    chains.df[mc, 17] = 0
    
    chains.df[mc, 1:16] = current.params
    
    chains.df[mc, 19] = calibrateGravity(params = current.params, oddf = od)
  }
}

#### stop tracking run time and progress
close(mcmc.progress)
end.time <- Sys.time()

end.time-start.time
##### 2.3 min for 100 MC
##### 40.6 min for 2000 MC


## save
save(chains.df, file = "./01-Data/02-Analytic-Data/mrw.rds")


## clean environment
rm(list=ls())
gc()







