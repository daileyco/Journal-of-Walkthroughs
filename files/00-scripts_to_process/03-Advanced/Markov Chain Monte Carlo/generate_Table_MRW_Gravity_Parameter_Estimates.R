# script to summarize Metropolis random walk results for gravity model parameter estimates

## load data
load("./01-Data/02-Analytic-Data/chains_list.rds")
load("./01-Data/02-Analytic-Data/chains_list_mc11k_mprnorm.rds")


## packages
library(dplyr)
library(tidyr)
library(coda)


## run descriptors
data.subsets <- expand.grid(period = c("2011-2020", "2011-2015", "2016-2020"), 
                            region = c("US", "Midwest", "Northeast", "South", "West"))






## loop through files

for(ii in 1:length(chains.list)){
  
  ## isolate data
  chains.df <- chains.list[[ii]]
  
  
  assign(paste0("param.ests", ii), 
         chains.df %>%
           select(1:11) %>%
           summarise(across(1:11, 
                            list(mean = ~mean(.x), 
                                 lb = ~quantile(.x, probs = 0.025),  
                                 ub = ~quantile(.x, probs = 0.975), 
                                 ess = ~effectiveSize(mcmc(.x))))) %>%
           pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>%
           mutate(type = sub("^.+[_](.+)$", "\\1", parameter), 
                  parameter = sub("^(.+)[_].+$", "\\1", parameter)) %>%
           pivot_wider(names_from = type, values_from = estimate) %>% 
           mutate(estimate = paste0(round(mean,3), " (", round(lb,3), ", ", round(ub,3), ")"), 
                  ess = round(ess,1)) %>% 
           select(parameter, estimate, ess) %>% 
           # pivot_wider(names_from = parameter, values_from = estimate) %>%
           mutate(period = data.subsets$period[ii], 
                  region = data.subsets$region[ii])
  )
  
}


param.ests <- bind_rows(mget(ls(pattern = "param.ests"))) %>%
  select(region, period, everything())



param.meanscis <- param.ests %>% 
  select(-ess) %>%
  pivot_wider(names_from = parameter, values_from = estimate) %>%
  arrange(region, period)

param.means <- param.ests %>% 
  select(-ess) %>%
  mutate(estimate = trimws(sub("[(].+[)]", "", estimate))) %>%
  pivot_wider(names_from = parameter, values_from = estimate) %>%
  arrange(region, period)
  
param.ess <- param.ests %>% 
  select(-estimate) %>%
  pivot_wider(names_from = parameter, values_from = ess) %>%
  arrange(region, period)






