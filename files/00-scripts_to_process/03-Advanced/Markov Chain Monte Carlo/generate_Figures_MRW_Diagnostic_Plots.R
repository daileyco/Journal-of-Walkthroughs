# script to generate figures for diagnostics of Metropolis random walk

## load data
load("./01-Data/02-Analytic-Data/chains_list.rds")
load("./01-Data/02-Analytic-Data/chains_list_gibbs_mc1k.rds")


## packages
library(dplyr)
library(coda)
library(foreach)
library(doParallel)


## run descriptors
data.subsets <- expand.grid(period = c("2011-2020", "2011-2015", "2016-2020"), 
                            region = c("US", "Midwest", "Northeast", "South", "West")) %>%
  mutate(id = paste0(region, "_", period))




## set up parallel computation
cl <- makeCluster(mc <- getOption("cl.cores", 15))

clusterExport(cl=cl, 
              varlist=c("data.subsets"))
clusterEvalQ(cl, 
             {
               library(dplyr)
             })


registerDoParallel(cl)


foreach(chains.df=iter(chains.list), 
        ii = 1:length(chains.list)) %dopar% {
          
          ## Trace plots
          
          ### RMSE against MCMC run index
          
          # svg(filename = paste0("./03-Output/02-Figures/trace_rmse_", data.subsets$id[ii],".svg"), width = 8, height = 18, pointsize = 10)
          png(filename = paste0("./03-Output/02-Figures/trace_rmse_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 32, pointsize = 10)
          
          var.names <- names(chains.df)[grep("objective", names(chains.df))]
          block <- sub("objective[_]*", "", var.names)
          nplots <- length(var.names)
          par(mfrow=c(nplots,1), mar=c(5.1,4.1,4.1,2.1))
          
          for(i in 1:nplots){
            plot(chains.df[,var.names[i]], type = "l", 
                 ylab="objective", 
                 xlab="MC Index",
                 main=paste0("Root Mean Square Error Trace", ifelse(block[i]=="", "", paste0("\nBlock ", block[i]))))
            points(y=chains.df[,var.names[i]][which(chains.df[,var.names[i]]==chains.df[,var.names[i]][which.min(chains.df[,var.names[i]])])], 
                   x=which(chains.df[,var.names[i]]==chains.df[,var.names[i]][which.min(chains.df[,var.names[i]])]), 
                   pch = 16, 
                   col = "red")
            legend("topright", legend=c(paste0("Minimum = ", round(chains.df[,var.names[i]][which.min(chains.df[,var.names[i]])],2))), col=c("red"), pch=16)
            
          }
          
          dev.off()
          
          
          
          
          ### Parameter estimates against MCMC run index
          
          
          # svg(filename = paste0("./03-Output/02-Figures/trace_parameter_estimates_", data.subsets$id[ii],".svg"), width = 16, height = 16, pointsize = 10)
          png(filename = paste0("./03-Output/02-Figures/trace_parameter_estimates_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
          
          par(mfrow=c(4,3), mar=c(2.1,2.1,2.1,1.1))
          
          for(i in 1:11){
            plot(unlist(chains.df[,i]), type = "l", main = names(chains.df)[i])
          }
          
          dev.off()
          
          
          
          ### Alpha and acceptance
          
          
          # svg(filename = paste0("./03-Output/02-Figures/trace_alpha_", data.subsets$id[ii],".svg"), width = 8, height = 4.5, pointsize = 10)
          png(filename = paste0("./03-Output/02-Figures/trace_alpha_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 32, pointsize = 10)
          
          var.names <- names(chains.df)[grep("alpha[^0-1]", names(chains.df))]
          var.names2 <- names(chains.df)[grep("accept", names(chains.df))]
          block <- sub("alpha[_]*", "", var.names)
          nplots <- length(var.names)
          par(mfrow=c(nplots,1), mar=c(5.1,4.1,4.1,2.1))
          
          for(i in 1:nplots){
            plot(chains.df[,var.names[i]], type = "l", 
                 ylab="Alpha", 
                 xlab="MC Index",
                 main=paste0("Alpha Trace", 
                             ifelse(block[i]=="", "", paste0("\nBlock ", block[i])), 
                             paste0("\nAcceptance = ", round(sum(chains.df[,var.names2[i]][-1])/nrow(chains.df)*100,1), "%")))
          }
          
          dev.off()
          
          
          
          
          
          
          ## Histograms
          
          
          # svg(filename = paste0("./03-Output/02-Figures/histograms_parameter_estimates_", data.subsets$id[ii],".svg"), width = 16, height = 16, pointsize = 10)
          png(filename = paste0("./03-Output/02-Figures/histograms_parameter_estimates_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
          
          par(mfrow=c(4,3), mar=c(2.1,2.1,2.1,0))
          
          for(i in 1:11){
            hist(unlist(chains.df[,i]), main = names(chains.df)[i])
          }
          
          
          dev.off()
          
          
          
          
          ## Running Means
          
          # svg(filename = paste0("./03-Output/02-Figures/runningmeans_parameter_estimates_", data.subsets$id[ii],".svg"), width = 16, height = 16, pointsize = 10)
          png(filename = paste0("./03-Output/02-Figures/runningmeans_parameter_estimates_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
          
          par(mfrow=c(4,3), mar=c(2.1,2.1,2.1,2.1))
          
          for(i in 1:11){
            this.running.mean <- cumsum(unlist(chains.df[,i]))/1:nrow(chains.df)
            
            plot(this.running.mean, type="l", main = names(chains.df)[i])
          }
          
          dev.off()
          
          
          
          ## Autocorrelation
          
          
          # svg(filename = paste0("./03-Output/02-Figures/autocorrelation_parameter_estimates_", data.subsets$id[ii],".svg"), width = 16, height = 16, pointsize = 10)
          
          png(filename = paste0("./03-Output/02-Figures/autocorrelation_parameter_estimates_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
          
          
          par(mfrow=c(4,3), mar=c(2.1,2.1,3.1,0))
          
          for(i in 1:11){
            acf(unlist(chains.df[,i]), main = "")
            title(main = paste0(names(chains.df)[i], 
                                "\nESS = ", 
                                round(effectiveSize(mcmc(chains.df[,i])),1)), 
                  line = 0.75)
          }
          
          dev.off()
          
          
          ## Geweke-Brooks
          
          
          # svg(filename = paste0("./03-Output/02-Figures/geweke_", data.subsets$id[ii],".svg"), width = 16, height = 16, pointsize = 10)
          
          png(filename = paste0("./03-Output/02-Figures/geweke_", data.subsets$id[ii],".png"), units = "in", res = 300, width = 16, height = 16, pointsize = 10)
          
          
          par(mfrow=c(4,3), mar=c(4.1,4.1,2.1,1.1))
          
          geweke.plot(mcmc(chains.df[,1:11]), auto.layout = FALSE, ask = FALSE)
          
          dev.off()
          
        }

stopCluster(cl)


## clean environment
rm(list=ls())
gc()

