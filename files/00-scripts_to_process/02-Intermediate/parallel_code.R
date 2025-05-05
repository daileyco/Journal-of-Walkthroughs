
x <- runif(100)
y <- runif(100)

library(foreach)
library(doParallel)
cl <- makeCluster(mc <- getOption("cl.cores", 8))

clusterExport(cl=cl, varlist=c("x", "y"))
# clusterEvalQ(cl, {})

registerDoParallel(cl)

ai.list <- parLapply(cl,
                     1:100,
                     function(data.index){
                       for(i in 1:10){
                        Sys.sleep(1)  
                         }
                       z <- x * y
                       save(z, file = paste0("./ex", data.index, ".rds"))
                     })

stopCluster(cl)