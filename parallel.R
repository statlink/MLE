library(parallel)
library(doParallel)
library(foreach)
      
    cl <- makePSOCKcluster(ncores)
    registerDoParallel(cl)
    ## B is the number of replications of the experiment
    betaboot <- foreach(i = 1:B, .combine = rbind, .packages = c( "dcov", "Rfast2" ),
                       .export=c("Sample.int", "tvreg") ) %dopar% {
        ### do stuff here  
        ida <- Rfast2::Sample.int(n, n, replace = TRUE)
        yb <- y[ida, ]
        xb <- x[ida, ]
        qa <- optim(par, tvreg, y = yb, x = xb, d = d)
        ## finish
        return( qa$estimate )
      }  ##  end foreach
      stopCluster(cl)

  
