

getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

packages <- getPackages(c("ergm", "dplyr", "BH"))


packages <- getPackages(c( "BH"))

packages <- getPackages(c( "Rmpi", "snow"))

packages <- getPackages(c("readRDS"))

packages <- getPackages(c("mice"))

packages <- getPackages(c("metafor"))


dr <- "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/"

download.packages(packages, destdir= dr, 
                  type="source")







improved.imputation.list <- list()

improve.convergence <- function(dataset = NULL, schoolnum = NULL, start_iters = 1, end_iters = NULL, savename= NULL){
  
  leng <- ifelse(is.na(end_iters) ,  length(dataset), end_iters ) 
  
  for (imp in start_iters:leng){
    
    dataset <- imputation.list[[imp]][[schoolnum]]
    tmp <-enformulate.curved(dataset)
    gest2<-try(ergm(tmp$formula,
                    constraints=~bd(maxout=6),       
                    control=control.ergm(init=tmp$theta, MCMC.burnin=2e+05,seed = 162,
                                         MCMC.interval=30000,   # default 1024 
                                         MCMLE.maxit=30,      # default 20
                                         parallel=16, parallel.type="MPI"),    
                    eval.loglik = F ###This cuts computation time but doesn't update screen
    )
    improved.imputation.list[[imp]] <- gest2 
    print(imp) 
    if (imp > 9)  save(improved.imputation.list, file = paste0("improved.imputation_imp_",imp,"_schoolnum_",schoolnum,"_",savename,".rdata"))
  }
  return(improved.imputation.list)
}


