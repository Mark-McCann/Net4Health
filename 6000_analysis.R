rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 6000 Ergms
# Mark McCann modified to loop over imputations


#############
#  Purpose  #
#############

# Running ergms across imp samples

##############
#            #
#    Notes   #
#            #
##############

# 

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################


#########################
#                       #
#    Load packages      #
#                       #
#########################

require(parallel)
library(network)
library(ergm)
library(mice)

#########################
#                       #
#     Load functions    #
#                       #
#########################

run.imputed.ergms <- function(dataset = NULL, savename = NULL, num_iters = NA) {
  
  #If num_iters is specified, use that number of iterations.
  # Otherwise, use the whole length of the file
  leng <- ifelse(is.na(num_iters) ,  length(dataset), num_iters ) 
  
  for (j in 1:leng) {
    df1 <- dataset[[j]]

    for (i in 1:6) {
      df <- df1[[i]]
      
      school.result.list[[i]] <- ergm(df ~ edges 
                                      + mutual
                                      + gwesp(0.25, fixed = T)
                                      + nodematch("gender")
                                      + nodefactor("gender")
                                      + nodematch("sex.var", diff = T)
                                      + nodefactor("sex.var")
                                      + absdiff("know.var")
                                      + absdiff("att.var")
                                      + absdiff("conf.var")
                                      ,
                                      directed=T, 
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"),MCMC.samplesize=2000,MCMC.interval=1024, 
                                                           MCMLE.termination = "Hummel",MCMLE.last.boost = 4, 
                                                           force.main=F , seed = 274,
                                                           parallel=11, parallel.type="PSOCK")
                                      #                                     eval.loglik = F ###This cuts computation time but doesnt update screen
      )
    }
    imputation.list[[j]] <- school.result.list
    print(j)
    write.table(1, file = paste0("completed iteration",j,"so far.txt"))
    save(imputation.list, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}

#########################
#                       #
#  Main body of script  #
#                       #
#########################

#####Run these before calling the run.imputed.ergms function     
imputation.list <- list()
school.result.list <- list()


baseline.imp.results <- run.imputed.ergms(dataset = ergm.data.baseline.imputed, 
                                          savename = "baseline",
                                          num_iters = NA)

