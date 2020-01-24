rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 6500 improve convergence

# Mark McCann developed this script


#############
#  Purpose  #
#############


# Taking prelim models from 6000_control_paper_ERGMs_with_missing

# Currently the temp iteration files from these runs

# And restarting the ERGMs with higher burn in 

# SO far, convergence has been really good using this approach

# Tried reducing the interval to 30k to save time.
#   Mostly fine, but a few will need rerun to fix trending on
#       e.g. Gender 3 coefs

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

# Update to run on the final set of imputations

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

#mcmc.diagnostics(imputation.list[[1]][[2]])
#Try longer burnin at 2e+05
##Looked better with longer burnin but still some autocor
# Push up the interval higher to 100000
#  This fit looks perfect - ramp up to cover all iters

## Pushed this down to 10,000 to try and save time
#     Was good but no perfect. try 30,000 instead

improved.imputation.list <- list()

improve.convergence <- function(dataset = NULL, schoolnum = NULL, num_iters = NULL, savename= NULL){
  
  leng <- ifelse(is.na(num_iters) ,  length(dataset), num_iters ) 
  
  for (imp in 1:leng){
    
    dataset <- imputation.list[[imp]][[schoolnum]]
    tmp <-enformulate.curved(dataset)
    gest2<-try(ergm(tmp$formula,
                    constraints=~bd(maxout=6),
                    control=control.ergm(init=tmp$theta, MCMC.burnin=2e+05,
                                         MCMC.interval=30000,   # default 1024 
                                         MCMLE.maxit=30,      # default 20
                                         parallel=11, parallel.type="PSOCK"),
                    eval.loglik = F ###This cuts computation time but doesnt update screen
    ))
    
    improved.imputation.list[[imp]] <- gest2
    save(improved.imputation.list, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/improved.imputation_imp_",imp,"_schoolnum_",schoolnum,"_",savename,".rdata"))
    
  }
  
  return(improved.imputation.list)
}

#########################
#                       #
#  Main body of script  #
#                       #
#########################

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/")
dir()

load("ergm_imp22_baseline.rdata")

#Have done 12 already #baseline.improved.sch1 <- improve.convergence(dataset = imputation.list, schoolnum = 1, num_iters = 20, savename = "baseline")
baseline.improved.sch2 <- improve.convergence(dataset = imputation.list, schoolnum = 2, num_iters = 10, savename = "baseline")
baseline.improved.sch3 <- improve.convergence(dataset = imputation.list, schoolnum = 3, num_iters = 10, savename = "baseline")
baseline.improved.sch4 <- improve.convergence(dataset = imputation.list, schoolnum = 4, num_iters = 10, savename = "baseline")
baseline.improved.sch5 <- improve.convergence(dataset = imputation.list, schoolnum = 5, num_iters = 10, savename = "baseline")
baseline.improved.sch6 <- improve.convergence(dataset = imputation.list, schoolnum = 6, num_iters = 10, savename = "baseline")

load("ergm_imp19_control.rdata")

control.improved.sch1 <- improve.convergence(dataset = imputation.list, schoolnum = 1, num_iters = 10, savename = "control")
control.improved.sch2 <- improve.convergence(dataset = imputation.list, schoolnum = 2, num_iters = 10, savename = "control")
control.improved.sch3 <- improve.convergence(dataset = imputation.list, schoolnum = 3, num_iters = 10, savename = "control")
control.improved.sch4 <- improve.convergence(dataset = imputation.list, schoolnum = 4, num_iters = 10, savename = "control")
control.improved.sch5 <- improve.convergence(dataset = imputation.list, schoolnum = 5, num_iters = 10, savename = "control")
control.improved.sch6 <- improve.convergence(dataset = imputation.list, schoolnum = 6, num_iters = 10, savename = "control")


###All 10 imps are completed. Delete the previous iteration temp files
for (i in 1:9){
  for (j in 1:6){
    for (k in c("control","baseline")){
    unlink(paste0("improved.imputation_imp_",i,"_schoolnum_",j,"_",k,".rdata"))
    }
  }
  }

load("improved.imputation_imp_10_schoolnum_1_control.rdata")
mcmc.diagnostics(improved.imputation.list[[1]])
mcmc.diagnostics(improved.imputation.list[[10]])

####still some trending in Nodefactor gender 3. 
#    Feed these results through the improve.convergence routine
dir()

###Naming convention on the save file changed mid-runs
#     This is school 1 baseline

load("improved.imputation_10_baseline.rdata")

mcmc.diagnostics(improved.imputation.list[[1]])



load("improved.imputation_imp_10_schoolnum_2_baseline.rdata")
mcmc.diagnostics(improved.imputation.list[[8]])
mcmc.diagnostics(improved.imputation.list[[10]])

dir()
