rm(list = ls())
##Temp lister of iteration nums while models finish running

####Not enough memory to load all of these files on desktop machine

###Test code with 4 iters then move to bigger machine to run

iternums <- 1:10
iternums <- 1:20

iternums <- 1:4

#################
#               #
#      Name     #
#               #
#################

# 7000 rubins pools

# Emily Long developed script for PaLS 

# Mark McCann modified for STASH


#############
#  Purpose  #
#############


# Pooling results of MI models in 6000

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

####Update rubins.pools function for 6100 model coefs



#########################
#                       #
#    Load packages      #
#                       #
#########################

library(ergm)
library(mice)

#########################
#                       #
#     Load functions    #
#                       #
#########################
####################################### 
#   Pool results using Rubin's rules  #
#######################################

pool.6500 <- function(input.results = NULL, schoolnum = NULL) {
  
  leng <- length(input.results)
  
  #summary(input.results[[1]][[1]]) 
  
  mice.coef.table <- matrix(0, nc = leng + 1, nr = 14)
  mice.se.table <- mice.coef.table
  
  mice.pooled.est     <- data.frame(matrix(NA, nr = 14, nc = 7))
  colnames(mice.pooled.est) <- c("variable","pool.est","pool.se","btw.var", "est/se","l.cl","u.cl")
  mice.pooled.est[1,1] <- "edges"
  mice.pooled.est[2,1] <- "mutual"
  mice.pooled.est[3,1] <- "gwesp"
  mice.pooled.est[4,1] <- "match.gender"
  mice.pooled.est[5,1] <- "nodefac.gen2"
  mice.pooled.est[6,1] <- "nodefac.gen3"
  mice.pooled.est[7,1] <- "nodematch.sex1"
  mice.pooled.est[8,1] <- "nodematch.sex2"
  mice.pooled.est[9,1] <- "nodematch.sex3"
  mice.pooled.est[10,1] <- "nodefac.sex2"
  mice.pooled.est[11,1] <- "nodefac.sex3"
  mice.pooled.est[12,1] <- "diff.know"
  mice.pooled.est[13,1] <- "diff.att"
  mice.pooled.est[14,1] <- "diff.conf"
  
  
  for (i in 1:leng) {
    col.id <- i + 1
    mice.coef.table[1,col.id] <- as.numeric(input.results[[i]][[schoolnum]]$coef['edges'] )
    mice.coef.table[2,col.id] <- input.results[[i]][[schoolnum]]$coef['mutual']
    mice.coef.table[3,col.id] <- input.results[[i]][[schoolnum]]$coef['gwesp.fixed.0.25']
    mice.coef.table[4,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.gender']
    mice.coef.table[5,col.id] <- input.results[[i]][[schoolnum]]$coef['nodefactor.gender.2']
    mice.coef.table[6,col.id] <- input.results[[i]][[schoolnum]]$coef['nodefactor.gender.3']
    mice.coef.table[7,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.1']
    mice.coef.table[8,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.2']
    mice.coef.table[9,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.3']
    mice.coef.table[10,col.id] <- input.results[[i]][[schoolnum]]$coef['nodefactor.sex.var.2']
    mice.coef.table[11,col.id] <- input.results[[i]][[schoolnum]]$coef['nodefactor.sex.var.3']
    mice.coef.table[12,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.know.var']
    mice.coef.table[13,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.att.var']
    mice.coef.table[14,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.conf.var']
    
    mice.se.table[1,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['edges']
    mice.se.table[2,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['mutual']
    mice.se.table[3,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['gwesp.fixed.0.25']
    mice.se.table[4,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.gender']
    mice.se.table[5,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodefactor.gender.2']
    mice.se.table[6,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodefactor.gender.3']
    mice.se.table[7,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.1']
    mice.se.table[8,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.2']
    mice.se.table[9,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.3']
    mice.se.table[10,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodefactor.sex.var.2']
    mice.se.table[11,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodefactor.sex.var.3']
    mice.se.table[12,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.know.var']
    mice.se.table[13,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.att.var']
    mice.se.table[14,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.conf.var']
    
  }
  
  for (i in 1:14) {
    pooled.res <- pool.scalar(mice.coef.table[i,2:(leng + 1)], mice.se.table[i,2:(leng + 1)],  k = 1) 
    
    
    mice.pooled.est[i,2] <-  round(pooled.res$qbar,2)
    mice.pooled.est[i,3] <-  round(pooled.res$ubar,2)
    mice.pooled.est[i,4] <-  round(pooled.res$b,2)
    mice.pooled.est[i,5] <-  round(pooled.res$qbar / pooled.res$ubar , 2)
    mice.pooled.est[i,6] <-  round(pooled.res$qbar - (1.96 * pooled.res$ubar),2)
    mice.pooled.est[i,7] <-  round(pooled.res$qbar + (1.96 * pooled.res$ubar),2)  
    
  }
  
  mice.array <- list(mice.coef.table, mice.se.table, mice.pooled.est)
  
  return(mice.array)
}


pool.6100 <- function(input.results = NULL, schoolnum = NULL) {
  leng <- length(input.results)
  #Two tables for coefs and se
  mice.coef.table <- matrix(0, nc = leng + 1, nr = length(input.results[[1]][[1]]$coef))
  mice.se.table <- mice.coef.table
  #Table for results of rubin's rules pool
  mice.pooled.est     <- data.frame(matrix(NA, nr = length(input.results[[1]][[1]]$coef), nc = 7))
  colnames(mice.pooled.est) <- c("variable","pool.est","pool.se","btw.var", "est/se","l.cl","u.cl")
  #Put names in the coef table
  for (x in 1:length(input.results[[1]][[1]]$coef) ){
    mice.pooled.est[x,1] <- names(input.results[[1]][[1]]$coef)[x]
  }  

  for (i in 1:leng) {
    col.id <- i + 1
    mice.coef.table[1,col.id] <- input.results[[i]][[schoolnum]]$coef['edges'] 
    mice.coef.table[2,col.id] <- input.results[[i]][[schoolnum]]$coef['mutual']
    mice.coef.table[3,col.id] <- input.results[[i]][[schoolnum]]$coef['gwesp.fixed.0.25']
    mice.coef.table[4,col.id] <- input.results[[i]][[schoolnum]]$coef['idegree1.5']
    mice.coef.table[5,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.gender']
    mice.coef.table[6,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.1']
    mice.coef.table[7,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.2']
    mice.coef.table[8,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.3']
    mice.coef.table[9,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.know.var']
    mice.coef.table[10,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.att.var']
    mice.coef.table[11,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.conf.var']
    mice.coef.table[12,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeifactor.sex.var.2']
    mice.coef.table[13,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeifactor.sex.var.3']
    mice.coef.table[14,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeofactor.sex.var.2']
    mice.coef.table[15,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeofactor.sex.var.2']
    mice.coef.table[16,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.know.var']
    mice.coef.table[17,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.att.var']
    mice.coef.table[18,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.conf.var']
    mice.coef.table[19,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.know.var']
    mice.coef.table[20,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.att.var']
    mice.coef.table[21,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.conf.var']
    
    mice.se.table[1,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['edges']
    mice.se.table[2,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['mutual']
    mice.se.table[3,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['gwesp.fixed.0.25']
    mice.se.table[4,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['idegree1.5']
    mice.se.table[5,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.gender']
    mice.se.table[6,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.1']
    mice.se.table[7,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.2']
    mice.se.table[8,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.3']
    mice.se.table[9,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.know.var']
    mice.se.table[10,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.att.var']
    mice.se.table[11,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.conf.var']
    mice.se.table[12,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeifactor.sex.var.2']
    mice.se.table[13,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeifactor.sex.var.3']
    mice.se.table[14,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeofactor.sex.var.2']
    mice.se.table[15,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeofactor.sex.var.3']
    mice.se.table[16,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.know.var']
    mice.se.table[17,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.att.var']
    mice.se.table[18,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.conf.var']
    mice.se.table[19,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.know.var']
    mice.se.table[20,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.att.var']
    mice.se.table[21,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.conf.var']
  }

  for (i in 1:dim(mice.coef.table)[1]) {
    pooled.res <- pool.scalar(mice.coef.table[i,2:(leng + 1)], mice.se.table[i,2:(leng + 1)],  k = 1) 
    mice.pooled.est[i,2] <-  round(pooled.res$qbar,2)
    mice.pooled.est[i,3] <-  round(pooled.res$ubar,2)
    mice.pooled.est[i,4] <-  round(pooled.res$b,2)
    mice.pooled.est[i,5] <-  round(pooled.res$qbar / pooled.res$ubar , 2)
    mice.pooled.est[i,6] <-  round(pooled.res$qbar - (1.96 * pooled.res$ubar),2)
    mice.pooled.est[i,7] <-  round(pooled.res$qbar + (1.96 * pooled.res$ubar),2)  
    
  }
  
  mice.array <- list(coefs = mice.coef.table, 
                     ses = mice.se.table, 
                     pooled.ests = mice.pooled.est)
  
  return(mice.array)
}




#########################
#                       #
#  Main body of script  #
#                       #
#########################


# Rubin's pools requires an input obect that has the indexing format:
#     object[[imputation]][schoolid]

#setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/")

####Load 6500 models
#setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC")
#dir()
#  for (sch in c(1,2) ){
#    for (iter in c(5,10) ){
#        for (borc in c("baseline")){          
#  load(paste0("improved.imputation_imp_",iter,"_schoolnum_",sch,"_",borc,".rdata") )
#  assign(paste0(borc,".iter",iter,".sch",sch), improved.imputation.list)
#    }
#  }
#}


####Load 6100 models
setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC")

######Check which models haven't completed

for (sch in 1:6 ){
  for (iter in iternums){
    for (borc in c("baseline","control")){          
      if ( !file.exists(paste0("ergm_6100_imp",iter,"_sch_",sch,"_",borc,".rdata") ) ) print(paste0("Missing file iter",iter," sch",sch," ",borc)) 
    }
  }
}


###Create lists to hold results from all models
baseline.6100.models <- list()
control.6100.models <- list()

#Let these hold inner lists to hold iter and school indices
prelist <- list()
prelist[[1]] <- NA

baseline.6100.models[[max(iternums)]] <- prelist
control.6100.models[[max(iternums)]] <- prelist


for (sch in 1:6 ){
  for (iter in iternums ){
    for (borc in c("baseline")){          
      load(paste0("ergm_6100_imp",iter,"_sch_",sch,"_",borc,".rdata") )
      #      assign(paste0("6100.",borc,".iter",iter,".sch",sch), imputation.list)
      baseline.6100.models[[iter]][[sch]] <- imputation.list[[iter]][[sch]]
      print(paste0("Imputation ",iter," School ",sch," loaded successfully"))
    }
  }
}

for (sch in 1:6 ){
  for (iter in iternums ){
    for (borc in c("control")){          
      load(paste0("ergm_6100_imp",iter,"_sch_",sch,"_",borc,".rdata") )
      #      assign(paste0("6100.",borc,".iter",iter,".sch",sch), imputation.list)
      control.6100.models[[iter]][[sch]] <- imputation.list[[iter]][[sch]]
      print(paste0("Imputation ",iter," School ",sch," loaded successfully"))
    }
  }
}

#############################################################################
######           Create pdfs of mcmc diagnostics and gof  #######
#############################################################################

setwd("//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
for (sch in 1){
  pdf(paste0("mcmc_diag_sch",sch,".pdf"))
    for (iter in iternums){    
      mcmc.diagnostics(baseline.6100.models[[iter]][[sch]])
      ideg <- gof(baseline.6100.models[[iter]][[sch]]~idegree)
      plot(ideg)
      odeg <- gof(baseline.6100.models[[iter]][[sch]]~idegree)
      plot(odeg)
      tricen <- gof(baseline.6100.models[[iter]][[sch]]~triadcensus)
      plot(tricen)
    }
  dev.off()
}


###Fudge for the temp data to keep the school ID that the pooling code uses
#for (i in 1:10){
#  baseline.imp10.sch1[[i]][[1]] <- baseline.imp10.sch1[[i]]
#}

#for (i in 1:6){
#  assign(paste0("baseline.pooled.sch",i), pool.6500(input.results = imputation.list.baseline, schoolnum = i))
#  assign(paste0("control.pooled.sch",i), pool.6500(input.results = imputation.list.control, schoolnum = i))
#}


for (i in 1:6){
  assign(paste0("baseline.pooled.6100.sch",i), pool.6100(input.results = baseline.6100.models, schoolnum = i))
  assign(paste0("control.pooled.6100.sch" ,i), pool.6100(input.results = control.6100.models, schoolnum = i))
}



#save(baseline.pooled.sch1, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch1.rdata"))
#save(baseline.pooled.sch2, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch2.rdata"))
#save(baseline.pooled.sch3, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch3.rdata"))
#save(baseline.pooled.sch4, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch4.rdata"))
#save(baseline.pooled.sch5, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch5.rdata"))
#save(baseline.pooled.sch6, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch6.rdata"))

#save(control.pooled.sch1, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch1.rdata"))
#save(control.pooled.sch2, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch2.rdata"))
#save(control.pooled.sch3, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch3.rdata"))
#save(control.pooled.sch4, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch4.rdata"))
#save(control.pooled.sch5, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch5.rdata"))
#save(control.pooled.sch6, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch6.rdata"))


#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(baseline.pooled.sch1[[3]]$variable,
                              ##Baseline coefs
                              paste0(baseline.pooled.sch1[[3]]$pool.est," (",
                                     baseline.pooled.sch1[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch1[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.sch2[[3]]$pool.est," (",
                                     baseline.pooled.sch2[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch2[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.sch3[[3]]$pool.est," (",
                                     baseline.pooled.sch3[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch3[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.sch4[[3]]$pool.est," (",
                                     baseline.pooled.sch4[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch4[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.sch5[[3]]$pool.est," (",
                                     baseline.pooled.sch5[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch5[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.sch6[[3]]$pool.est," (",
                                     baseline.pooled.sch6[[3]]$l.cl    ,", ",
                                     baseline.pooled.sch6[[3]]$u.cl    ,")")
                              ,
                              ######Control schools
                              paste0(control.pooled.sch1[[3]]$pool.est," (",
                                     control.pooled.sch1[[3]]$l.cl    ,", ",
                                     control.pooled.sch1[[3]]$u.cl    ,")"),
                              paste0(control.pooled.sch2[[3]]$pool.est," (",
                                     control.pooled.sch2[[3]]$l.cl    ,", ",
                                     control.pooled.sch2[[3]]$u.cl    ,")"),
                              paste0(control.pooled.sch3[[3]]$pool.est," (",
                                     control.pooled.sch3[[3]]$l.cl    ,", ",
                                     control.pooled.sch3[[3]]$u.cl    ,")"),
                              paste0(control.pooled.sch4[[3]]$pool.est," (",
                                     control.pooled.sch4[[3]]$l.cl    ,", ",
                                     control.pooled.sch4[[3]]$u.cl    ,")"),
                              paste0(control.pooled.sch5[[3]]$pool.est," (",
                                     control.pooled.sch5[[3]]$l.cl    ,", ",
                                     control.pooled.sch5[[3]]$u.cl    ,")"),
                              paste0(control.pooled.sch6[[3]]$pool.est," (",
                                     control.pooled.sch6[[3]]$l.cl    ,", ",
                                     control.pooled.sch6[[3]]$u.cl    ,")")
                              
                              
)

colnames(pooled.results.table) <- c("Parameter","School 1b","School 2b","School 3b",
                                    "School 4b", "School 5b", "School 6b",
                                    "School 1c","School 2c","School 3c",
                                    "School 4c", "School 5c", "School 6c")


pooled.results.table


write.csv(pooled.results.table, file = "//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper/pooled estimates_temporary2.csv")



#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(baseline.pooled.6100.sch1[[3]]$variable,
                              ######Control schools
                              paste0(control.pooled.6100.sch1[[3]]$pool.est," (",
                                     control.pooled.6100.sch1[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch1[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6100.sch2[[3]]$pool.est," (",
                                     control.pooled.6100.sch2[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch2[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6100.sch3[[3]]$pool.est," (",
                                     control.pooled.6100.sch3[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch3[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6100.sch4[[3]]$pool.est," (",
                                     control.pooled.6100.sch4[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch4[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6100.sch5[[3]]$pool.est," (",
                                     control.pooled.6100.sch5[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch5[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6100.sch6[[3]]$pool.est," (",
                                     control.pooled.6100.sch6[[3]]$l.cl    ,", ",
                                     control.pooled.6100.sch6[[3]]$u.cl    ,")")
,
##Baseline coefs
                              paste0(baseline.pooled.6100.sch1[[3]]$pool.est," (",
                                    baseline.pooled.6100.sch1[[3]]$l.cl    ,", ",
                                    baseline.pooled.6100.sch1[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6100.sch2[[3]]$pool.est," (",
       baseline.pooled.6100.sch2[[3]]$l.cl    ,", ",
       baseline.pooled.6100.sch2[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6100.sch3[[3]]$pool.est," (",
       baseline.pooled.6100.sch3[[3]]$l.cl    ,", ",
       baseline.pooled.6100.sch3[[3]]$u.cl    ,")"),
                             paste0(baseline.pooled.6100.sch4[[3]]$pool.est," (",
       baseline.pooled.6100.sch4[[3]]$l.cl    ,", ",
       baseline.pooled.6100.sch4[[3]]$u.cl    ,")"),
paste0(baseline.pooled.6100.sch5[[3]]$pool.est," (",
       baseline.pooled.6100.sch5[[3]]$l.cl    ,", ",
       baseline.pooled.6100.sch5[[3]]$u.cl    ,")"),
paste0(baseline.pooled.6100.sch6[[3]]$pool.est," (",
       baseline.pooled.6100.sch6[[3]]$l.cl    ,", ",
       baseline.pooled.6100.sch6[[3]]$u.cl    ,")")

                              
)

colnames(pooled.results.table) <- c("Parameter","School 1b","School 2b","School 3b",
                                    "School 4b", "School 5b", "School 6b",
                                    "School 1c","School 2c","School 3c",
                                    "School 4c", "School 5c", "School 6c")

#View(pooled.results.table)
write.csv(pooled.results.table, file = "//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper/pooled estimates_6100_temporary_fourimps.csv")


for (sch in 1:6){
  save(list = paste0("baseline.pooled.6100.sch",sch), file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6100_sch",sch,".rdata") )
  save(list = paste0("control.pooled.6100.sch",sch), file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6100_sch",sch,".rdata") )
}



