rm(list = ls())


#############
#  Purpose  #
#############

# Pooling results of MI models in 6000


##############
#            #
#    Notes   #
#            #
##############

# Emily Long developed script for PaLS 
# Mark McCann modified for STASH


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

rubin.pools <- function(input.results = NULL, schoolnum = NULL) {
  
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




#########################
#                       #
#  Main body of script  #
#                       #
#########################



###Fudge for the temp data to keep the school ID that the pooling code uses
for (i in 1:10){
  baseline.imp10.sch1[[i]][[1]] <- baseline.imp10.sch1[[i]]
}

baseline.pooled.sch1 <- rubin.pools(input.results = baseline.imp10.sch1, schoolnum = 1)

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


