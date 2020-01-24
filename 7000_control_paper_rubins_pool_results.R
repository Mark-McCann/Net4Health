rm(list = ls())
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

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/")
dir()

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC")
dir()
  for (sch in c(1,2) ){
    for (iter in c(5,10) ){
        for (borc in c("baseline")){          
  load(paste0("improved.imputation_imp_",iter,"_schoolnum_",sch,"_",borc,".rdata") )
  assign(paste0("baseline.iter",iter,".sch",sch), improved.imputation.list)
    }
  }
}

load("improved.imputation_imp_7_schoolnum_6_control.rdata")
mcmc.diagnostics(improved.imputation.list[[1]])


load("improved.imputation_imp_10_schoolnum_1_baseline.rdata")
for (i in 6:10){
baseline.imp10.sch1[[i]] <- improved.imputation.list[[i]]
}

load("improved.imputation_imp_5_schoolnum_1_baseline.rdata")
baseline.imp10.sch1 <- improved.imputation.list

load("improved.imputation_imp_10_schoolnum_1_baseline.rdata")
for (i in 6:10){
  baseline.imp10.sch1[[i]] <- improved.imputation.list[[i]]
}


###Fudge for the temp data to keep the school ID that the pooling code uses
for (i in 1:10){
  baseline.imp10.sch1[[i]][[1]] <- baseline.imp10.sch1[[i]]
}



baseline.pooled.sch1 <- rubin.pools(input.results = baseline.imp10.sch1, schoolnum = 1)
baseline.pooled.sch2 <- rubin.pools(input.results = imputation.list.baseline, schoolnum = 2)
baseline.pooled.sch3 <- rubin.pools(input.results = imputation.list.baseline, schoolnum = 3)
baseline.pooled.sch4 <- rubin.pools(input.results = imputation.list.baseline, schoolnum = 4)
baseline.pooled.sch5 <- rubin.pools(input.results = imputation.list.baseline, schoolnum = 5)
baseline.pooled.sch6 <- rubin.pools(input.results = imputation.list.baseline, schoolnum = 6)

control.pooled.sch1 <- rubin.pools(input.results = imputation.list.control, schoolnum = 1)
control.pooled.sch2 <- rubin.pools(input.results = imputation.list.control, schoolnum = 2)
control.pooled.sch3 <- rubin.pools(input.results = imputation.list.control, schoolnum = 3)
control.pooled.sch4 <- rubin.pools(input.results = imputation.list.control, schoolnum = 4)
control.pooled.sch5 <- rubin.pools(input.results = imputation.list.control, schoolnum = 5)
control.pooled.sch6 <- rubin.pools(input.results = imputation.list.control, schoolnum = 6)


save(baseline.pooled.sch1, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch1.rdata"))
save(baseline.pooled.sch2, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch2.rdata"))
save(baseline.pooled.sch3, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch3.rdata"))
save(baseline.pooled.sch4, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch4.rdata"))
save(baseline.pooled.sch5, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch5.rdata"))
save(baseline.pooled.sch6, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch6.rdata"))

save(control.pooled.sch1, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch1.rdata"))
save(control.pooled.sch2, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch2.rdata"))
save(control.pooled.sch3, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch3.rdata"))
save(control.pooled.sch4, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch4.rdata"))
save(control.pooled.sch5, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch5.rdata"))
save(control.pooled.sch6, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch6.rdata"))



str(baseline.pooled.sch1[[3]])

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








load("ergm_imp22_baseline.rdata")
imputation.list.baseline <- imputation.list

load("ergm_imp20_control.rdata")
imputation.list.control <- imputation.list

#load("ergm_imp3_baseline.rdata")
#imputation.list.baseline[[1]] <- imputation.list[[1]]
#imputation.list.baseline[[2]] <- imputation.list[[2]]
#imputation.list.baseline[[3]] <- imputation.list[[3]]

#imputation.list <- imputation.list.baseline
#save(imputation.list, file = "ergm_imp22_baseline.rdata")

#length(imputation.list)

summary(imputation.list[[1]][[1]])
#summary(imputation.list[[2]][[1]])
#summary(imputation.list[[3]][[1]])
summary(imputation.list[[4]][[1]])
summary(imputation.list[[5]][[1]])
summary(imputation.list[[6]][[1]])
summary(imputation.list[[7]][[1]])
summary(imputation.list[[8]][[1]])
summary(imputation.list[[9]][[1]])
summary(imputation.list[[15]][[1]])
#summary(imputation.list[[10]][[1]])




###coefs 
baseline.pooled[[1]]
##Se's
baseline.pooled[[2]]
#Results
baseline.pooled[[3]]


control.imp.results[[1]]

mcmc.check <- mcmc.diagnostics(imputation.list[[1]][[1]])
mcmc.check <- mcmc.diagnostics(imputation.list[[1]][[2]])
mcmc.check <- mcmc.diagnostics(imputation.list[[1]][[3]])

mcmc.check <- mcmc.diagnostics(imputation.list[[1]][[4]])
mcmc.check <- mcmc.diagnostics(imputation.list[[2]][[4]])
mcmc.check <- mcmc.diagnostics(imputation.list[[3]][[4]])
mcmc.check <- mcmc.diagnostics(imputation.list[[1]][[5]])
mcmc.check <- mcmc.diagnostics(imputation.list[[3]][[6]])


mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[1]])
#####1 Not mixing well, trending in stats
mcmc.check <- mcmc.diagnostics(imputation.list[[18]][[1]])





mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[2]])
mcmc.check <- mcmc.diagnostics(imputation.list[[18]][[2]])
####2 Doesn't look awful but sig trend in the stats

mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[3]])
####3 Not mixing well

mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[4]])
mcmc.check <- mcmc.diagnostics(imputation.list[[18]][[4]])
####4 Looks good, some stats not perfect

mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[5]])
#####OK but not great

mcmc.check <- mcmc.diagnostics(imputation.list[[4]][[6]])
####Miles off, lots of trending

mcmc.check <- mcmc.diagnostics(imputation.list[[5]][[6]])
####A fair bit off and trending

#mcmc.check <- mcmc.diagnostics(imputation.list[[6]][[6]])



getwd()
# Pool using Rubin's rules
impvar <- pool.scalar(mice.coef.table[1,2:(leng + 1)], mice.se.table[1,2:(leng + 1)],  k = 1) 
impvar$qbar # pooled estimate
impvar$ubar # pooled se
impvar$b # between-imputation variance

















ergm.data.baseline.imputed[[1]][[2]]
########################################
#####Pool results using mice   #########
########################################

##create list with each imputation run in a single list 

imp.list.school1 <- list()
imp.list.school2 <- list()
imp.list.school3 <- list()
imp.list.school4 <- list()
imp.list.school5 <- list()
imp.list.school6 <- list()

for (j in 1:length(baseline.imp.results)) {
 imp.list.school1[[1]] <- data.frame(name = "sch1", coefs = baseline.imp.results[[j]][[1]])    
 imp.list.school2[[j]] <- baseline.imp.results[[j]][[2]]    
 imp.list.school3[[j]] <- baseline.imp.results[[j]][[3]]    
 imp.list.school4[[j]] <- baseline.imp.results[[j]][[4]]    
 imp.list.school5[[j]] <- baseline.imp.results[[j]][[5]]    
 imp.list.school6[[j]] <- baseline.imp.results[[j]][[6]]    
}

school1_pooled.base <- summary(pool(imp.list.school1))
school2_pooled.base <- summary(pool(imp.list.school2))
school3_pooled.base <- summary(pool(imp.list.school3))
school4_pooled.base <- summary(pool(imp.list.school4))
school5_pooled.base <- summary(pool(imp.list.school5))
school6_pooled.base <- summary(pool(imp.list.school6))




#check
#summary[[9]]  



#---+---+---+---+---+---+---+---+---+---+---+---+#
# + + +       MODEL 1        + + +
#---+---+---+---+---+---+---+---+---+---+---+---+#

mod1.fit <- mclapply(net,  function(x) {
                  ergm(x ~ edges 
                       + nodematch ("gender")
                       + nodefactor ("know.var", base=1)
                       + gwesp(0.25, fixed=T),
                       directed=T, 
                       constraints=~bd(maxout=6),
                       control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
                                }
)


mod1.summary <- mclapply(mod1.fit, function(x) {summary(x)} )
#mod1.diag <- mclapply(mod1.fit, function(x) {mcmc.diagnostics(x)} )


#for (i in 1:12){
#  
#  my.fit[[i]] <- ergm(net[[i]] ~ edges 
#                      + nodematch ("gender")
#                      + nodefactor ("know.var", base=1)
#                      + gwesp(0.25, fixed=T),
#                      directed=T, 
#                      constraints=~bd(maxout=6),
#                      control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
  
#  summary[[i]] <- summary(my.fit[[i]])
  
#}  

#for (i in 1:12){
#mcmc.diagnostics(my.fit[[i]])
#}

#---+---+---+---+---+---+---+---+---+---+---+---+#
# + + +       MODEL 2        + + +
#---+---+---+---+---+---+---+---+---+---+---+---+#


mod2.fit <- mclapply(net,  function(x) {
  ergm(x ~ edges 
       + nodematch ("gender")
       + nodefactor("gender")
       + absdiff ("know.var")
       + absdiff ("att.var")
       + absdiff ("conf.var")
       + gwesp(0.25, fixed=T),
       directed=T, 
       constraints=~bd(maxout=6),
       control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
}
)
mod2.summary <- mclapply(mod2.fit, function(x) {summary(x)} )
mod2.diag <- mclapply(mod2.fit, function(x) {mcmc.diagnostics(x)} )


#for (i in 1:12){
#  
#  my.fit[[i]] <- ergm(net[[i]] ~ edges 
#                      + nodematch ("gender")
#                      + nodefactor("gender")
#                      + absdiff ("know.var")
#                      + absdiff ("att.var")
#                      + absdiff ("conf.var")
#                      + gwesp(0.25, fixed=T),
#                      directed=T, 
#                      constraints=~bd(maxout=6),
#                      control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
  
#  summary[[i]] <- summary(my.fit[[i]])
  
#}  

#---+---+---+---+---+---+---+---+---+---+---+---+#
# + + +       MODEL 3        + + + - FULL MODEL
#---+---+---+---+---+---+---+---+---+---+---+---+#


mod3.fit <- mclapply(net,  function(x) {
  ergm(x ~ edges 
       + mutual
       + nodematch("gender", diff = T, levels=NULL)
       + nodefactor("gender")
       + nodefactor("outschool.var")
       + absdiff ("know.var")
       + absdiff  ("att.var")
       + absdiff  ("conf.var")
       + absdiff  ("sex.var")
       + gwesp(0.25, fixed=T),
       directed=T, 
       constraints=~bd(maxout=6),
       control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
}
)
mod3.summary <- mclapply(mod3.fit, function(x) {summary(x)} )
mod3.diag <- mclapply(mod3.fit, function(x) {mcmc.diagnostics(x)} )



#for (i in 1:12){

#my.fit[[i]] <- ergm(net[[i]] ~ edges 
#                    + mutual
#                    + nodematch("gender", diff = T, levels=NULL)
#                    + nodefactor("gender")
#                    + absdiff ("know.var")
#                    + absdiff  ("att.var")
#                    + absdiff  ("conf.var")
#                    + absdiff  ("sex.var")
#                    + gwesp(0.25, fixed=T),
#                    directed=T, 
#                    constraints=~bd(maxout=6),
#                    control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))

# summary[[i]] <- summary(my.fit[[i]])
#}



#mcmc.diagnostics(my.fit[[1]])



# ------------------------------------#  
#   +  +  +  GOODNESS of FIT  +  +  +
# ------------------------------------#
gof_dist <- list()
gof_odegr <- list()
gof_idegr <- list()
gof_triads <- list()

baseline.iter10.sch1[[10]]
x <- gof(baseline.iter10.sch1[[10]]~triadcensus)

summary(x)
plot(x)

ideg <- gof(baseline.iter10.sch1[[10]]~idegree)
plot(ideg)
odeg <- gof(baseline.iter10.sch1[[10]]~odegree)
plot(odeg)


for (i in 1:12){

par( mfrow = c(4, 1))
gof_dist[[i]] <-gof(my.fit[[i]]~distance)						
#summary(gof_dist[[i]])	
#plot(gof_dist[[i]])

gof_odegr[[i]] <-gof(my.fit[[i]]~odegree)						
#summary(gof_odegr[[i]])	
#plot(gof_odegr[[i]])

gof_idegr[[i]] <-gof(my.fit[[i]]~idegree)						
#summary(gof_idegr[[i]])	
#plot(gof_idegr[[i]])

gof_triads[[i]] <-gof(my.fit[[i]]~triadcensus)						
#summary(gof_triads[[i]])	
#plot(gof_triads[[i]])

}





#---------------------------------------------------------------------
#------------------------------------------------------------------------
#----------------------------------------------------------------------------
my.fit[[2]] <- ergm(net[[2]] ~ edges 
                    + mutual
                    + nodematch("gender", diff = T, levels=NULL)
                    + nodefactor("gender")
                    + absdiff ("know.var")
                    + absdiff  ("att.var")
                    + absdiff  ("conf.var")
                    + absdiff  ("sex.var")
                    + gwesp(0.25, fixed=T),
                    directed=T, 
                    constraints=~bd(maxout=6),
                    control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
summary(my.fit[[2]])

my.fit[[8]] <- ergm(net[[8]] ~ edges 
                    + mutual
                    + nodematch("gender", diff = T, levels=NULL)
                    + nodefactor("gender")
                    + absdiff ("know.var")
                    + absdiff  ("att.var")
                    + absdiff  ("conf.var")
                    + absdiff  ("sex.var")
                    + gwesp(0.25, fixed=T),
                    directed=T, 
                    constraints=~bd(maxout=6),
                    control=control.ergm(main.method=c("MCMLE"), MCMC.burnin=20000, force.main=TRUE))
summary(my.fit[[8]])


