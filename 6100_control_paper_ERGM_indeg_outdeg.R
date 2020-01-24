rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 6000 Ergms

# Chiara Broccatelli developed ergm script
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
  
  #for (j in 1:3) {
  #for (j in 1:22) {
  for (j in 1:leng) {
    #for (j in 4:40) {
    df1 <- dataset[[j]]
    for (i in 1:6) {
      df <- df1[[i]]
      
      school.result.list[[i]] <- ergm(df ~ edges 
                                      + mutual
                                      + gwesp(0.25, fixed = T)
                                      + idegree1.5()
                                      + nodematch("gender")
                                      + nodefactor("gender")
                                      + nodematch("sex.var", diff = T)
                                      + nodefactor("sex.var")
                                      + absdiff("know.var")
                                      + absdiff("att.var")
                                      + absdiff("conf.var")
                                      + nodecov("know.var")
                                      + nodecov("att.var")
                                      + nodecov("conf.var")
                                      ,
                                      directed=T, 
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"),MCMC.interval=1024, 
                                                           force.main=F , seed = 274,
                                                           parallel=11, parallel.type="PSOCK")
                                      #                                     eval.loglik = F ###This cuts computation time but doesnt update screen
      )
    }
    imputation.list[[j]] <- school.result.list
    print(j)
 #   write.table(1, file = paste0("completed iteration",j,"so far.txt"))
#    save(imputation.list, file = paste0("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/ergm_imp",j,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}

#########################
#                       #
#  Main body of script  #
#                       #
#########################

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/")


load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/ergm_data_control_imputed.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/ergm_data_baseline_imputed.rdata")

#####Run these before calling the run.imputed.ergms function     
imputation.list <- list()
school.result.list <- list()

#####Baseline completed up to 22 imputations
baseline.ideg.odeg <- run.imputed.ergms(dataset = ergm.data.baseline.imputed, 
                                          savename = "baseline",
                                          num_iters = 5)



control.ideg.odeg <- run.imputed.ergms(dataset = ergm.data.control.imputed, 
                                         savename = "control",
                                         num_iters = 5)

save(baseline.ideg.odeg , file= "baseline.ideg.odeg.rdata")
save(control.ideg.odeg , file= "control.ideg.odeg.rdata")





########Previously fit models


ergm.data.baseline.imputed[[1]][[1]]

summary(ergm.data.baseline.imputed[[1]][[1]] ~ edges 
                                + mutual
                                + gwesp(0.25, fixed = T)
                                + idegree1.5()
                                + nodematch("gender")
                                + nodefactor("gender")
                                + nodematch("sex.var", diff = T)
                                + nodefactor("sex.var")
                                + absdiff("know.var")
                                + absdiff("att.var")
                                + absdiff("conf.var")
                                + nodecov("know.var")
                                + nodecov("att.var")
                                + nodecov("conf.var"))
                                
x <- mixingmatrix(ergm.data.baseline.imputed[[1]][[1]], "sex.var")
x <- mixingmatrix(ergm.data.baseline.imputed[[1]][[1]], "know.var")
x <- mixingmatrix(ergm.data.baseline.imputed[[1]][[1]], "att.var")
x <- mixingmatrix(ergm.data.baseline.imputed[[1]][[1]], "conf.var")
x <- mixingmatrix(ergm.data.baseline.imputed[[1]][[1]], "gender")

x.tot <- ergm.data.baseline.imputed[[1]][[1]]$gal$mnext


#Total percent of ties in each cell
x$matrix / x.tot * 100 

#Row percentages, proportion send to
rbind(
x$matrix[1,] /  sum(x$matrix[1,]),
x$matrix[2,] /  sum(x$matrix[2,]),
x$matrix[3,] /  sum(x$matrix[3,])
)
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
# NOTE: it is better to include all the terms at the same time and so, it is better to only use the full model for the publication


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

help("ergm.terms")

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

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/+MELNET+/")
save.image(file = "all.12.models.Rdata")




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


