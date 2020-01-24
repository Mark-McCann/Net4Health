##Multiple imputation using indegree attributes

rm(list = ls())

library(foreign)
library(network)
library(dplyr)

library(mice)


load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_net_dataset.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_net_dataset.rdata")


#View(control.net.dataset[[1]])
summary(control.net.dataset[[2]])
summary(control.net.dataset[[3]])
summary(control.net.dataset[[4]])
summary(control.net.dataset[[5]])
summary(control.net.dataset[[6]])

###Create a flat file with all network data
control.flat.file  <- control.net.dataset[[1]]
baseline.flat.file <- baseline.net.dataset[[1]]
for (i in 2:6) {
  control.flat.file  <- rbind(control.flat.file,control.net.dataset[[i]])
  baseline.flat.file <- rbind(baseline.flat.file,baseline.net.dataset[[i]])
  
}

##Create a control dummy variable
baseline.flat.file$control <- 0
control.flat.file$control  <- 1


###Ids aren't unique across sweeps, add 1000 to baseline IDs
control.flat.file$id <- as.numeric(as.character(control.flat.file$id))
baseline.flat.file$id <- as.numeric(as.character(baseline.flat.file$id))
baseline.flat.file$id <- baseline.flat.file$id + 1000


###Create the full attributes file
full.attributes <- rbind(baseline.flat.file,control.flat.file)

full.attributes <- full.attributes[, c(1,2,18,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

#View(full.attributes)

###Change variables to appropriate format for mice
summary(full.attributes)
str(full.attributes)

full.attributes$gender                <- as.factor(full.attributes$gender)
full.attributes$school.id             <- as.factor(full.attributes$school.id)
full.attributes$know.var              <- as.numeric(as.character(full.attributes$know.var)) 
full.attributes$att.var               <- as.numeric(as.character(full.attributes$att.var)) 
full.attributes$conf.var              <- as.numeric(as.character(full.attributes$conf.var)) 
full.attributes$scale.var             <- as.numeric(as.character(full.attributes$scale.var)) 
full.attributes$talk.var              <- as.numeric(as.character(full.attributes$talk.var)) 
full.attributes$out.school.frnd       <- as.numeric(as.character(full.attributes$out.school.frnd)) 
full.attributes$mean.nom.ftalk        <- as.numeric(as.character(full.attributes$mean.nom.ftalk)) 
full.attributes$mean.nom.ftimein      <- as.numeric(as.character(full.attributes$mean.nom.ftimein)) 
full.attributes$mean.nom.ftimeonline  <- as.numeric(as.character(full.attributes$mean.nom.ftimeonline)) 
full.attributes$mean.nom.ftimeout     <- as.numeric(as.character(full.attributes$mean.nom.ftimeout)) 
full.attributes$mean.nom.sex          <- as.numeric(as.character(full.attributes$mean.nom.sex)) 
full.attributes$indegree              <- as.numeric(as.character(full.attributes$indegree)) 
full.attributes$outdegree             <- as.numeric(as.character(full.attributes$outdegree)) 


full.attributes$na_count <- apply(full.attributes[,c("gender","know.var",
                                                     "att.var","conf.var",
                                                     "scale.var","talk.var")], 1, function(x) sum(is.na(x)))

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
#Look at pattern of missingness
md.pattern(full.attributes)

#Does the missing pattern of sex var depend on gender
miss_sex <- is.na(full.attributes$sex.var)
histogram(~gender|miss_sex, data=full.attributes)

#What about knowledge?
histogram(~know.var|miss_sex, data=full.attributes)


#   #    #     #   #  IMPUTE  #     #     #     #     #     #

quickpred(full.attributes, 
          mincor = 0.1,
          include = c('Sex.var',
                      'respondent_school',
                      'control'),
          exclude = c("id","na_count"),
          #                                                  defaultMethod = c()
)

######Corr below 0.1 and sex3.var included in all
imputed <- mice(full.attributes, pred = quickpred(full.attributes, 
                                                  mincor = 0.1,
                                                  include = c('sex.var',
                                                              'school.id',
                                                              'control'),
                                                  exclude = c("id","na_count"),
                                                  ),
                                                  m=50,seed=123)

# What method was used?
imputed$meth
# for what the default is, see "?mice": it is predictive mean matching.

# check out  imputed values
imputed$imp$gender

#compare imputed to observed with missingness
summary(imputed$imp$gender)
summary(full.attributes$gender) 

summary(full.attributes)

summary(imputed$imp$scale.var)
summary(full.attributes$scale.var) 

summary(imputed$imp$know.var)
summary(full.attributes$know.var) 

summary(imputed$imp$conf.var)
summary(full.attributes$conf.var) 

summary(imputed$imp$att.var)
summary(full.attributes$att.var) 

summary(imputed$imp$sex.var)
summary(full.attributes$sex.var) 

#Look at which variables are used to predict what
imputed$predictorMatrix

#If need to do more iterations to check convergence
#imp40 <- mice.mids(imputed, maxit=40, print=F)
#plot(imp40)

# Checking convergence of imputation model
#plot(imputed)
#densityplot(imputed, scales = list(relation = "free", layout = c(5, 1)))
#densityplot(imputed, ~conf.var|.imp)

#densityplot(imp40, scales = list(relation = "free", layout = c(5, 1)))
#densityplot(imp40, ~conf.var|.imp)

# Plot using lattice
#par( mfrow = c(1, 1) )
#stripplot(imputed, pch = 20, cex = 1.2)
#stripplot(imputed)
#xyplot(imputed, conf.var ~ sex3.var | .imp, pch = 20, cex = 1.4)

#stripplot(imp40, pch = 20, cex = 1.2)
#stripplot(imp40)
#xyplot(imp40, conf.var ~ sex3.var | .imp, pch = 20, cex = 1.4)


all.imputed <- list()
for (i in 1:imputed$m){
all.imputed[[i]] <- complete(imputed, i)
}

for (i in 1:length(all.imputed)) {
all.imputed[[i]][all.imputed[[i]]$control==0,]$id <- all.imputed[[i]][all.imputed[[i]]$control==0,]$id - 1000
}

control.imputed  <- lapply(all.imputed, function(x) filter(x, control == 1) )
baseline.imputed <- lapply(all.imputed, function(x) filter(x, control == 0) )



save(control.imputed, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control.imputed.rdata")
save(baseline.imputed, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline.imputed.rdata")


