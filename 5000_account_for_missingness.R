rm(list = ls())


#############
#  Purpose  #
#############

#  Multiple imputation of data

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

library(foreign)
library(network)
library(dplyr)
library(mice)


#########################
#                       #
#     Load functions    #
#                       #
#########################


#########################
#                       #
#  Main body of script  #
#                       #
#########################

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


