rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 8000 metafor combine models 

#     Mark McCann developed the script 

#############
#  Purpose  #
#############

#Metafor meta analysis of final ERGM estimates

##############
#            #
#    Notes   #
#            #
##############


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

library("metafor")

#########################
#                       #
#     Load functions    # 
#                       #
#########################

forestplot <- function(data = NULL,
                       coefrow = NULL ,
                       variable = "",
                       varname = "",
                       conf = 95
){
  ##Create a 12 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns
  coef.table <- matrix(NA, nr=10, nc = 3) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable==varname)]
    #Fill the table with varname row 2nd col SE  
    coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable==varname)]
  }
  
  ###Add a baseline / control dummmy var  
  coef.table[1:5,3] <- 0
  coef.table[6:10,3] <- 1
  colnames(coef.table) <- c("coef","se","control")
  
  coef.table <- coef.table[!is.na(coef.table[,1]),] 
  #Run meta analysis
  metareg <- rma(yi=coef.table[,1],
                 sei=coef.table[,2],
                 #                 slab=c("Net1", "Net2", "Net3", "Net4", "Net5", "Net6"
                 #                        , "Net7", "Net8","Net9","Net10","Net11","Net12") 
                 # slab=c("Sch 1 base", "Sch 2 base", "Sch 3 base", "Sch 4 base", "Sch 5 base", "Sch 6 base"
                 #        , "Sch 1 ctrl", "Sch 2 ctrl","Sch 3 ctrl","Sch 4 ctrl","Sch 5 ctrl","Sch 6 ctrl") 
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                        , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = conf)
  
  metareg.wave <- rma(yi=coef.table[,1],
                      sei=coef.table[,2], 
                      slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                             , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                      , method = "REML" 
                      , mods = coef.table[,3])
  
  print(metareg.wave)
  
  #Plot results
  setwd("\\\\192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
  
  pdf(paste0(variable," forest 12 networks.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  
  dev.off()
  return(metareg)  
  
}


#########################
#                       #
#  Main body of script  #
#                       #
#########################

##################################################

#                     6500 Models                #


#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch1.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch2.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch4.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch5.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_sch6.rdata")

#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch1.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch2.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch4.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch5.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_sch6.rdata")


indata <- list() 

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data")
filename <- list.files(pattern = "6100_sch*")

for (sch in 1:12) {
  load(filename[sch])
}

indata <- list()
indata[[1]] <- baseline.pooled.6100.sch1[[3]]
indata[[2]] <- baseline.pooled.6100.sch2[[3]]
indata[[3]] <- baseline.pooled.6100.sch3[[3]]


#####Omit School 4
#indata[[4]] <- baseline.pooled.6100.sch4[[3]]
#######Sex2 coef not defined. Fix to zero for purpose of plot
#indata[[4]]$pool.est[7] <- 0.00000000001
#indata[[4]]$pool.se[7]  <-  0.9
####Also se of ifactor for sexvar
#indata[[4]]$pool.se[12] <- 0.9

indata[[4]] <- baseline.pooled.6100.sch5[[3]]
indata[[5]] <- baseline.pooled.6100.sch6[[3]]

indata[[6]]  <- control.pooled.6100.sch1[[3]]
indata[[7]]  <- control.pooled.6100.sch2[[3]]
indata[[8]]  <- control.pooled.6100.sch3[[3]]
####Omit school 4
#indata[[10]] <- control.pooled.6100.sch4[[3]]


indata[[9]] <- control.pooled.6100.sch5[[3]]
indata[[10]] <- control.pooled.6100.sch6[[3]]


##############
### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

know.meta <- forestplot(data =  indata ,variable = "6100 knowledge"  , varname = "absdiff.know.var", conf = 95)
att.meta  <- forestplot(data =  indata ,variable = "6100 attitudes"  , varname = "absdiff.att.var", conf = 95)
conf.meta <- forestplot(data =  indata ,variable = "6100 confidence"  , varname = "absdiff.conf.var", conf = 95)

sex1match.meta <- forestplot(data =  indata ,variable = "6100 matchsex1"  , varname = "nodematch.sex.var.1")
sex2match.meta <- forestplot(data =  indata ,variable = "6100 matchsex2"  , varname = "nodematch.sex.var.2")
sex3match.meta <- forestplot(data =  indata ,variable = "6100 matchsex3"  , varname = "nodematch.sex.var.3")

edges.meta <- forestplot(data =  indata ,variable = "6100 edges"  , varname = "edges")
mutual.meta <- forestplot(data =  indata ,variable = "6100 mutual"  , varname = "mutual")
gwesp.meta <- forestplot(data =  indata ,variable = "6100 gwesp"  , varname = "gwesp.fixed.0.25")
ideg.meta <- forestplot(data =  indata ,variable = "6100 idegree"  , varname = "idegree1.5")

gendermatch.meta <- forestplot(data =  indata ,variable = "6100 gender match"  , varname = "nodematch.gender")

sex2ifac.meta <- forestplot(data =  indata ,variable = "6100 ifactor sex2"  , varname = "nodeifactor.sex.var.2")
sex3ifac.meta <- forestplot(data =  indata ,variable = "6100 ifactor sex3"  , varname = "nodeifactor.sex.var.3")
sex2ofac.meta <- forestplot(data =  indata ,variable = "6100 ofactor sex2", varname = "nodeofactor.sex.var.2")
sex3ofac.meta <- forestplot(data =  indata ,variable = "6100 ofactor sex3"  , varname = "nodeofactor.sex.var.3")
knowicov.meta <- forestplot(data =  indata ,variable = "6100 indeg Knowledge", varname = "nodeicov.know.var")
knowocov.meta <- forestplot(data =  indata ,variable = "6100 outdeg Knowledge", varname = "nodeocov.know.var")

atticov.meta <- forestplot(data =  indata ,variable = "6100 indeg Attitude", varname = "nodeicov.att.var")
attocov.meta <- forestplot(data =  indata ,variable = "6100 outdeg Attitude", varname = "nodeocov.att.var")

conficov.meta <- forestplot(data =  indata ,variable = "6100 indeg Confidence", varname = "nodeicov.conf.var")
confocov.meta <- forestplot(data =  indata ,variable = "6100 outdeg Confidence", varname = "nodeocov.conf.var")



varnamecol <- c("edges",
                "mutual",
                "Gwesp",
                "Indegree Sqrt",
                "Gender match",
                "Not sexually active",
                "Active no intercourse",
                "Intercourse",
                "Knowledge difference",
                "Attitudes difference",
                "Confidence difference",
                "in Active no intercourse",
                "in Intercourse",
                "in Knowledge",
                "in Attitudes",
                "in Confidence",
                "out Active no intercourse",
                "out Intercourse",
                "out Knowledge",
                "out Attitudes",
                "out  Confidence")


#####layout of meta analysis summary table

# Variable labels 

##Intercept lci uci 

# I squared variability

meta.table <- data.frame(matrix(NA, nr = length(varnamecol), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnamecol
counter    <- 1

for (varlist in c("edges.meta", "mutual.meta", "gwesp.meta","ideg.meta",
                  "gendermatch.meta", "sex1match.meta", "sex2match.meta", "sex3match.meta",
                  "know.meta", "att.meta", "conf.meta", "sex2ifac.meta", "sex3ifac.meta", "knowicov.meta","atticov.meta",
                  "conficov.meta", "sex2ofac.meta", "sex3ofac.meta", "knowocov.meta", "attocov.meta", "confocov.meta"
                  )){
  xp <-  predict(get(varlist), transf = exp, digits = 2)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(get(varlist)$I2,2)
  counter <- counter + 1
}


write.csv(meta.table, file = "Metaanalysis table 10 schools 20 imputations omit sch 4.csv")




