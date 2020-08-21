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
               , method = "REML", level = conf)
  
  print(metareg.wave)
  
  #Plot results
  setwd()
  
  pdf(paste0(variable," forest.pdf"))
  
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




