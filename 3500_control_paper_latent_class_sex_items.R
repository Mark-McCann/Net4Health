#################
#               #
#      Name     #
#               #
#################

# 3500 latent class sex items

# Mark McCann developed this script
#     this is based on 1500_LCA_run_models the DataAnalysis/Syntax folder


#############
#  Purpose  #
#############


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

rm(list = ls())

require("reshape2")
require("ggplot2")
require("poLCA")
require("dplyr")
require("plyr")

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

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data")

load("control_attributes_with_raw.rdata")
load("baseline_attributes_with_raw.rdata")

control.attributes.with.raw$control <- 1
baseline.attributes.with.raw$control <- 0

control.attributes.with.raw$id <- control.attributes.with.raw$respondent_id
baseline.attributes.with.raw$id <- baseline.attributes.with.raw$respondent_id + 1000

##Trim down to relevant variables
cont.trim <- control.attributes.with.raw[,c("id","Oral","Genitals","Kissing","Intercourse","Masturbate", "control")]
base.trim <- baseline.attributes.with.raw[,c("id","Oral","Genitals","Kissing","Intercourse","Masturbate", "control")]

df <- rbind(cont.trim,base.trim)

df$Kissing      <- df$Kissing + 1
df$Genitals     <- df$Genitals  + 1
df$Masturbate   <- df$Masturbate   + 1
df$Oral         <- df$Oral + 1
df$Intercourse  <-  df$Intercourse + 1
df$control      <-  df$control+ 1



table(df$Kissing, useNA = "always")      / dim(df)[1] * 100
table(df$Genitals, useNA = "always")     / dim(df)[1] * 100
table(df$Masturbate, useNA = "always")   / dim(df)[1] * 100
table(df$Oral, useNA = "always")         / dim(df)[1] * 100
table(df$Intercourse, useNA = "always")  / dim(df)[1] * 100
table(df$control, useNA = "always")      / dim(df)[1] * 100


sexact.obj <- cbind(
  Kissing,      
  Genitals,     
  Masturbate,   
  Oral,         
  Intercourse  
) ~ 1 



#Name generator
varname <- list()
#The LCA fit objects
sexact.class <- list()
#Table of fit stats
sexact.fit.table <- as.data.frame(matrix(NA, nrow=10, ncol = 3))
names(sexact.fit.table) <- c("Classes", "AIC", "BIC")


#Run for five classes
for (i in 1:5){
  varname[[i]] <- paste0("sexact.class",i)

  sexact.class[[i]] <- poLCA(sexact.obj, df, nclass = i, nrep = 50 , na.rm = FALSE , maxiter = 50000 )
  sexact.fit.table[i,1] <- i  
  sexact.fit.table$AIC[i] <- sexact.class[[i]]$aic
  sexact.fit.table$BIC[i] <- sexact.class[[i]]$bic
  #  sexact.fit.table$ent[i] <- poLCA.entropy(sexact.class[[i]]) 
}



plot(sexact.fit.table$AIC)
plot(sexact.fit.table$BIC)

plot(sexact.fit.table[2:5,]$AIC)
plot(sexact.fit.table[2:5,]$BIC)
plot(sexact.class[[3]]) 

# 3 lowest BIC and AIC


tiff("//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper/Activity 3 Class LCA.tif", compression = "none", width = 960, height = 960)
sexact.class.3.model <- reshape2::melt(sexact.class[[3]]$probs, level=2)
sexact3.plot <- ggplot(sexact.class.3.model,aes(x = L2, y = value, fill = Var2))
sexact3.plot <- sexact3.plot + geom_bar(stat = "identity", position = "stack")
sexact3.plot <- sexact3.plot + facet_grid(Var1 ~ .) 
sexact3.plot <- sexact3.plot + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
sexact3.plot <- sexact3.plot + labs(x = "Items",y="", fill ="Answer categories")
sexact3.plot <- sexact3.plot + theme( axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank(),                    
                                      panel.grid.major.y=element_blank())
sexact3.plot <- sexact3.plot + guides(fill = guide_legend(reverse=TRUE))
print(sexact3.plot)
dev.off()


tiff("//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper/Activity 3 Class LCA.tif", compression = "none", width = 960, height = 960)
sexact.class.3.model <- reshape2::melt(sexact.class[[3]]$probs, level=2)
sexact.class.3.model$Var1 <- revalue(sexact.class.3.model$Var1, c("class 1: "="Active no sex" , "class 2: "="Active","class 3: "="Not active"))
sexact3.plot <- ggplot(sexact.class.3.model,aes(x = L2, y = value, fill = Var2))
sexact3.plot <- sexact3.plot + geom_bar(stat = "identity", position = "stack")
sexact3.plot <- sexact3.plot + facet_grid(Var1 ~ .) 
sexact3.plot <- sexact3.plot + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
sexact3.plot <- sexact3.plot + labs(x = "Sexual act questions",y="Probability of answering 'Yes'", fill = c("Active no sex","Sexually active","Inactive"))
sexact3.plot <- sexact3.plot + theme( axis.text.y=element_blank(),
                                      axis.ticks.y=element_blank(),                    
                                      panel.grid.major.y=element_blank(),
#                                      axis.text.x = element_text(size=10),
                                      text = element_text(size=40),
)
sexact3.plot <- sexact3.plot + guides(fill = "none")
print(sexact3.plot)

dev.off()





#Sexual Activity
df$sex.predclass <- sexact.class[[3]]$predclass

df$sex.var <- factor(df$sex.predclass, 
                           levels = c(1,2,3), labels = c("Partiall active", "Active", "Inactive"))


table(df$sex.var) 
table(df$sex.var) / dim(df)[1]
table(df$sex.var ,useNA = "always") / dim(df)[1]


save(sexact.class, file = "\\\\192.168.0.17/stash_sna/Data/AnonymisedData/working data/Sexual_Activity_Latent_Class_Results.rdata")

load("\\\\192.168.0.17/stash_sna/Data/AnonymisedData/working data/Sexual_Activity_Latent_Class_Results.rdata")
