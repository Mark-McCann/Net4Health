rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# 4000 Descriptive tables

# Mark McCann developed this script
#    3500 latent class sex items also calculated some values for the tables

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

require(network)

#########################
#                       #
#     Load functions    #
#                       #
#########################

source('//192.168.0.17/stash_sna/DataAnalysis/Syntax/+MELNET+/combineLists.R')

#########################
#                       #
#  Main body of script  #
#                       #
#########################


load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_full_network_with_missing.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_full_network_with_missing.rdata")



net <- combine.lists(control.full.network.with.missing,baseline.full.network.with.missing)


#get.vertex.attribute(net[[1]] ,"vertex.names")
#get.vertex.attribute(net[[2]] ,"vertex.names")
#get.vertex.attribute(net[[3]] ,"vertex.names")
#get.vertex.attribute(net[[4]] ,"vertex.names")
#get.vertex.attribute(net[[5]] ,"vertex.names")

#get.vertex.attribute(net[[6]] ,"vertex.names")
#get.vertex.attribute(net[[7]] ,"vertex.names")
#get.vertex.attribute(net[[8]] ,"vertex.names")
#get.vertex.attribute(net[[9]] ,"vertex.names")
#get.vertex.attribute(net[[10]] ,"vertex.names")
#get.vertex.attribute(net[[11]] ,"vertex.names")
#get.vertex.attribute(net[[12]] ,"vertex.names")


###Create table to hold descriptives

###Change nr to number of rows you add 
desctable <- data.frame(matrix(NA, nc = 14, nr = 18))

colnames(desctable) <- c("Variable",
                         "Net1", "Net2", "Net3", "Net4",
                         "Net5", "Net6", "Net7", "Net8",
                         "Net9", "Net10", "Net11", "Net12",
                         "Total")

###add rows with labels here
desctable[1,1] <- "Pupils"
desctable[2,1] <- "Gender"
desctable[3,1] <- "Boy"
desctable[4,1] <- "Girl"
desctable[5,1] <- "Trans / Non-binary"
desctable[6,1] <- "Missing gender"
desctable[7,1] <- "Sexual activity missing"
desctable[8,1] <- "Inactive"
desctable[9,1] <- "Active no experience"
desctable[10,1] <- "Sexually active"
desctable[11,1] <- "Knowledge"
desctable[12,1] <- "Missing Knowledge"
desctable[13,1] <- "Attitudes"
desctable[14,1] <- "Missing Attitudes"
desctable[15,1] <- "Confidence"
desctable[16,1] <- "Missing confidence"
desctable[17,1] <- "Outside school friends"
desctable[18,1] <- "Non-responding pupils"

for (i in 1:12){


descnet <- net[[i]]
column <- i + 1
know   <- as.numeric(get.vertex.attribute(descnet, "know.var"))
att    <- as.numeric(get.vertex.attribute(descnet, "att.var"))
conf   <- as.numeric(get.vertex.attribute(descnet, "conf.var"))
gender <- as.numeric(get.vertex.attribute(descnet, "gender"))
sex    <- get.vertex.attribute(descnet, "sex.var")
outfrn <- as.numeric(get.vertex.attribute(descnet, "outschool.var"))

tot.in.network <- descnet$gal$n
###Fill in table values
desctable[1,column] <- tot.in.network
#Gender
desctable[3,column] <- paste0(table(gender, useNA = "always")[1]      ," (", round((table(gender, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
desctable[4,column] <- paste0(table(gender, useNA = "always")[2]      ," (", round((table(gender, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
desctable[5,column] <- paste0(table(gender, useNA = "always")[3]      ," (", round((table(gender, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 
desctable[6,column] <- paste0(tail(table(gender, useNA = "always"),1) ," (", round((tail(table(gender, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 

#Sexual activity
desctable[7,column] <- paste0(table(sex, useNA = "always")[4]      ," (", round((table(sex, useNA = "always")[4] / tot.in.network) *100 ,1) ,")") 


desctable[10,column] <- paste0(table(sex, useNA = "always")[1]      ," (", round((table(sex, useNA = "always")[1] / tot.in.network) *100 ,1) ,")") 
desctable[8,column]  <- paste0(table(sex, useNA = "always")[2]      ," (", round((table(sex, useNA = "always")[2] / tot.in.network) *100 ,1) ,")") 
desctable[9,column]  <- paste0(table(sex, useNA = "always")[3]      ," (", round((table(sex, useNA = "always")[3] / tot.in.network) *100 ,1) ,")") 

#Knowledge
desctable[11,column]   <- paste0(round(mean(know, na.rm = T),2)                   ," (",round(sd(know, na.rm = T),2),")")
desctable[12,column]   <- paste0(tail(table(know, useNA = "always"),1) ," (", round((tail(table(know, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 


#Attitudes 
desctable[13,column] <-  paste0(round(mean(att, na.rm = T),2)                   ," (",round(sd(att, na.rm = T),2),")") 
desctable[14,column] <-  paste0(tail(table(att, useNA = "always"),1) ," (", round((tail(table(att, useNA = "always"),1) / tot.in.network) *100 ,1) ,")") 

#Confidence 
desctable[15,column] <- paste0(round(mean(conf, na.rm = T),2)                   ," (",round(sd(conf, na.rm = T),2),")")
desctable[16,column] <- paste0(tail(table(conf, useNA = "always"),1) ," (", round((tail(table(conf, useNA = "always"),1) / tot.in.network) *100 ,1) ,")")
#Outside school friends
desctable[17,column] <- paste0(round(mean(outfrn, na.rm = T),2)                   ," (",round(sd(outfrn, na.rm = T),2),")")

}




######################################################################
####################Total columns and absent pupils###################
######################################################################



############Load the networks dataset. The pupil count is higher than respondent count 
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_net_dataset.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_net_dataset.rdata")

c.net.desc.df <- rbind(control.net.dataset[[1]],
  control.net.dataset[[2]],
  control.net.dataset[[3]],
  control.net.dataset[[4]],
  control.net.dataset[[5]],
  control.net.dataset[[6]])
b.net.desc.df <- rbind(baseline.net.dataset[[1]],
  baseline.net.dataset[[2]],
  baseline.net.dataset[[3]],
  baseline.net.dataset[[4]],
  baseline.net.dataset[[5]],
  baseline.net.dataset[[6]])

c.net.desc.df$control <- 1
b.net.desc.df$control  <- 0

net.desc.df <- rbind(c.net.desc.df, b.net.desc.df)


table(net.desc.df$school.id, net.desc.df$control)


count_na <- function(x) sum(is.na(x))
View(net.desc.df)

net.desc.df$misscount <- NULL
for (i in 1:dim(net.desc.df)[1]){
net.desc.df$misscount[i] <- sum(is.na(net.desc.df[i,c(2:9)]))
}

net.desc.df$misscount

net.desc.df$nodemiss <- net.desc.df$misscount==7

missing <- rep(NA,12)
missing[1] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 1)])[2]
missing[2] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 2)])[2]
missing[3] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 3)])[2]
missing[4] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 4)])[2]
missing[5] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 5)])[2]
missing[6] <- table(net.desc.df$nodemiss[which(net.desc.df$control==1 & net.desc.df$school.id == 6)])[2]

missing[7] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 1)])[2]
missing[8] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 2)])[2]
missing[9] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 3)])[2]
missing[10] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 4)])[2]
missing[11] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 5)])[2]
missing[12] <- table(net.desc.df$nodemiss[which(net.desc.df$control==0 & net.desc.df$school.id == 6)])[2]



#####################################################
##               Total columns                     ##


###Take total sample from the tables, as this includes the not present

desctable[1,14] <- 170+	121	+184+	48+	148+	130+	172+	102+	185+	45+	139+	125
total.sample <- 170+	121	+184+	48+	148+	130+	172+	102+	185+	45+	139+	125


total.boys  <- sum(71 ,	57,	73,	21,	70,	53,	55,	41,	74,	26,	51,	49)
total.girls <- sum(66	,53	,84	,19	,56	,57	,87	,54	,94 ,	6	,71	,57)
total.trans <- sum(4	,1	,1	,8	,3	,1	,5	,7	,5	,13	,2	,19)
gender.miss <- sum(29	,10	,26	,8	,19	,19	,25	,7	,12	,13	,15	,19 )
sex.miss    <- sum(31	,13	,28	,8	,21	,26	,33	,8	,12	,13	,24	,20 )
inact.miss  <- sum(28	,25	,34	,5	,23	,22	,41	,26	,59	,2	,34	,25)
nosex.miss  <- sum(63	,58	,81	,25	,64	,51	,71	,49	,72	,16	,61	,46)
active.miss <- sum(48	,25	,41	,10	,40	,31	,27	,19	,42	,14	,20	,34)

totcol <- function(input = NULL){
  x <- paste0(input," (", round((input / total.sample * 100),2),")")
  return(x)
}


desctable[1,14] <- total.sample

desctable[3,14] <- totcol(total.boys)
desctable[4,14] <- totcol(total.girls)
desctable[5,14] <- totcol(total.trans)
desctable[6,14] <- totcol(gender.miss)
desctable[7,14] <- totcol(sex.miss)
desctable[8,14] <- totcol(inact.miss)
desctable[9,14] <- totcol(nosex.miss)
desctable[10,14] <- totcol(active.miss)




know.missing <- 33 + 13 + 31+ 9 +	21 + 23+  30+ 9+  12+ 13+ 25+	23
att.missing  <- 36 + 13 + 29+ 10+	23 + 25+	34+ 9+	16+ 15+ 27 +25
conf.missing <- 35 + 13 +	28+	14+	21 + 24+	38+	8+	15+	14+ 24 +21

paste0("Table 1 knowledge mean (se): ",(round( mean(desc.df$know.var, na.rm = T) , 2)),
       " (",round( sd(desc.df$know.var, na.rm = T) , 2),")")

desctable[11,14] <- paste0(round(mean(desc.df$know.var, na.rm = T) , 2),
                          " (",round( sd(desc.df$know.var, na.rm = T) , 2),")")


paste0("Table 1 knowledge missing: ",know.missing," (",
       round(know.missing/total.sample,2),")")

desctable[12,14] <- paste0(know.missing," (",
                           round(know.missing/total.sample,2),")")


paste0("Table 1 att mean (se): ",(round( mean(desc.df$att.var, na.rm = T) , 2)),
       " (",round( sd(desc.df$att.var, na.rm = T) , 2),")")

desctable[13,14] <- paste0(round( mean(desc.df$att.var, na.rm = T) , 2),
                           " (",round( sd(desc.df$att.var, na.rm = T) , 2),")")

paste0("Table 1 att missing: ",att.missing," (",round(att.missing/total.sample,2),")")

desctable[14,14] <- paste0(att.missing," (",round(att.missing/total.sample,2),")")
                           
paste0("Table 1 conf mean (se): ",(round( mean(desc.df$conf.var, na.rm = T) , 2))," (",round( sd(desc.df$conf.var, na.rm = T) , 2),")")

desctable[15,14] <- paste0(round( mean(desc.df$conf.var, na.rm = T) , 2)," (",round( sd(desc.df$conf.var, na.rm = T) , 2),")")

paste0("Table 1 conf missing: ",conf.missing," (",round(conf.missing/total.sample,2),")")

desctable[16,14] <- paste0(conf.missing," (",round(conf.missing/total.sample,2),")")


school.sample <- c(170,	121,184,	48,	148,	130,	172,	102,	185,	45,	139,	125)


for (i in 1:12){
  col <- i + 1
  desctable[18,col] <- paste0(missing[[i]]," (", round(missing[[i]] / school.sample[[i]] * 100,2),")")
}

desctable[18,14] <-  paste0(sum(missing)," (", round(sum(missing) / total.sample * 100,2),")")

desctable[18,13] <- paste0(sum(missing)," (", round(sum(missing) / sum(netpupils) * 100,2),")")

desctable <- desctable[c(1:6,8:10,7,11:18),]

setwd("//192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
write.csv(desctable, file = "Control school descriptives 7th Dec.csv")


