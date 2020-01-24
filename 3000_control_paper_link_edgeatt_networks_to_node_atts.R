rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# Chiara Broccatelli developed this script
# Mark McCann modified 


#############
#  Purpose  #
#############

# add attributes to networks


#########################
#                       #
#    Load packages      #
#                       #
#########################
library(dplyr)
library(sna)
library(network)

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

# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##
#--------------------------------------------------------------------------------
#             + + +         Preparing Control and Baseline nets and attributes         + + + 
#--------------------------------------------------------------------------------
# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##

#===============================================================================
#                       IMPORTING NETWORKS
#===============================================================================

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_attributes.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_attributes.rdata")

#  +  control schools  +
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_edge_att_networks.rdata")
#  +  baseline schools  +
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_edge_att_networks.rdata")

# - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ## - ++ - ##
#-------------------------------------------------------------
# + + + IMPORTING THE ATTRIBUTES 
#-------------------------------------------------------------

# ATTRIBUTES SCHOOLS
#Control school

summary(control.attributes)
summary(baseline.attributes)

# let's start with control schools
c.gender <- list()
c.know.var <- list()
c.att.var <- list()
c.conf.var <- list()
c.scale.var <- list()
c.info.var <- list()
c.sex.var <- list()
c.talk.var <- list()
c.outschool.var <- list()
c.schoolid.var <- list()

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_outschool_friends.rdata")



for (i in 1:6){
  
  link.atts <- filter(control.attributes, respondent_school == i)
  c.gender[[i]]    <- as.character(link.atts$gender[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.sex.var[[i]]   <- as.numeric(link.atts$sex3.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.scale.var[[i]] <- as.numeric(link.atts$scale.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.talk.var[[i]]  <- as.numeric(link.atts$talk.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.know.var[[i]]  <- as.numeric(link.atts$know.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.conf.var[[i]]  <- as.numeric(link.atts$conf.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.att.var[[i]]   <- as.numeric(link.atts$att.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.schoolid.var[[i]] <- as.numeric(link.atts$respondent_school[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
  c.outschool.var[[i]] <- as.numeric(control.outside.school[[i]]$outschfriends[match(network.vertex.names(control.edge.att.network[[i]]), control.outside.school[[i]]$respondent_id)])

  }



#table(c.att.var[[1]], useNA = "ifany")
#table(c.gender[[1]], useNA = "ifany")
#table(c.sex.var[[1]], useNA = "ifany")
#table(c.scale.var[[1]], useNA = "ifany")
#table(c.talk.var[[1]], useNA = "ifany")
#table(c.know.var[[1]], useNA = "ifany")
#table(c.conf.var[[1]], useNA = "ifany")
#table(c.att.var[[1]], useNA = "ifany")
#table(c.schoolid.var[[1]], useNA = "ifany")
#table(c.outschool.var[[1]], useNA = "ifany")


 set.vertex.attribute(control.edge.att.network[[i]], "gender"   , c.gender[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "sex.var"  , c.sex.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "scale.var", c.scale.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "talk.var" , c.talk.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "know.var" , c.know.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "conf.var" , c.conf.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "att.var"  , c.att.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "school.id"  , c.schoolid.var[[i]])
 set.vertex.attribute(control.edge.att.network[[i]], "outschool.var"  , c.outschool.var[[i]])


 for (i in 1:6) {
  link.atts <- filter(control.attributes, respondent_school == i)

control.edge.att.network[[i]] %v% "gender" <- as.character(
  link.atts$gender[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "sex.var" <- as.character(
  link.atts$sex3.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "scale.var" <- as.character(
  link.atts$scale.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "talk.var" <- as.character(
  link.atts$talk.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "know.var" <- as.character(
  link.atts$know.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "conf.var" <- as.character(
  link.atts$conf.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "att.var" <- as.character(
  link.atts$att.var[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])
control.edge.att.network[[i]] %v% "school.id" <- as.character(
  link.atts$respondent_school[match(network.vertex.names(control.edge.att.network[[i]]), link.atts$id)])

 ###Check isolates - i.e. didn't receive a nomination
 iso.check <- degree(control.edge.att.network[[i]]) == 0
 ###Check nodes without a linked gender - i.e. responded but didn't send a nomination
 miss.check <- is.na(get.vertex.attribute(control.edge.att.network[[i]],'gender') )
 
 ##Check where these are the same i.e. not in data or nominations
 #    These are true missing, weren't a respondent or recipient of nomination
         iso.check & miss.check 
 
 true.missing.ids <- network.vertex.names(control.edge.att.network[[i]])[iso.check & miss.check]
 
 #Drop the true missing ids
 
 control.edge.att.network[[i]] <- delete.vertices(control.edge.att.network[[i]],true.missing.ids)
 
}

#  baseline schools
b.gender <- list()
b.know.var <- list()
b.att.var <- list()
b.conf.var <- list()
b.scale.var <- list()
b.sex.var <- list()
b.talk.var <- list()
b.schoolid.var <- list()
b.outschool.var <- list()

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_outschool_friends.rdata")

for (i in 1:6){
  link.atts <- filter(baseline.attributes, respondent_school == i)
  b.gender[[i]] <- as.character(link.atts$gender[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.sex.var[[i]] <- as.numeric(link.atts$sex3.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.scale.var[[i]] <- as.numeric(link.atts$scale.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.talk.var[[i]] <- as.numeric(link.atts$talk.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.know.var[[i]] <- as.numeric(link.atts$know.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.conf.var[[i]] <- as.numeric(link.atts$conf.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.att.var[[i]] <- as.numeric(link.atts$att.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.schoolid.var[[i]] <- as.numeric(link.atts$respondent_school[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  b.outschool.var[[i]] <- as.numeric(baseline.outside.school[[i]]$outschfriends[match(network.vertex.names(baseline.edge.att.network[[i]]), baseline.outside.school[[i]]$respondent_id)])
  }

#table(b.gender[[1]])
#table(b.sex.var[[1]])
#table(b.scale.var[[1]])
#table(b.talk.var[[1]])
#table(b.know.var[[1]])
#table(b.conf.var[[1]])
#table(b.att.var[[1]])
#table(b.schoolid.var[[5]], useNA = "always")
#table(b.outschool.var[[1]], useNA = "always")

for (i in 1:6){
   set.vertex.attribute(baseline.edge.att.network[[i]], "gender", b.gender[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "sex.var", b.sex.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "scale.var", b.scale.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "talk.var", b.talk.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "know.var", b.know.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "conf.var", b.conf.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "att.var", b.att.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "school.id"  , b.schoolid.var[[i]])
   set.vertex.attribute(baseline.edge.att.network[[i]], "outschool.var", b.outschool.var[[i]])
  
  
link.atts <- filter(baseline.attributes, respondent_school == i)
  
  baseline.edge.att.network[[i]] %v% "gender" <- as.character(
    link.atts$gender[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "sex.var" <- as.character(
    link.atts$sex3.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "scale.var" <- as.character(
    link.atts$scale.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "talk.var" <- as.character(
    link.atts$talk.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "know.var" <- as.character(
    link.atts$know.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "conf.var" <- as.character(
    link.atts$conf.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "att.var" <- as.character(
    link.atts$att.var[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  baseline.edge.att.network[[i]] %v% "school.id" <- as.character(
    link.atts$respondent_school[match(network.vertex.names(baseline.edge.att.network[[i]]), link.atts$id)])
  
  
  
  

###Check isolates - i.e. didn't receive a nomination
     iso.check <- degree(baseline.edge.att.network[[i]]) == 0
###Check nodes without a linked school id - i.e. didn't send a nomination
     miss.check <- is.na(get.vertex.attribute(baseline.edge.att.network[[i]],'gender') )

##Check where these are the same i.e. not in data or nominations
#    These are true missing, weren't a respondent or recipient of nomination
#        iso.check & miss.check 

true.missing.ids <- network.vertex.names(baseline.edge.att.network[[i]])[iso.check & miss.check]

#Drop the true missing ids

baseline.edge.att.network[[i]] <- delete.vertices(baseline.edge.att.network[[i]],true.missing.ids)


##This code shouldnt be needed with ids fixed up####Drop ids linked to a different school id 
####Drop Ids for other schools
#wrong.schid <- get.vertex.attribute(baseline.edge.att.network[[i]],'school.id') != i
#drop.ids <- network.vertex.names(baseline.edge.att.network[[i]])[wrong.schid]
###remove NAs to pass to delete vertices command
#drop.ids <- drop.ids[complete.cases(drop.ids)]
#baseline.edge.att.network[[i]] <- delete.vertices(baseline.edge.att.network[[i]],drop.ids)
}

baseline.full.network.with.missing <- list()
for (i in 1:6){
baseline.full.network.with.missing[[i]] <- baseline.edge.att.network[[i]]
}

control.full.network.with.missing <- list()
for (i in 1:6){
  control.full.network.with.missing[[i]] <- control.edge.att.network[[i]]
}

###############################################################
###Create node attributes based on in-edge attributes         #
###############################################################

for (i in 1:6){
###COunt the number of nodes in the network
leng <- control.full.network.with.missing[[i]]$gal$n

##Create a list with NAs, this prevents an error whenever the next loop returns no value
mean.indeg.sex         <- rep(NA, leng)
mean.indeg.ftalk       <- rep(NA, leng)
mean.indeg.ftimein     <- rep(NA, leng)
mean.indeg.ftimeout    <- rep(NA, leng)
mean.indeg.ftimeonline <- rep(NA, leng)


####Take the mean sex nominated by friends
for (j in 1:leng){
mean.indeg.sex[j]         <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"fsex") ,  na.rm = T)
mean.indeg.ftalk[j]       <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftalk") ,  na.rm = T)
mean.indeg.ftimein[j]     <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimein") ,  na.rm = T)
mean.indeg.ftimeout[j]    <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeout") ,  na.rm = T)
mean.indeg.ftimeonline[j] <- mean( get.edge.attribute(get.edges(control.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeonline") ,  na.rm = T)

}
####Create this as a node attribute
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.sex',mean.indeg.sex)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftalk',mean.indeg.ftalk)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimein',mean.indeg.ftimein)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeout',mean.indeg.ftimeout)
set.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeonline',mean.indeg.ftimeonline)

}


#####            #####
#####   Baseline #####
#####            #####
#####            #####

for (i in 1:6){
  ###COunt the number of nodes in the network
  leng <- baseline.full.network.with.missing[[i]]$gal$n
  
  ##Create a list with NAs, this prevents an error whenever the next loop returns no value
  mean.indeg.sex         <- rep(NA, leng)
  mean.indeg.ftalk       <- rep(NA, leng)
  mean.indeg.ftimein     <- rep(NA, leng)
  mean.indeg.ftimeout    <- rep(NA, leng)
  mean.indeg.ftimeonline <- rep(NA, leng)
  
  
  ####Take the mean sex nominated by friends
  for (j in 1:leng){
    mean.indeg.sex[j]         <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"fsex") ,  na.rm = T)
    mean.indeg.ftalk[j]       <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftalk") ,  na.rm = T)
    mean.indeg.ftimein[j]     <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimein") ,  na.rm = T)
    mean.indeg.ftimeout[j]    <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeout") ,  na.rm = T)
    mean.indeg.ftimeonline[j] <- mean( get.edge.attribute(get.edges(baseline.full.network.with.missing[[i]],j, neighborhood = "in") ,"ftimeonline") ,  na.rm = T)
    
  }
  ####Create this as a node attribute
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.sex',mean.indeg.sex)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftalk',mean.indeg.ftalk)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimein',mean.indeg.ftimein)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeout',mean.indeg.ftimeout)
  set.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeonline',mean.indeg.ftimeonline)
  
}




control.full.network.with.missing[[1]]
control.full.network.with.missing[[2]]
control.full.network.with.missing[[3]]
control.full.network.with.missing[[4]]
control.full.network.with.missing[[5]]
get.vertex.attribute(control.full.network.with.missing[[1]],'mean.nom.sex')

network.vertex.names(control.full.network.with.missing[[1]])

save(baseline.full.network.with.missing, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_full_network_with_missing.rdata")
save(control.full.network.with.missing, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_full_network_with_missing.rdata")



#-------------------------------------------------------------
# + + + SAVE THE DATA + + + + + + + +  
#------------------------------------------------------------- 
# BASELINE
baseline.net.dataset <- list()


for (i in 1:6){
  baseline.net.dataset[[i]] <- as.data.frame(cbind(network.vertex.names(baseline.full.network.with.missing[[i]]),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'school.id'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'gender'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'know.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'att.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'conf.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'scale.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'sex.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'talk.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'outschool.var'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftalk'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimein'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeonline'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.ftimeout'),
                                                   get.vertex.attribute(baseline.full.network.with.missing[[i]],'mean.nom.sex'),
                                                   degree(baseline.full.network.with.missing[[i]], cmode = "indegree"),
                                                   degree(baseline.full.network.with.missing[[i]], cmode = "outdegree")
                                                  )
                                             )

colnames(baseline.net.dataset[[i]]) <- c( "id", "school.id", "gender", "know.var", "att.var", "conf.var", "scale.var", "sex.var",
                                          "talk.var",  "out.school.frnd",
                                          "mean.nom.ftalk", "mean.nom.ftimein","mean.nom.ftimeonline" ,"mean.nom.ftimeout","mean.nom.sex","indegree","outdegree")
}


# Control
control.net.dataset <- list()

for (i in 1:6){
  control.net.dataset[[i]] <- as.data.frame(cbind(network.vertex.names(control.full.network.with.missing[[i]]),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'school.id'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'gender'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'know.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'att.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'conf.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'scale.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'sex.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'talk.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'outschool.var'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftalk'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimein'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeonline'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.ftimeout'),
                                                   get.vertex.attribute(control.full.network.with.missing[[i]],'mean.nom.sex'),
                                                   degree(control.full.network.with.missing[[i]], cmode = "indegree"),
                                                   degree(control.full.network.with.missing[[i]], cmode = "outdegree")
                                                   )
                                            )
  colnames(control.net.dataset[[i]]) <- c( "id", "school.id", "gender", "know.var", "att.var", "conf.var", "scale.var", "sex.var",
                              "talk.var",  "out.school.frnd",
                              "mean.nom.ftalk", "mean.nom.ftimein","mean.nom.ftimeonline" ,"mean.nom.ftimeout","mean.nom.sex","indegree","outdegree")
}

summary(control.net.dataset[[1]])
summary(control.net.dataset[[2]])
dim(control.net.dataset[[3]])
dim(control.net.dataset[[4]])
dim(control.net.dataset[[5]])
dim(control.net.dataset[[6]])

dim(baseline.net.dataset[[1]])
dim(baseline.net.dataset[[2]])
dim(baseline.net.dataset[[3]])
dim(baseline.net.dataset[[4]])
dim(baseline.net.dataset[[5]])
dim(baseline.net.dataset[[6]])

#View(baseline.net.dataset[[1]])
#View(control.net.dataset[[1]])
summary(baseline.net.dataset[[2]])
summary(baseline.net.dataset[[3]])
summary(baseline.net.dataset[[4]])
summary(baseline.net.dataset[[5]])
summary(baseline.net.dataset[[6]])

#Missing school ids are correct people
#####Add school id for the missing ids

###No missing Ids in amended code

for (i in 1:6)  baseline.net.dataset[[i]]$school.id <- i
for (i in 1:6)  control.net.dataset[[i]]$school.id <- i

save(control.net.dataset, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_net_dataset.rdata")
save(baseline.net.dataset, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_net_dataset.rdata")