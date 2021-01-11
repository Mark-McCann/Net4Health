rm(list = ls())
#################
#               #
#      Notes    #
#               #
#################

# Not currently working  - This script requires 1500 to run in the same R session first
#    this is needed to create the network objects that this script uses.

#  * This should be fixed up to create saved network files so the scripts run independently 

# Chiara Broccatelli developed this script for STASH
# Mark McCann modified for Net4Health


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

setwd("C:/Users/mmc78h/Documents/A Work/Net4Health/Data")


load("Pilot Y2 Recoded Data.rdata")
load("Pilot Y4 Recoded Data.rdata")


detach(igraph)

class(friend.10names)

set.vertex.attribute(friend.10names, 'StiAgre' , recoded.Y4$StiAgrY4Scale)
set.vertex.attribute(friend.10names, 'boy' , recoded.Y4$boy)
set.vertex.attribute(friend.10names, 'FindOut' , YearFour$q_37_e)
set.vertex.attribute(friend.10names, 'Compete' , YearFour$q_37_g)
set.vertex.attribute(friend.10names, 'ghq'     , YearFour$GHQY4CaseScale)

set.vertex.attribute(friend.10names, 'Loneliness'     , recoded.Y4$LonUCLAY4Scale)

table(recoded.Y4$LonUCLAY4Scale, useNA = "always")


net.igraph <- asIgraph(friend.10names)

library(igraph)


malecols <- c("green","blue")
V(net.igraph)$color <- malecols[as.numeric(V(net.igraph)$male) + 1]


V(net.igraph)$size <- V(net.igraph)$Loneliness 

plot(net.igraph)


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

