rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 3100 Classroom networks

# Mark McCann developed this script

# This draw heavily on the website below, and the plot code used in the IHW SNA project 
# https://solomonmg.github.io/blog/2012/Working-with-Bipartite-Affiliation-Network-Data-in-R/

#############
#  Purpose  #
#############

# This file creates social contact networks based on presence in the same classroom


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

library(igraph)
library('Matrix')
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

#Load the pupil by class edgelist
load("T:/projects/Net19 S00371/Data/AnonymisedData/pilot_school_data/working data/TEMP_NO_PUPILID_s2_pupil_subject_edgelist.rdata")
load("T:/projects/Net19 S00371/Data/AnonymisedData/pilot_school_data/working data/TEMP_NO_PUPILID_s4_pupil_subject_edgelist.rdata")

##shorten filename to make it easier to code up
s2raw <- s2.pupil.subject.edgelist

head(s2raw)

#Load the raw edgelist
graphnet <- graph_from_edgelist(as.matrix(s2raw))
###The graphnet object doesn't differentiate between pupil and class nodes

#Convert into an adjacency matrix
adj <- get.adjacency(graphnet)

#Order the adjacency matrix
adj <- adj[order(rownames(adj)),order(colnames(adj))]
#Find where the pupil IDs end and the class IDs start 
rownames(adj)
##Predictably enough (because n = 205), the 205th row/col is the last pupil 

dim(adj)
#Rows 1 to 205 are the pupils
#ROws 206 to 363 are the classes

#Create a 2 mode graph object
twomode <- graph_from_adjacency_matrix(adj, add.rownames = TRUE) 
#Check where the pupils are
V(twomode)$name[1:205]
#Set group characteristic
V(twomode)$class          <- 0
V(twomode)$class[206:363] <- 1

V(twomode)$label          <- ""

V(twomode)$size[1:205]    <- 1
V(twomode)$size[206:363] <- 6

V(twomode)$frame.color <- "black"

#Set colour of node based on group membership
V(twomode)$color[which(V(twomode)$class==0)] <- rgb(1, 0, 0, 0.5)
V(twomode)$color[which(V(twomode)$class==1)] <- rgb(0.8, 1, 1, 0.5)

#Set paper node to one colour

#Set edge characteristics
E(twomode)$color <- rgb(.8,.8,.8,.8)
E(twomode)$width <- 0.5
E(twomode)$arrow.size <- 0.1

plot(twomode,main = 'Pupils and classes')

##########################################################
#Two to one mode
##########################################################


#Convert edgelist to sparse matrix
A <- spMatrix(nrow=length(unique(s2raw$id)),
              ncol=length(unique(s2raw$Subject)),
              i = as.numeric(factor(s2raw$id)),
              j = as.numeric(factor(s2raw$Subject)),
              x = rep(1, length(as.numeric(s2raw$id))) )
row.names(A) <- levels(factor(s2raw$id))
colnames(A) <- levels(factor(s2raw$Subject))


##########################################################
#    One Mode projection - Pupil shared classes network  #
##########################################################

pupilmat <- tcrossprod(as.matrix(A)) 

pupil.1mode  <- graph.adjacency(pupilmat, mode = 'undirected')
E(pupil.1mode)$weight <- count.multiple(pupil.1mode)
pupil.1mode <- simplify(pupil.1mode)
V(pupil.1mode)$size <- 4


E(pupil.1mode)$width <- log(E(pupil.1mode)$weight+1) * 0.05
##Set the gamma (line transparency) on a log scale as a proportion of the highest connection
egam <- (log(E(pupil.1mode)$weight))/max(log(E(pupil.1mode)$weight))

plot(
  E(pupil.1mode)$weight,  
  egam
)
  
range(E(pupil.1mode)$weight)

range(egam)

E(pupil.1mode)$color <- rgb(0,0,0,egam)
V(pupil.1mode)$label <- ""

E(pupil.1mode)$arrow.width <- 0.1

pdf("s2 pupil 1 mode shared classes network.pdf")
plot(pupil.1mode,main = 'Pupil shared classes network', arrow.width = 0.01)
dev.off()


##########################################################
#  One Mode projection - Classes shared pupil network    #
##########################################################

classmat <- tcrossprod(t(as.matrix(A))) 


class.1mode  <- graph.adjacency(classmat, mode = 'undirected')
E(class.1mode)$weight <- count.multiple(class.1mode)
class.1mode <- simplify(class.1mode)
V(class.1mode)$size <- 1


E(class.1mode)$width <- log(E(class.1mode)$weight+1) * 0.05
##Set the gamma (line transparency) on a log scale as a proportion of the highest connection
egam <- (log(E(class.1mode)$weight))/max(log(E(class.1mode)$weight))

plot(
  E(class.1mode)$weight,  
  egam
)

range(E(class.1mode)$weight)
range(egam)

E(class.1mode)$color <- rgb(0,0,0,egam)
E(class.1mode)$frame.color <- NA
E(class.1mode)$arrow.size <- 0.3


V(class.1mode)$label.cex <- 0.2
V(class.1mode)$label.color <- rgb(0,1,.2,.9)
#V(class.1mode)$label <- ""


pdf("s2 class 1 mode shared pupils network.pdf")
plot(class.1mode,main = 'Class shared pupil network')
dev.off()

pdf("s2 class 1 mode shared pupils network kklayout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.kamada.kawai)
dev.off()


pdf("s2 class 1 mode shared pupils network drl layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.drl)
dev.off()

pdf("s2 class 1 mode shared pupils network gem layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.gem)
dev.off()

pdf("s2 class 1 mode shared pupils network circle layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.circle)
dev.off()




###############################################################################
###############################################################################
###############################################################################

##shorten filename to make it easier to code up
s4raw <- s4.pupil.subject.edgelist

head(s4raw)

#Load the raw edgelist
graphnet <- graph_from_edgelist(as.matrix(s4raw))
###The graphnet object doesn't differentiate between pupil and class nodes

#Convert into an adjacency matrix
adj <- get.adjacency(graphnet)

#Order the adjacency matrix
adj <- adj[order(rownames(adj)),order(colnames(adj))]
#Find where the pupil IDs end and the class IDs start 
rownames(adj)
##The 199th row/col is the last pupil 

dim(adj)
#Rows 1 to 199 are the pupils
#ROws 200 to 352 are the classes

#Create a 2 mode graph object
twomode <- graph_from_adjacency_matrix(adj, add.rownames = TRUE) 
#Check where the pupils are
V(twomode)$name[1:199]
#Set group characteristic
V(twomode)$class          <- 0
V(twomode)$class[200:352] <- 1

V(twomode)$label          <- ""

V(twomode)$size[1:199]    <- 1
V(twomode)$size[200:352] <- 6

V(twomode)$frame.color <- NA

#Set colour of node based on group membership
V(twomode)$color[which(V(twomode)$class==0)] <- rgb(1, 0, 0, 0.5)
V(twomode)$color[which(V(twomode)$class==1)] <- rgb(0.8, 1, 1, 0.5)

#Set paper node to one colour

#Set edge characteristics
E(twomode)$color <- rgb(.8,.8,.8,.8)
E(twomode)$width <- 0.5
E(twomode)$arrow.size <- 0.1

plot(twomode,main = 'Pupils and classes')

##########################################################
#Two to one mode
##########################################################


#Convert edgelist to sparse matrix
A <- spMatrix(nrow=length(unique(s4raw$id)),
              ncol=length(unique(s4raw$Subject)),
              i = as.numeric(factor(s4raw$id)),
              j = as.numeric(factor(s4raw$Subject)),
              x = rep(1, length(as.numeric(s4raw$id))) )
row.names(A) <- levels(factor(s4raw$id))
colnames(A) <- levels(factor(s4raw$Subject))


##########################################################
#    One Mode projection - Pupil shared classes network  #
##########################################################

pupilmat <- tcrossprod(as.matrix(A)) 

pupil.1mode  <- graph.adjacency(pupilmat, mode = 'undirected')
E(pupil.1mode)$weight <- count.multiple(pupil.1mode)
pupil.1mode <- simplify(pupil.1mode)
V(pupil.1mode)$size <- 2


E(pupil.1mode)$width <- log(E(pupil.1mode)$weight+1) * 0.05
##Set the gamma (line transparency) on a log scale as a proportion of the highest connection
egam <- (log(E(pupil.1mode)$weight))/max(log(E(pupil.1mode)$weight))

plot(
  E(pupil.1mode)$weight,  
  egam
)

range(E(pupil.1mode)$weight)
range(egam)

E(pupil.1mode)$color <- rgb(0,0,0,egam)
V(pupil.1mode)$label <- ""


pdf("s4 pupil 1 mode shared classes network.pdf")
plot(pupil.1mode,main = 'Pupil shared classes network')
dev.off()


##########################################################
#  One Mode projection - Classes shared pupil network    #
##########################################################

classmat <- tcrossprod(t(as.matrix(A))) 


class.1mode  <- graph.adjacency(classmat, mode = 'undirected')
E(class.1mode)$weight <- count.multiple(class.1mode)
class.1mode <- simplify(class.1mode)
V(class.1mode)$size <- 1


E(class.1mode)$width <- log(E(class.1mode)$weight+1) * 0.05
##Set the gamma (line transparency) on a log scale as a proportion of the highest connection
egam <- (log(E(class.1mode)$weight))/max(log(E(class.1mode)$weight))

plot(
  E(class.1mode)$weight,  
  egam
)

range(E(class.1mode)$weight)
range(egam)

E(class.1mode)$color <- rgb(0,0,0,egam)
E(class.1mode)$frame.color <- NA

V(class.1mode)$label.cex <- 0.2
V(class.1mode)$label.color <- rgb(0,1,.2,.9)
#V(class.1mode)$label <- ""


pdf("s4 class 1 mode shared pupils network.pdf")

plot(class.1mode,main = 'Class shared pupil network')

dev.off()

pdf("s4 class 1 mode shared pupils network kklayout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.kamada.kawai)
dev.off()


pdf("s4 class 1 mode shared pupils network drl layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.drl)
dev.off()

pdf("s4 class 1 mode shared pupils network gem layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.gem)
dev.off()


pdf("s4 class 1 mode shared pupils network circle layout.pdf")
plot(class.1mode,main = 'Class shared pupil network', layout = layout.circle)
dev.off()

