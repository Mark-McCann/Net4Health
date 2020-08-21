rm(list = ls())

#############
#  Purpose  #
#############

# Linking imputed data to networks

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


########Function to create an ergm-ready object 

# Netfile has all the network data
#  impfile has ONE imputed dataset for all SIX schools 
j = 1
create.ergm.imputation.data <- function(netfile = control.edge.att.network , impfile = control.imputed[[1]]){
#read in the file specified
    pre.att <-    impfile

    #Loop over six schools
  for (j in 1:6) {
#Filter to look at the 'i'th school attributes
    link.atts <- filter(pre.att, school.id == j)
# Link on node attributes, matching by ID
    netfile[[j]] %v% "gender" <- as.numeric(as.character(link.atts$gender[match(network.vertex.names(netfile[[j]]), link.atts$id)]))
    netfile[[j]] %v% "school.id" <- as.numeric( as.character(link.atts$school.id[match(network.vertex.names(netfile[[j]]), link.atts$id)]))
    netfile[[j]] %v% "na_count" <- as.numeric(as.character(link.atts$na_count[match(network.vertex.names(netfile[[j]]), link.atts$id)]))
    ###Check isolates - i.e. didn't receive a nomination
    iso.check <- degree(netfile[[j]]) == 0
    no.indeg.id <-    network.vertex.names(netfile[[j]])[degree(netfile[[j]], cmode = "indegree") == 0] 
    
    #Identify nodes with missing on all vars
    missnode    <- get.vertex.attribute(netfile[[j]], 'na_count') == 6
    # Some NAs as not true ID vars to link na_count attribute to
    missnode[is.na(missnode)] <- T
    #Get those IDs
    true.missing.ids <- network.vertex.names(netfile[[j]])[iso.check & missnode]
    #Drop the true missing ids
    netfile[[j]] <- delete.vertices(netfile[[j]],true.missing.ids)
    
    #Now look at who was nominated but didn't complete a survey 
    new.missnode     <- get.vertex.attribute(netfile[[j]], 'na_count') == 6       
    new.missnode.ids <- network.vertex.names(netfile[[j]])[new.missnode]
    
    #Check they have no outdegrees either
    
    ##Loks like there are outdegrees for the missing nodes. Shouldn't be from looking at the raw data
    degree(netfile[[j]], cmode = "outdegree")[new.missnode.ids]
    
    ####Set the out-edges from missing nodes to NA
    #get.edgeIDs(netfile[[j]], new.missnode.ids, neighborhood = "out" )
    #get.edges(netfile[[j]],143, neighborhood = "out" )
    
  #  get.edgeIDs(netfile[[j]],143, neighborhood = "out" )
    
   # netfile[[j]][new.missnode.ids[[9]],]
    
#    It looks like these IDs have outdegrees when it looks like they shouldn't Check this
#    Other higher IDs are appropriately set to missing
 #   new.missnode.ids[[1]]
#    new.missnode.ids[[4]]
#    new.missnode.ids[[5]]
#    new.missnode.ids[[6]]
#    new.missnode.ids[[7]]
      }
 return(netfile)
}

#####Save first imputation ERGM object
ergm.data.control.imputed <- list()
for (i in 1:length(control.imputed)) {
  ergm.data.control.imputed[[i]] <- create.ergm.imputation.data(netfile = control.edge.att.network, impfile = control.imputed[[i]])
}

ergm.data.baseline.imputed <- list()
for (i in 1:length(baseline.imputed)) {
  ergm.data.baseline.imputed[[i]] <- create.ergm.imputation.data(netfile = baseline.edge.att.network, impfile = baseline.imputed[[i]])
}
  
  


