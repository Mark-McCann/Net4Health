#############
#  Purpose  #
#############

# Get packages needed to run ERGMs on HPC

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


getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

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

packages <- getPackages(c("ergm", "dplyr", "BH"))

packages <- getPackages(c( "BH"))

packages <- getPackages(c( "Rmpi", "snow"))

packages <- getPackages(c("readRDS"))

packages <- getPackages(c("mice"))

packages <- getPackages(c("metafor"))

dr <- "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/"

download.packages(packages, destdir= dr, 
                  type="source")



