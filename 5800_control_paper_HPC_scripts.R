#################
#               #
#      Name     #
#               #
#################

# Mark McCann created the file

#############
#  Purpose  #
#############

# Creates separate hpc files

##############
#            #
#    Notes   #
#            #
##############


#########################
#                       #
#    Load packages      #
#                       #
#########################

##Add in the hpc requires
####This was run off-campus
setwd("C:/A Work/A London/hpc")



#######HPC R files
#read files in the directory
infile <- list()
for (sch in 1:6){
  infile[[sch]] <- readLines(paste0("6100_i1_s",sch,".R") )
}

###Change the version to refer to the correct version number
####Each version number ran a separate model. 

for (sch in 1:6){      
  
for (imp in 1:20){
      infile[[sch]][4] <- paste0("version <- ",imp)
      writeLines(infile[[sch]], con = paste0("6100_i",imp,"_s",sch,".R"))
    }
}


####Read in a batch file
infile <- readLines("6501_13.sh" )
####Change the bactch file to refer to correct school and imputation
for (sch in 1:6){      
  for (imp in 1:20){
    infile[11] <- paste0(" R CMD BATCH /export/home/mmc78h/STASH/6100_i",imp,"_s",sch,".R /export/home/mmc78h/STASH/6100_i",imp,"_s",sch,".out")
    writeLines(infile[[sch]], con = paste0("6100_i",imp,"_s",sch,".sh"))
  }
}



