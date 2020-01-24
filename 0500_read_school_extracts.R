rm(list = ls())

#################
#               #
#      Name     #
#               #
#################


#     Modification needed for the section below

    ##    Create Pupil Class incidence matrices        ##

#    This will need the anonymised ID added once created by the database manager



# Mark McCann developed this script


#############
#  Purpose  #
#############

# Loads raw pupil names data and prepares:

#  Name roster for database manager
#  Subject attendance for co-location of pupils


#########################
#                       #
#    Load packages      #
#                       #
#########################

library(dplyr)
library(readxl)

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
setwd("Q:/Project Recipient Data/Net4Health S00317/Pilot/PersonalData/01_StudyMasterFile/Section01_School info files")

s2.raw.df <- read_excel("REDACTED - Pupil Names, Sections & Teachers.xlsx", sheet = "S2 Pupils")
s4.raw.df <- read_excel("REDACTED - Pupil Names, Sections & Teachers.xlsx", sheet = "S4 Pupils")

colnames(s2.raw.df)
###Rename columns

##From this
#"Surname"     "Known as"    "House"       "House Class" "Subject 1"
#"...6"        "...7"       "...8"        "...9"        "Subject 2"
#"...11"       "...12"       "...13"       "...14"      "Subject 3"

##To this
#"Surname"     "Known as"    "House"       "House Class" "Subject 1"
#"Subject 1 Name",	"Subject 1 staff title",	"Subject 1 staff initial",	"Subject 1 staff name"	"Subject 2",
#"Subject 2 Name",	"Subject 2 staff title",	"Subject 2 staff initial",	"Subject 2 staff name"	"Subject 3",



colnames(s2.raw.df) <- c(
  "Surname"       , "Known as"             ,  "House"                  ,"House Class"           , "Subject 1 ID",
  "Subject 1 Name",	"Subject 1 staff title",	"Subject 1 staff initial",	"Subject 1 staff name",	"Subject 2 ID",
  "Subject 2 Name",	"Subject 2 staff title",	"Subject 2 staff initial",	"Subject 2 staff name",	"Subject 3 ID",
  "Subject 3 Name",	"Subject 3 staff title",	"Subject 3 staff initial",	"Subject 3 staff name",	"Subject 4 ID",
  "Subject 4 Name",	"Subject 4 staff title",	"Subject 4 staff initial",	"Subject 4 staff name",	"Subject 5 ID",
  "Subject 5 Name",	"Subject 5 staff title",	"Subject 5 staff initial",	"Subject 5 staff name",	"Subject 6 ID",
  "Subject 6 Name",	"Subject 6 staff title",	"Subject 6 staff initial",	"Subject 6 staff name",	"Subject 7 ID",
  "Subject 7 Name",	"Subject 7 staff title",	"Subject 7 staff initial",	"Subject 7 staff name",	"Subject 8 ID",
  "Subject 8 Name",	"Subject 8 staff title",	"Subject 8 staff initial",	"Subject 8 staff name",	"Subject 9 ID",
  "Subject 9 Name",	"Subject 9 staff title",	"Subject 9 staff initial",	"Subject 9 staff name",	"Subject 10 ID",
  "Subject 10 Name",	"Subject 10 staff title",	"Subject 10 staff initial",	"Subject 10 staff name",	"Subject 11 ID",
  "Subject 11 Name",	"Subject 11 staff title",	"Subject 11 staff initial",	"Subject 11 staff name","Subject 12 ID",
  "Subject 12 Name",	"Subject 12 staff title",	"Subject 12 staff initial",	"Subject 12 staff name",	"Subject 13 ID",
  "Subject 13 Name",	"Subject 13 staff title",	"Subject 13 staff initial",	"Subject 13 staff name",	"Subject 14 ID",
  "Subject 14 Name",	"Subject 14 staff title",	"Subject 14 staff initial",	"Subject 14 staff name",	"Subject 15 ID",
  "Subject 15 Name",	"Subject 15 staff title",	"Subject 15 staff initial",	"Subject 15 staff name",	"Subject 16 ID",
  "Subject 16 Name",	"Subject 16 staff title",	"Subject 16 staff initial",	"Subject 16 staff name",	"Subject 17 ID",
  "Subject 17 Name",	"Subject 17 staff title",	"Subject 17 staff initial",	"Subject 17 staff name"
)

#Check the data
#  View(s2.raw.df)

#Looks OK


############################
##       prepare S4       ##
##    Same as S2,         ##
#    but fewer subjects   ##
############################

colnames(s4.raw.df) <- c(
  "Surname"       , "Known as"             ,  "House"                  ,"House Class"           , "Subject 1 ID",
  "Subject 1 Name",	"Subject 1 staff title",	"Subject 1 staff initial",	"Subject 1 staff name",	"Subject 2 ID",
  "Subject 2 Name",	"Subject 2 staff title",	"Subject 2 staff initial",	"Subject 2 staff name",	"Subject 3 ID",
  "Subject 3 Name",	"Subject 3 staff title",	"Subject 3 staff initial",	"Subject 3 staff name",	"Subject 4 ID",
  "Subject 4 Name",	"Subject 4 staff title",	"Subject 4 staff initial",	"Subject 4 staff name",	"Subject 5 ID",
  "Subject 5 Name",	"Subject 5 staff title",	"Subject 5 staff initial",	"Subject 5 staff name",	"Subject 6 ID",
  "Subject 6 Name",	"Subject 6 staff title",	"Subject 6 staff initial",	"Subject 6 staff name",	"Subject 7 ID",
  "Subject 7 Name",	"Subject 7 staff title",	"Subject 7 staff initial",	"Subject 7 staff name",	"Subject 8 ID",
  "Subject 8 Name",	"Subject 8 staff title",	"Subject 8 staff initial",	"Subject 8 staff name",	"Subject 9 ID",
  "Subject 9 Name",	"Subject 9 staff title",	"Subject 9 staff initial",	"Subject 9 staff name",	"Subject 10 ID",
  "Subject 10 Name",	"Subject 10 staff title",	"Subject 10 staff initial",	"Subject 10 staff name",	"Subject 11 ID",
  "Subject 11 Name",	"Subject 11 staff title",	"Subject 11 staff initial",	"Subject 11 staff name","Subject 12 ID",
  "Subject 12 Name",	"Subject 12 staff title",	"Subject 12 staff initial",	"Subject 12 staff name",	"Subject 13 ID",
  "Subject 13 Name",	"Subject 13 staff title",	"Subject 13 staff initial",	"Subject 13 staff name"
)

#Check the data
#  View(s4.raw.df)
#Looks OK




#################################
##      Save name rosters      ##
#################################

s2.names <- s2.raw.df[,1:2]

####Test the statement "There are no duplicate names in S2, True or False?"
print("There are no duplicate first & last names in S2, True or False?")
dim(s2.names)[1] == dim(unique(s2.names))[1]

print("There are no duplicate surnames in S2, True or False?")
length(s2.names$Surname) == length(unique(s2.names$Surname))

#Looks OK, save file

write.csv(s2.names, file = "Q:/Project Recipient Data/Net4Health S00317/Pilot/PersonalData/01_StudyMasterFile/Section01_School info files/s2 pupil names.csv")


#####Same for S4###########

s4.names <- s4.raw.df[,1:2]

####Test the statement "There are no duplicate names in S2, True or False?"
print("There are no duplicate first & last names in S4, True or False?")
dim(s4.names)[1] == dim(unique(s4.names))[1]

print("There are no duplicate surnames in s4, True or False?")
length(s4.names$Surname) == length(unique(s4.names$Surname))

#Looks OK, save file

write.csv(s4.names, file = "Q:/Project Recipient Data/Net4Health S00317/Pilot/PersonalData/01_StudyMasterFile/Section01_School info files/s4 pupil names.csv")


#####################################################
##                                                 ##
##                                                 ##
##    Create Pupil Class incidence matrices        ##
##                                                 ##
##                                                 ##
#####################################################

####Need to come back here and add in the pupil ID from the PHRF database manager

colnames(s2.raw.df)
s2.pupil.class.df <- select(s2.raw.df,
                            "Subject 1 ID" ,
                            "Subject 2 ID" ,
                            "Subject 3 ID" ,      
                            "Subject 4 ID" ,       
                            "Subject 5 ID" ,  
                            "Subject 6 ID" ,
                            "Subject 7 ID" ,
                            "Subject 8 ID" ,
                            "Subject 9 ID" ,
                            "Subject 10 ID",        
                            "Subject 11 ID",
                            "Subject 12 ID",          
                            "Subject 13 ID",    
                            "Subject 14 ID",
                            "Subject 15 ID",           
                            "Subject 16 ID",           
                            "Subject 17 ID" ) 
#View(s2.pupil.class.df)       

s2.pupil.subject.edgelist <- reshape(s2.pupil.class.df, 
                                     varying =  1:17,
                                     v.names = c("Subject"),
                                     direction = "long")

s2.pupil.subject.edgelist <- s2.pupil.subject.edgelist[c("id","Subject")]

###Remove NAs
s2.pupil.subject.edgelist <- s2.pupil.subject.edgelist[which(!is.na(s2.pupil.subject.edgelist$Subject)),]


#############################
#                           #
#       Same for S4         #
#                           #
#############################


colnames(s4.raw.df)
s4.pupil.class.df <- select(s4.raw.df,
                            "Subject 1 ID" ,
                            "Subject 2 ID" ,
                            "Subject 3 ID" ,      
                            "Subject 4 ID" ,       
                            "Subject 5 ID" ,  
                            "Subject 6 ID" ,
                            "Subject 7 ID" ,
                            "Subject 8 ID" ,
                            "Subject 9 ID" ,
                            "Subject 10 ID",        
                            "Subject 11 ID",
                            "Subject 12 ID",          
                            "Subject 13 ID") 

s4.pupil.subject.edgelist <- reshape(s4.pupil.class.df, 
                                     varying =  1:13,
                                     v.names = c("Subject"),
                                     direction = "long")
s4.pupil.subject.edgelist <- s4.pupil.subject.edgelist[c("id","Subject")]

###Remove NAs
s4.pupil.subject.edgelist <- s4.pupil.subject.edgelist[which(!is.na(s4.pupil.subject.edgelist$Subject)),]


save(s2.pupil.subject.edgelist, file = "T:/projects/Net19 S00371/Data/AnonymisedData/pilot_school_data/working data/TEMP_NO_PUPILID_s2_pupil_subject_edgelist.rdata")
save(s4.pupil.subject.edgelist, file = "T:/projects/Net19 S00371/Data/AnonymisedData/pilot_school_data/working data/TEMP_NO_PUPILID_s4_pupil_subject_edgelist.rdata")
