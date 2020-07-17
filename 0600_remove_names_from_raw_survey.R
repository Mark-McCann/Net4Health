rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# Mark McCann developed this script


#############
#  Purpose  #
#############

# Loads raw pupil survey data and removes names 


#########################
#                       #
#    Load packages      #
#                       #
#########################

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


setwd("Q:/Project Recipient Data/Net4Health S00317/Pilot/PersonalData/01_StudyMasterFile/Section02_RawData")


#load in the raw files 
file1 <- read.csv("N4H extract 12-02-2020.csv")
file2 <- read.csv("N4H extract 12-02-2020 2.csv")
file3 <- read.csv("N4H extract 12-02-2020 3.csv")

#####create a list of variables  with names                       
drop        <-    c("respondent_first_name",
                   "respondent_second_name",
                   "q_net_friend_1_first_name",
                   "q_net_friend_1_second_name",
                   "q_net_friend_1_nickname",
                   "q_net_friend_2_first_name",
                   "q_net_friend_2_second_name",
                   "q_net_friend_2_nickname",
                   "q_net_friend_3_first_name",
                   "q_net_friend_3_second_name",
                   "q_net_friend_3_nickname",
                   "q_net_friend_4_first_name",
                   "q_net_friend_4_second_name",
                   "q_net_friend_4_nickname",
                   "q_net_friend_5_first_name",
                   "q_net_friend_5_second_name",
                   "q_net_friend_5_nickname",
                   "q_net_friend_6_first_name",
                   "q_net_friend_6_second_name",
                   "q_net_friend_6_nickname",
                   "q_net_friend_7_first_name",
                   "q_net_friend_7_second_name",
                   "q_net_friend_7_nickname",
                   "q_net_friend_8_first_name",
                   "q_net_friend_8_second_name",
                   "q_net_friend_8_nickname",
                   "q_net_friend_9_first_name",
                   "q_net_friend_9_second_name",
                   "q_net_friend_9_nickname",
                   "q_net_friend_10_first_name",
                   "q_net_friend_10_second_name",
                   "q_net_friend_10_nickname",
                   "q_net_friend_emotional_support_outward_1_first_name",
                   "q_net_friend_emotional_support_outward_1_second_name",
                   "q_net_friend_emotional_support_outward_1_nickname",
                   "q_net_friend_emotional_support_outward_2_first_name",
                   "q_net_friend_emotional_support_outward_2_second_name",
                   "q_net_friend_emotional_support_outward_2_nickname",
                   "q_net_friend_emotional_support_outward_3_first_name",
                   "q_net_friend_emotional_support_outward_3_second_name",
                   "q_net_friend_emotional_support_outward_3_nickname",
                   "q_net_friend_emotional_support_inward_1_first_name",
                   "q_net_friend_emotional_support_inward_1_second_name",
                   "q_net_friend_emotional_support_inward_1_nickname",
                   "q_net_friend_emotional_support_inward_2_first_name",
                   "q_net_friend_emotional_support_inward_2_second_name",
                   "q_net_friend_emotional_support_inward_2_nickname",
                   "q_net_friend_emotional_support_inward_3_first_name",
                   "q_net_friend_emotional_support_inward_3_second_name",
                   "q_net_friend_emotional_support_inward_3_nickname",
                   "q_net_friend_trust_1_first_name",
                   "q_net_friend_trust_2_first_name",
                   "q_net_friend_trust_3_first_name",
                   "q_net_friend_best_grades_pals_ladder_1_first_name",
                   "q_net_friend_best_grades_pals_ladder_2_first_name",
                   "q_net_friend_best_grades_pals_ladder_3_first_name",
                   "q_net_friend_respect_pals_ladder_1_first_name",
                   "q_net_friend_respect_pals_ladder_2_first_name",
                   "q_net_friend_respect_pals_ladder_3_first_name",
                   "q_net_friend_people_you_dont_like_1_first_name",
                   "q_net_friend_people_you_dont_like_1_second_name",
                   "q_net_friend_people_you_dont_like_2_first_name",
                   "q_net_friend_people_you_dont_like_2_second_name",
                   "q_net_friend_people_you_dont_like_3_first_name",
                   "q_net_friend_people_you_dont_like_3_second_name",
                   "q_net_friend_pals_ladder_1_first_name",
                   "q_net_friend_pals_ladder_2_first_name",
                   "q_net_friend_pals_ladder_3_first_name",
                   "q_other_gangs_person_2_first_name",
                   "q_gangs_person_1_first_name",
                   "q_gangs_person_1_second_name",
                   "q_gangs_person_2_first_name",
                   "q_gangs_person_2_second_name",
                   "q_other_gangs_person_1_first_name",
                   "q_other_gangs_person_1_second_name",
                   "q_other_gangs_person_2_first_name",
                   "q_other_gangs_person_2_second_name",
                   )


#drop the name info variables from the data frame
file1 <- file1[,!(names(file1) %in% drop)]
file2 <- file2[,!(names(file2) %in% drop)]
file3 <- file3[,!(names(file3) %in% drop)]

setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")

##Save the files
write.csv(file1, "N4H extract 12-02-2020 1 - Anonymised.csv")
write.csv(file2, "N4H extract 12-02-2020 2 - Anonymised.csv")
write.csv(file3, "N4H extract 12-02-2020 3 - Anonymised.csv")
