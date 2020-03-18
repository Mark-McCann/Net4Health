rm(list=ls())
#####This is still very messy, a lot of redunant code and can't be run top to bottom

#################
#               #
#      Name     #
#               #
#################



#############
#  Purpose  #
#############

# Quick overview of emotional interaction networks 
# For the West partnership Principal Education Psychologists meeting






#########################
#                       #
#    Load packages      #
#                       #
#########################
library(network)
library(dplyr)
library(reshape2)
library(network)
library(intergraph)
library(broom.mixed)
library(lme4)

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


#########################################################################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    +    +    +       Net4Health Dummy data   +    +    +    +    +    +
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#########################################################################


# Mark: setwd("T:/projects/Net19 S00371/Data/AnonymisedData/dummy_data")

#T:\projects\Net4Health S00371\Data\AnonymisedData\dummy_data\Test Extract 05012020.csv
# Claudia
# setwd("N:/")

# test.df <- read.csv("Net4Health Test Extract 20012020.csv", stringsAsFactors = FALSE)

#setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/dummy_data")

setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")
test.df <- read.csv("N4H extract 12-02-2020 3 - Anonymised.csv", stringsAsFactors = FALSE)





drop <- c("q_net_friend_1_hang_out",
          "q_net_friend_2_hang_out",
          "q_net_friend_3_hang_out",
          "q_net_friend_4_hang_out",
          "q_net_friend_5_hang_out",
          "q_net_friend_6_hang_out",
          "q_net_friend_7_hang_out",
          "q_net_friend_8_hang_out",
          "q_net_friend_9_hang_out",
          "q_net_friend_10_hang_out",
          "q_net_friend_1_weekend",
          "q_net_friend_2_weekend",
          "q_net_friend_3_weekend",
          "q_net_friend_4_weekend",
          "q_net_friend_5_weekend",
          "q_net_friend_6_weekend",
          "q_net_friend_7_weekend",
          "q_net_friend_8_weekend",
          "q_net_friend_9_weekend",
          "q_net_friend_10_weekend",
          "q_net_friend_1_social_media",
          "q_net_friend_2_social_media",
          "q_net_friend_3_social_media",
          "q_net_friend_4_social_media",
          "q_net_friend_5_social_media",
          "q_net_friend_6_social_media",
          "q_net_friend_7_social_media",
          "q_net_friend_8_social_media",
          "q_net_friend_9_social_media",
          "q_net_friend_10_social_media",
          "q_net_friend_1_current_affairs",
          "q_net_friend_1_music",
          "q_net_friend_1_school",
          "q_net_friend_1_other_pupils",
          "q_net_friend_1_other_people",
          "q_net_friend_1_sports",
          "q_net_friend_1_alchohol",
          "q_net_friend_1_relationships",
          "q_net_friend_1_keeping_fit",
          "q_net_friend_1_eating",
          "q_net_friend_1_other_text_1",
          "q_net_friend_1_other_text_2",
          "q_net_friend_1_other_text_3",
          "q_net_friend_2_current_affairs",
          "q_net_friend_2_music",
          "q_net_friend_2_school",
          "q_net_friend_2_other_pupils",
          "q_net_friend_2_other_people",
          "q_net_friend_2_sports",
          "q_net_friend_2_alchohol",
          "q_net_friend_2_relationships",
          "q_net_friend_2_keeping_fit",
          "q_net_friend_2_eating",
          "q_net_friend_2_other_text_1",
          "q_net_friend_2_other_text_2",
          "q_net_friend_2_other_text_3",
          "q_net_friend_3_current_affairs",
          "q_net_friend_3_music",
          "q_net_friend_3_school",
          "q_net_friend_3_other_pupils",
          "q_net_friend_3_other_people",
          "q_net_friend_3_sports",
          "q_net_friend_3_alchohol",
          "q_net_friend_3_relationships",
          "q_net_friend_3_keeping_fit",
          "q_net_friend_3_eating",
          "q_net_friend_3_other_text_1",
          "q_net_friend_3_other_text_2",
          "q_net_friend_3_other_text_3",
          "q_net_friend_4_current_affairs",
          "q_net_friend_4_music",
          "q_net_friend_4_school",
          "q_net_friend_4_other_pupils",
          "q_net_friend_4_other_people",
          "q_net_friend_4_sports",
          "q_net_friend_4_alchohol",
          "q_net_friend_4_relationships",
          "q_net_friend_4_keeping_fit",
          "q_net_friend_4_eating",
          "q_net_friend_4_other_text_1",
          "q_net_friend_4_other_text_2",
          "q_net_friend_4_other_text_3",
          "q_net_friend_5_current_affairs",
          "q_net_friend_5_music",
          "q_net_friend_5_school",
          "q_net_friend_5_other_pupils",
          "q_net_friend_5_other_people",
          "q_net_friend_5_sports",
          "q_net_friend_5_alchohol",
          "q_net_friend_5_relationships",
          "q_net_friend_5_keeping_fit",
          "q_net_friend_5_eating",
          "q_net_friend_5_other_text_1",
          "q_net_friend_5_other_text_2",
          "q_net_friend_5_other_text_3",
          "q_net_friend_6_current_affairs",
          "q_net_friend_6_music",
          "q_net_friend_6_school",
          "q_net_friend_6_other_pupils",
          "q_net_friend_6_other_people",
          "q_net_friend_6_sports",
          "q_net_friend_6_alchohol",
          "q_net_friend_6_relationships",
          "q_net_friend_6_keeping_fit",
          "q_net_friend_6_eating",
          "q_net_friend_6_other_text_1",
          "q_net_friend_6_other_text_2",
          "q_net_friend_6_other_text_3",
          "q_net_friend_7_current_affairs",
          "q_net_friend_7_music",
          "q_net_friend_7_school",
          "q_net_friend_7_other_pupils",
          "q_net_friend_7_other_people",
          "q_net_friend_7_sports",
          "q_net_friend_7_alchohol",
          "q_net_friend_7_relationships",
          "q_net_friend_7_keeping_fit",
          "q_net_friend_7_eating",
          "q_net_friend_7_other_text_1",
          "q_net_friend_7_other_text_2",
          "q_net_friend_7_other_text_3",
          "q_net_friend_8_current_affairs",
          "q_net_friend_8_music",
          "q_net_friend_8_school",
          "q_net_friend_8_other_pupils",
          "q_net_friend_8_other_people",
          "q_net_friend_8_sports",
          "q_net_friend_8_alchohol",
          "q_net_friend_8_relationships",
          "q_net_friend_8_keeping_fit",
          "q_net_friend_8_eating",
          "q_net_friend_8_other_text_1",
          "q_net_friend_8_other_text_2",
          "q_net_friend_8_other_text_3",
          "q_net_friend_9_current_affairs",
          "q_net_friend_9_music",
          "q_net_friend_9_school",
          "q_net_friend_9_other_pupils",
          "q_net_friend_9_other_people",
          "q_net_friend_9_sports",
          "q_net_friend_9_alchohol",
          "q_net_friend_9_relationships",
          "q_net_friend_9_keeping_fit",
          "q_net_friend_9_eating",
          "q_net_friend_9_other_text_1",
          "q_net_friend_9_other_text_2",
          "q_net_friend_9_other_text_3",
          "q_net_friend_10_current_affairs",
          "q_net_friend_10_music",
          "q_net_friend_10_school",
          "q_net_friend_10_other_pupils",
          "q_net_friend_10_other_people",
          "q_net_friend_10_sports",
          "q_net_friend_10_alchohol",
          "q_net_friend_10_relationships",
          "q_net_friend_10_keeping_fit",
          "q_net_friend_10_eating",
          "q_net_friend_10_other_text_1",
          "q_net_friend_10_other_text_2",
          "q_net_friend_10_other_text_3",
          "q_net_friend_1_listen_music",
          "q_net_friend_1_shops",
          "q_net_friend_1_clubs",
          "q_net_friend_1_cinema",
          "q_net_friend_1_books",
          "q_net_friend_1_hobby",
          "q_net_friend_1_scouts",
          "q_net_friend_1_watch_sports",
          "q_net_friend_1_street",
          "q_net_friend_1_nowhere",
          "q_net_friend_1_play_board_games",
          "q_net_friend_1_play_computer_games",
          "q_net_friend_1_internet",
          "q_net_friend_1_use_social_media",
          "q_net_friend_1_drink_alchohol",
          "q_net_friend_1_smoke_cigarettes",
          "q_net_friend_1_together_text_1",
          "q_net_friend_1_together_text_2",
          "q_net_friend_1_together_text_3",
          "q_net_friend_2_listen_music",
          "q_net_friend_2_shops",
          "q_net_friend_2_clubs",
          "q_net_friend_2_cinema",
          "q_net_friend_2_books",
          "q_net_friend_2_hobby",
          "q_net_friend_2_scouts",
          "q_net_friend_2_watch_sports",
          "q_net_friend_2_street",
          "q_net_friend_2_nowhere",
          "q_net_friend_2_play_board_games",
          "q_net_friend_2_play_computer_games",
          "q_net_friend_2_internet",
          "q_net_friend_2_use_social_media",
          "q_net_friend_2_drink_alchohol",
          "q_net_friend_2_smoke_cigarettes",
          "q_net_friend_2_together_text_1",
          "q_net_friend_2_together_text_2",
          "q_net_friend_2_together_text_3",
          "q_net_friend_3_listen_music",
          "q_net_friend_3_shops",
          "q_net_friend_3_clubs",
          "q_net_friend_3_cinema",
          "q_net_friend_3_books",
          "q_net_friend_3_hobby",
          "q_net_friend_3_scouts",
          "q_net_friend_3_watch_sports",
          "q_net_friend_3_street",
          "q_net_friend_3_nowhere",
          "q_net_friend_3_play_board_games",
          "q_net_friend_3_play_computer_games",
          "q_net_friend_3_internet",
          "q_net_friend_3_use_social_media",
          "q_net_friend_3_drink_alchohol",
          "q_net_friend_3_smoke_cigarettes",
          "q_net_friend_3_together_text_1",
          "q_net_friend_3_together_text_2",
          "q_net_friend_3_together_text_3",
          "q_net_friend_4_listen_music",
          "q_net_friend_4_shops",
          "q_net_friend_4_clubs",
          "q_net_friend_4_cinema",
          "q_net_friend_4_books",
          "q_net_friend_4_hobby",
          "q_net_friend_4_scouts",
          "q_net_friend_4_watch_sports",
          "q_net_friend_4_street",
          "q_net_friend_4_nowhere",
          "q_net_friend_4_play_board_games",
          "q_net_friend_4_play_computer_games",
          "q_net_friend_4_internet",
          "q_net_friend_4_use_social_media",
          "q_net_friend_4_drink_alchohol",
          "q_net_friend_4_smoke_cigarettes",
          "q_net_friend_4_together_text_1",
          "q_net_friend_4_together_text_2",
          "q_net_friend_4_together_text_3",
          "q_net_friend_5_listen_music",
          "q_net_friend_5_shops",
          "q_net_friend_5_clubs",
          "q_net_friend_5_cinema",
          "q_net_friend_5_books",
          "q_net_friend_5_hobby",
          "q_net_friend_5_scouts",
          "q_net_friend_5_watch_sports",
          "q_net_friend_5_street",
          "q_net_friend_5_nowhere",
          "q_net_friend_5_play_board_games",
          "q_net_friend_5_play_computer_games",
          "q_net_friend_5_internet",
          "q_net_friend_5_use_social_media",
          "q_net_friend_5_drink_alchohol",
          "q_net_friend_5_smoke_cigarettes",
          "q_net_friend_5_together_text_1",
          "q_net_friend_5_together_text_2",
          "q_net_friend_5_together_text_3",
          "q_net_friend_6_listen_music",
          "q_net_friend_6_shops",
          "q_net_friend_6_clubs",
          "q_net_friend_6_cinema",
          "q_net_friend_6_books",
          "q_net_friend_6_hobby",
          "q_net_friend_6_scouts",
          "q_net_friend_6_watch_sports",
          "q_net_friend_6_street",
          "q_net_friend_6_nowhere",
          "q_net_friend_6_play_board_games",
          "q_net_friend_6_play_computer_games",
          "q_net_friend_6_internet",
          "q_net_friend_6_use_social_media",
          "q_net_friend_6_drink_alchohol",
          "q_net_friend_6_smoke_cigarettes",
          "q_net_friend_6_together_text_1",
          "q_net_friend_6_together_text_2",
          "q_net_friend_6_together_text_3",
          "q_net_friend_7_listen_music",
          "q_net_friend_7_shops",
          "q_net_friend_7_clubs",
          "q_net_friend_7_cinema",
          "q_net_friend_7_books",
          "q_net_friend_7_hobby",
          "q_net_friend_7_scouts",
          "q_net_friend_7_watch_sports",
          "q_net_friend_7_street",
          "q_net_friend_7_nowhere",
          "q_net_friend_7_play_board_games",
          "q_net_friend_7_play_computer_games",
          "q_net_friend_7_internet",
          "q_net_friend_7_use_social_media",
          "q_net_friend_7_drink_alchohol",
          "q_net_friend_7_smoke_cigarettes",
          "q_net_friend_7_together_text_1",
          "q_net_friend_7_together_text_2",
          "q_net_friend_7_together_text_3",
          "q_net_friend_8_listen_music",
          "q_net_friend_8_shops",
          "q_net_friend_8_clubs",
          "q_net_friend_8_cinema",
          "q_net_friend_8_books",
          "q_net_friend_8_hobby",
          "q_net_friend_8_scouts",
          "q_net_friend_8_watch_sports",
          "q_net_friend_8_street",
          "q_net_friend_8_nowhere",
          "q_net_friend_8_play_board_games",
          "q_net_friend_8_play_computer_games",
          "q_net_friend_8_internet",
          "q_net_friend_8_use_social_media",
          "q_net_friend_8_drink_alchohol",
          "q_net_friend_8_smoke_cigarettes",
          "q_net_friend_8_together_text_1",
          "q_net_friend_8_together_text_2",
          "q_net_friend_8_together_text_3",
          "q_net_friend_9_listen_music",
          "q_net_friend_9_shops",
          "q_net_friend_9_clubs",
          "q_net_friend_9_cinema",
          "q_net_friend_9_books",
          "q_net_friend_9_hobby",
          "q_net_friend_9_scouts",
          "q_net_friend_9_watch_sports",
          "q_net_friend_9_street",
          "q_net_friend_9_nowhere",
          "q_net_friend_9_play_board_games",
          "q_net_friend_9_play_computer_games",
          "q_net_friend_9_internet",
          "q_net_friend_9_use_social_media",
          "q_net_friend_9_drink_alchohol",
          "q_net_friend_9_smoke_cigarettes",
          "q_net_friend_9_together_text_1",
          "q_net_friend_9_together_text_2",
          "q_net_friend_9_together_text_3",
          "q_net_friend_10_listen_music",
          "q_net_friend_10_shops",
          "q_net_friend_10_clubs",
          "q_net_friend_10_cinema",
          "q_net_friend_10_books",
          "q_net_friend_10_hobby",
          "q_net_friend_10_scouts",
          "q_net_friend_10_watch_sports",
          "q_net_friend_10_street",
          "q_net_friend_10_nowhere",
          "q_net_friend_10_play_board_games",
          "q_net_friend_10_play_computer_games",
          "q_net_friend_10_internet",
          "q_net_friend_10_use_social_media",
          "q_net_friend_10_drink_alchohol",
          "q_net_friend_10_smoke_cigarettes",
          "q_net_friend_10_together_text_1",
          "q_net_friend_10_together_text_2",
          "q_net_friend_10_together_text_3"
)

file1 <- test.df[,!(names(test.df) %in% drop)]



colnames(file1[,1:100])

################Emotional interactions questions 

table(file1$q_net_friend_emotional_support_outward_1_hidden_id)

#####Thisis wrong - this only shows within school friend IDs. 
## OUtside school friends will still appear in the answer to other Qs
file1 <-  mutate(file1, emo_1 = q_net_friend_emotional_support_outward_1_hidden_id != 0)
file1 <-  mutate(file1, emo_2 = q_net_friend_emotional_support_outward_2_hidden_id != 0)
file1 <-  mutate(file1, emo_3 = q_net_friend_emotional_support_outward_3_hidden_id != 0)


table(file1$emo_1, useNA = "ifany") / dim(file1)[1]
table(file1$emo_2, useNA = "ifany") / dim(file1)[1]
table(file1$emo_3, useNA = "ifany") / dim(file1)[1]


file1$emo_outdegree <- file1$emo_1 + file1$emo_2 + file1$emo_3

table(file1$emo_outdegree)

round(table(file1$emo_outdegree) / dim(file1)[1],4) * 100



file1$q_net_friend_emotional_support_outward_1_talk <- file1$q_net_friend_emotional_support_outward_1_talk %>%
  factor(. , 
         levels = c(1,2),
         labels = c("Haven't talked","Have talked") 
         )

file1$q_net_friend_emotional_support_outward_2_talk <- file1$q_net_friend_emotional_support_outward_2_talk %>%
  factor(. , 
         levels = c(1,2),
         labels = c("Haven't talked","Have talked") 
         )

file1$q_net_friend_emotional_support_outward_3_talk <- file1$q_net_friend_emotional_support_outward_3_talk %>%
  factor(. , 
         levels = c(1,2),
         labels = c("Haven't talked","Have talked") 
         )

table(file1$q_net_friend_emotional_support_outward_1_talk, useNA = "ifany")
table(file1$q_net_friend_emotional_support_outward_2_talk, useNA = "ifany")
table(file1$q_net_friend_emotional_support_outward_3_talk, useNA = "ifany")




file1$q_net_friend_emotional_support_outward_1_person <- file1$q_net_friend_emotional_support_outward_1_person %>%
  factor(. , 
         levels = c(1,2,3,4,5,6,7,8),
         labels = c("School Friend",
                    "Non school friend",
                    "Teacher",
                    "Counsellor",
                    "Nurse",
                    "Staff",
                    "Family",
                    "Other Adult")
  )


file1$q_net_friend_emotional_support_outward_2_person <- file1$q_net_friend_emotional_support_outward_2_person %>%
  factor(. , 
         levels = c(1,2,3,4,5,6,7,8),
         labels = c("School Friend",
                    "Non school friend",
                    "Teacher",
                    "Counsellor",
                    "Nurse",
                    "Staff",
                    "Family",
                    "Other Adult")
  )




file1$q_net_friend_emotional_support_outward_3_person <- file1$q_net_friend_emotional_support_outward_3_person %>%
  factor(. , 
         levels = c(1,2,3,4,5,6,7,8),
         labels = c("School Friend",
                    "Non school friend",
                    "Teacher",
                    "Counsellor",
                    "Nurse",
                    "Staff",
                    "Family",
                    "Other Adult")
  )


table(file1$q_net_friend_emotional_support_outward_1_person, useNA = "ifany")
table(file1$q_net_friend_emotional_support_outward_2_person, useNA = "ifany")
table(file1$q_net_friend_emotional_support_outward_3_person, useNA = "ifany")


#file1$q_net_friend_1_support <- file1$q_net_friend_1_support %>%
#  factor(. , 
#         levels = c(1,2,3,4,5),
#         labels = c("Never",
#                    "Only once",
#                    "A few times",
#                    "Sometimes",
#                    "Most times")
#  )

#table(file1$q_net_friend_1_support)

#file1$q_net_friend_2_support <- file1$q_net_friend_2_support %>%
#  factor(. , 
#         levels = c(1,2,3,4,5),
#         labels = c("Never",
#                    "Only once",
#                    "A few times",
#                    "Sometimes",
#                    "Most times")
#  )

#table(file1$q_net_friend_2_support)

#file1$q_net_friend_3_support <- file1$q_net_friend_3_support %>%
#  factor(. , 
#         levels = c(1,2,3,4,5),
#         labels = c("Never",
#                    "Only once",
#                    "A few times",
#                    "Sometimes",
#                    "Most times")
#  )

table(file1$q_net_friend_3_support)



###TBC

table(file1$q_net_friend_1_emotions)
table(file1$q_net_friend_1_makes_jokes)
table(file1$q_net_friend_1_changes_subject)
table(file1$q_net_friend_1_mood_1)
table(file1$q_net_friend_1_mood_text_1)
table(file1$q_net_friend_1_mood_2)
table(file1$q_net_friend_1_mood_text_2)
table(file1$q_net_friend_1_mood_3)
table(file1$q_net_friend_1_mood_text_3)


table(file1$q_net_friend_1_support, file1$q_net_friend_emotional_support_outward_1_person)
table(file1$q_net_friend_1_emotions, file1$q_net_friend_emotional_support_outward_1_person)
table(file1$q_net_friend_1_makes_jokes, file1$q_net_friend_emotional_support_outward_1_person)
table(file1$q_net_friend_1_changes_subject, file1$q_net_friend_emotional_support_outward_1_person)


#####Reshape into a long file to look at overall alter action by alter characteristic
colnames(file1)[1150:1170]
dim(file1)

YearTwo <- test.df[test.df$respondent_school == "22", ]
YearFour <- file1[test.df$respondent_school == "24", ]


file1 <- YearTwo
file1 <- YearFour



##################################################################
# Friend net
#################################################################
edge.dfY4 <- YearFour[,c("id",
                         "q_net_friend_1_hidden_id",
                         "q_net_friend_2_hidden_id",
                         "q_net_friend_3_hidden_id",
                         "q_net_friend_4_hidden_id",
                         "q_net_friend_5_hidden_id",
                         "q_net_friend_6_hidden_id",
                         "q_net_friend_7_hidden_id",
                         "q_net_friend_8_hidden_id",
                         "q_net_friend_9_hidden_id",
                         "q_net_friend_10_hidden_id")]

head(edge.dfY4)
str(edge.dfY4)

edge.dfY4 <- edge.dfY4 - 205

# make year four network

edgeY4 <- data.frame()
temp <- data.frame()
for (i in 2:ncol(edge.dfY4)) {
  temp <- cbind(edge.dfY4[, 1], edge.dfY4[, i])
  edgeY4 <- rbind(edgeY4, temp)
}
colnames(edgeY4) <- c("respondent_id", "alter")
edgeY4$alter <- as.character(edgeY4$alter)
table(edgeY4$alter, useNA = "ifany")
edgeY4$alter <- ifelse(edgeY4$alter == "\\N" | edgeY4$alter == "0" |edgeY4$alter == -205 , NA, edgeY4$alter)

edgecleanY4 <- edgeY4[which(!is.na(edgeY4$alter)),] 
edgecleanY4$respondent_id <- as.numeric(edgecleanY4$respondent_id)
edgecleanY4$alter <- as.numeric(edgecleanY4$alter)

detach(package:igraph)
test.netY4 <-network(edgecleanY4,matrix.type='edgelist',ignore.eval=FALSE)
YearFour <- mutate(YearFour, male = q_6 == 1)
set.vertex.attribute(test.netY4, 'male' , YearFour$male)

library(network)



plot(test.netY4)

####################################
#              Emo net
####################################


emo.edge.df <- file1[,
                 c("id",
                  "q_net_friend_emotional_support_outward_1_hidden_id",
  "q_net_friend_emotional_support_outward_2_hidden_id",
  "q_net_friend_emotional_support_outward_3_hidden_id")]

emo.edge.df <- emo.edge.df - 205

emo.edge.el <- melt(emo.edge.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.edge.el) <- c("respondent_id","friend.order","to.id")


emo.person.df <- file1[,c("id", "q_net_friend_emotional_support_outward_1_person",
                             "q_net_friend_emotional_support_outward_2_person",
                             "q_net_friend_emotional_support_outward_3_person")]

emo.person.el <- melt(emo.person.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.person.el) <- c("respondent_id","friend.order","person")


###########################
#    Offer support        #
###########################
emo.support.df <- file1[,c("id", "q_net_friend_1_support",
                           "q_net_friend_2_support",
                           "q_net_friend_3_support")]
emo.support.el <- melt(emo.support.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.support.el) <- c("respondent_id","friend.order","support")

###########################
#   Share own feelings    #
###########################
emo.emotions.df <- file1[,c("id", "q_net_friend_1_emotions",
                           "q_net_friend_2_emotions",
                           "q_net_friend_3_emotions")]
emo.emotions.el <- melt(emo.emotions.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.emotions.el) <- c("respondent_id","friend.order","emotions")

###########################
#          Jokes          #
###########################
emo.jokes.df <- file1[,c("id", "q_net_friend_1_makes_jokes",
                         "q_net_friend_2_makes_jokes",
                         "q_net_friend_3_makes_jokes")]
emo.jokes.el <- melt(emo.jokes.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.jokes.el) <- c("respondent_id","friend.order","jokes")


###########################
#     Change subject     #
###########################
emo.subject.df <- file1[,c("id", "q_net_friend_1_changes_subject",
                         "q_net_friend_2_changes_subject",
                         "q_net_friend_3_changes_subject")]
emo.subject.el <- melt(emo.subject.df, id.vars = "id", variable.name = "friend.order")
colnames(emo.subject.el) <- c("respondent_id","friend.order","subject")



emo.att.el <- bind_cols(emo.edge.el    ,   
                        emo.person.el  ,  
                        emo.support.el ,
                        emo.emotions.el,
                        emo.jokes.el   ,
                        emo.subject.el
)

dim(emo.att.el)
colnames(emo.att.el)
emo.att.el <- emo.att.el[,c("respondent_id","to.id","person","support",
                            "emotions","jokes","subject")]

#emo.att.el <- emo.att.el[which(emo.att.el$to.id!=0),]


emo.att.el$dismiss <- emo.att.el$jokes + emo.att.el$subject
#emo.att.el$to.id[emo.att.el$to.id == -205] <- NA

##Retain outside school alters for MLM
emo.att.mlm <- emo.att.el

##Drop outside school alters
emo.att.el <- filter(emo.att.el, emo.att.el$to.id != -205)




#View(emo.att.el)

att.net <-network(emo.att.el,matrix.type='edgelist',ignore.eval=FALSE)

set.vertex.attribute(att.net, 'male' , YearFour$male)


##Add characteristics for mlm
colnames(emo.att.el)


emo.att.el <- emo.att.mlm
####Add alter gender if in school year
YearFour$to.id <- YearFour$id - 205
emo.att.el <- left_join(emo.att.el, YearFour[,c("to.id","male")], by = "to.id")
colnames(emo.att.el)[9] <- "alter.male"



####Add ego gender 
YearFour$respondent_id <- YearFour$id - 205
emo.att.el <- left_join(emo.att.el, YearFour[,c("respondent_id","male")], by = "respondent_id")

colnames(emo.att.el)[10] <- "ego.male"

library(ergm)


get.vertex.attribute(test.netY4, "vertex.names")
get.vertex.attribute(att.net, "vertex.names")
get.vertex.attribute(test.netY4, "male")
get.vertex.attribute(att.net, "male")

colnames(emo.att.el)

table(emo.att.el$ego.male, useNA = "ifany")

table(emo.att.el[unique(emo.att.el$respondent_id),]$ego.male)

table(emo.att.el$alter.male, useNA = "ifany")


summary(test.netY4 ~ edges + mutual + triangles) / summary(test.netY4 ~ edges)
summary(att.net ~ edges + mutual + triangles) / summary(att.net ~ edges)

summary(test.netY4 ~ edges + mutual + triangles + nodematch('male', diff = T))
summary(att.net ~ edges + mutual + triangles + nodematch('male', diff = T)) 

x <- mixingmatrix(test.netY4,'male')
round(x$matrix / summary(test.netY4 ~ edges)[1],2)

friend.df <- as.data.frame(x$matrix)

####The numbers in table 1 in the briefing
x$matrix
sum(x$matrix[1,])
sum(x$matrix[2,])

round(x$matrix[1,] / sum(x$matrix[1,]),2)
round(x$matrix[2,] / sum(x$matrix[2,]),2)


library(ggplot2)

x <- as.data.frame(x$matrix)

x$From <- c("Girl", "Boy", "Girl", "Boy")
x$To   <- c("Girl", "Girl", "Boy", "Boy")

colnames(x) <- c("Sender","Recipient","Frequency")


setwd("T:/projects/Net4Health S00371/DisseminationAndImpact/Pilot Study/A intermediate outputs")
frnd.chart <- ggplot() + geom_bar(aes(y = Frequency, x = Sender, fill = Recipient),
                                  data = x,
                                  stat="identity") +
  scale_fill_manual(values=c("dark blue","dark green")) +
  labs(title = "Friendship ties") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        axis.text = element_text(size=20))

png("Friend gender mix mat.png", width = 980, height = 980)

ggplot() + geom_bar(aes(y = Frequency, x = Sender, fill = Recipient),
                                 data = x,
                                 stat="identity") +
  scale_fill_manual(values=c("dark blue","dark green")) +
  labs(title = "Friendship ties") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        axis.text = element_text(size=20))
dev.off()


x <- mixingmatrix(att.net,'male')
x$matrix / summary(att.net ~ edges)[1]

####The numbers in table 2 in the briefing
x$matrix
sum(x$matrix[1,])
sum(x$matrix[2,])

round(x$matrix[1,] / sum(x$matrix[1,]),2)
round(x$matrix[2,] / sum(x$matrix[2,]),2)

emo.df <- as.data.frame(x$matrix)

x <- as.data.frame(x$matrix)

x$From <- c("Girl", "Boy", "Girl", "Boy")
x$To   <- c("Girl", "Girl", "Boy", "Boy")

colnames(x) <- c("Sender","Recipient","Frequency")

emo.chart <- ggplot() + geom_bar(aes(y = Frequency, x = Sender, fill = Recipient),
                                 data = x,
                                 stat="identity") +
  scale_fill_manual(values=c("dark blue","dark green")) +
  labs(title = "Emotion support ties") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        axis.text = element_text(size=20))


png("Emo gender mix mat.png", width = 980, height = 980)

ggplot() + geom_bar(aes(y = Frequency, x = Sender, fill = Recipient),
                    data = x,
                    stat="identity") +
  scale_fill_manual(values=c("dark blue","dark green")) +
  labs(title = "Emotion support ties") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        axis.text = element_text(size=20))
dev.off()




library(gridExtra)
#install.packages("gridExtra")

png("Combined gender mix mat.png", width = 1500, height = 980)
grid.arrange(frnd.chart, emo.chart, ncol=2) 
dev.off()
##################
# Combined chart # 
combined.mix <- rbind(friend.df, emo.df)

combined.mix$From <- c("Girl", "Boy", "Girl", "Boy",
                       "Emo. Girl", "Emo. Boy", "Emo. Girl","Emo. Boy")

combined.mix$To   <- c("Girl", "Girl", "Boy", "Boy",
                       "Girl", "Girl", "Boy", "Boy")

colnames(combined.mix) <- c("Sender","Recipient","Frequency")


positions <- c("Girl", "Boy", "Emo .Girl","Emo. Boy")

library(forcats)

combined.mix$Sender <- as.factor(combined.mix$Sender)

ggplot() + geom_bar(aes(y = Frequency, x = reorder(Sender, -Frequency, sum), fill = Recipient),
                    data = combined.mix,
                    stat="identity") +
                    scale_fill_manual(values=c("dark blue","dark green") ) +
                                        labs(x = "Sender",
                                             title = "Within-school friendship and emotion support ties")

                                      


library(igraph)
net.igraph <- asIgraph(test.netY4)
emo.igraph <- asIgraph(att.net)

net.igraph <- simplify(net.igraph)
emo.igraph <- simplify(emo.igraph, remove.loops = T , remove.multiple = F)
plot(emo.igraph, vertex.label = "",vertex.size=1,
     edge.arrow.size = 0.3,
     edge.arrow.width = 0.3,
     edge.arrow.width = 0.3,
     edge.color = "black"
    
)
library(RColorBrewer)

#Check for isolates in the friend net
isolates <- which(degree(net.igraph, mode = 'all') == 0) 

#delete those vertices in the emo net
net.igraph <- delete.vertices(net.igraph, isolates)
emo.igraph <- delete.vertices(emo.igraph, isolates)

V(net.igraph)

plot(net.igraph, 
     vertex.color="black", vertex.size=2,  vertex.label="", 
     edge.arrow.size = 0.2,
#     edge.arrow.width = 0.1,
     arrow.mode = "-",
     edge.color = "black",
     layout = layout.kamada.kawai) 

layout.frnd <- layout.fruchterman.reingold(net.igraph)

layout.frnd <- layout.fruchterman.reingold(emo.igraph)

par(mfrow = c(1,1))


reds <- brewer.pal(5,name = "YlOrRd")
colrs <- c("light gray", reds)
E(emo.igraph)$color <- colrs[E(emo.igraph)$jokes + 1]
E(emo.igraph)$arrow.mode <- 0


plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.color="light gray", vertex.size=2,  vertex.label="", 
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     layout = layout.frnd,
     main = "Making jokes") 


#########

reds <- brewer.pal(5,name = "YlOrRd")
colrs <- c("light gray", reds)
E(emo.igraph)$color <- colrs[E(emo.igraph)$subject + 1]
E(emo.igraph)$arrow.mode <- 0


plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.color="light gray", vertex.size=2,  vertex.label="", 
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     layout = layout.frnd,
     main = "Change subject") 


#########

blues <- brewer.pal(5,name = "YlGnBu")
colrs <- c("light gray", blues)
E(emo.igraph)$color <- colrs[E(emo.igraph)$support + 1]
E(emo.igraph)$arrow.mode <- 0

plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.color="light gray", vertex.size=2,  vertex.label="", 
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     layout = layout.frnd,
     main = "Offer support") 

#########

blues <- brewer.pal(5,name = "YlGnBu")
colrs <- c("light gray", blues)
E(emo.igraph)$color <- colrs[E(emo.igraph)$emotions + 1]
E(emo.igraph)$arrow.mode <- 0

plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.color="light gray", vertex.size=2,  vertex.label="", 
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     layout = layout.frnd,
     main = "Shares emotions") 





#######################################
#        Dismissal plot

layout.frnd <- layout.fruchterman.reingold(net.igraph)

malecols <- c("blue", "red")
V(net.igraph)$color <- malecols[as.numeric(V(net.igraph)$male) + 1]

setwd("T:/projects/Net4Health S00371/DisseminationAndImpact/Pilot Study/A intermediate outputs")
png("Friend net dave harel.png", width = 980, height = 980)
plot(net.igraph, edge.arrow.size=0.4, #arrow.mode = 0,
     vertex.size=4,  vertex.label="", 
     edge.arrow.width = 0.5,
     edge.arrow.color = "black",
     edge.color = "black",
     layout = layout.davidson.harel,
     main = "Friendships") 

dev.off()

png("Emo net panels.png", width = 1980, height = 780)
par(mfrow = c(1,3))

layout.frnd <- layout.fruchterman.reingold(emo.igraph)

blues <- brewer.pal(5,name = "YlGnBu")
colrs <- c("gray", blues)
E(emo.igraph)$color <- colrs[E(emo.igraph)$support + 1]
E(emo.igraph)$arrow.mode <- 0

E(emo.igraph)$width <- E(emo.igraph)$support + 1 * 3
malecols <- c("blue", "red")
V(emo.igraph)$color <- malecols[as.numeric(V(net.igraph)$male) + 1]


plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.size=5,  vertex.label="", 
     edge.arrow.size = 0.1,
     layout = layout.frnd,
# main = "Emotional support",
 label.cex = 2) 



blues <- brewer.pal(5,name = "Greens")
colrs <- c("gray", blues)
E(emo.igraph)$color <- colrs[E(emo.igraph)$emotions + 1]
E(emo.igraph)$arrow.mode <- 0

plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.size=5,  vertex.label="", 
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     layout = layout.frnd,
#    main = "Shares emotions",
    label.cex = 2) 




reds <- brewer.pal(10,name = "YlOrRd")
colrs <- c("light gray", reds)
E(emo.igraph)$color <- colrs[E(emo.igraph)$dismiss + 1]
E(emo.igraph)$arrow.mode <- 0
E(emo.igraph)$width <- E(emo.igraph)$dismiss + 1


plot(emo.igraph, edge.arrow.size=.5, arrow.mode = 0,
     vertex.size=5,  vertex.label="", 
     edge.arrow.size = 0.1,
     layout = layout.frnd,
#    main = "Emotion dismissal",
    label.cex = 20) 


dev.off()
#######################################################
#     COmbined emotion dismissal items

head(emo.att.el)
dim(emo.att.el)

View(emo.att.el)

table(emo.att.el$person, useNA = "ifany") / dim(emo.att.el)[1]

###If linked to a to.id, impute person type to School friend
emo.att.el[which(is.na(emo.att.el$person) & emo.att.el$to.id != -205),]

emo.att.el$person[which(is.na(emo.att.el$person) & emo.att.el$to.id != -205)] <- "School Friend"


####Drop row missing any info
emo.att.el <- emo.att.el[which(!is.na(emo.att.el$person) & (emo.att.el$dismiss +emo.att.el$support != 0 )),] 

table(emo.att.el$person, useNA = "ifany")


round(table(emo.att.el$person, useNA = "ifany") / dim(emo.att.el)[1] * 100,2)

length(unique(emo.att.el$respondent_id))

#install.packages("psych")
library(psych)
describeBy(emo.att.el, group = emo.att.el$ego.male)



table(emo.att.el$jokes,emo.att.el$dismiss)
table(emo.att.el$subject,emo.att.el$dismiss)
table(emo.att.el$jokes,emo.att.el$subject)

x <- describe(emo.att.el)

x[,c("min","mean", "sd","max")]

hist(emo.att.el$dismiss)
hist(emo.att.el$support)
hist(emo.att.el$emotions)




head(emo.att.el)
table(emo.att.el$dismiss, emo.att.el$respondent_id)
table(emo.att.el$respondent_id,emo.att.el$dismiss)

fit1 <- lmer(dismiss ~ (1|respondent_id), data = emo.att.el, REML=T) # fit a var. components model
summary(fit1)
fit <- fit1
u0 <- ranef(fit, condVar = TRUE) 
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

level2ID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("level2ID" = level2ID, "u0" = u0[[1]], "u0se" = u0se)

colnames(u0tab)[2] <- "u0"

u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$level2ID), ]
colnames(u0tab)[4] <- "u0rank"

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for comm_id:_cons", ylim = c(-5, 10))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")




fit1 <- lmer(dismiss ~ (1|respondent_id) + ego.male + alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit1)
tidy(fit1,conf.int=TRUE  ,effect="fixed") 

fit2 <- lmer(dismiss ~ (1|respondent_id) + ego.male * alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit2)
tidy(fit2,conf.int=TRUE  ,effect="fixed") 


fit3 <- lmer(dismiss ~  person + (1|respondent_id)-1 ,
             data = emo.att.el, REML=FALSE) 

#fit3 <- lmer(dismiss ~  factor(person) + (1|respondent_id)-1 ,
#             data = emo.att.el, REML=FALSE) 

summary(fit3)
tidy(fit3,conf.int=TRUE  ,effect="fixed") 


fit4 <- lmer(dismiss ~  person + ego.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 


fit4 <- lmer(dismiss ~  factor(person )  + (1|respondent_id),
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 


fit4 <- lmer(dismiss ~  person + ego.male + alter.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 




######################################
### Supportive interactions

fit1 <- lmer(support ~ (1|respondent_id), data = emo.att.el, REML=FALSE) # fit a var. components model
summary(fit1)

fit <- fit1
u0 <- ranef(fit, condVar = TRUE) 
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

level2ID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("level2ID" = level2ID, "u0" = u0[[1]], "u0se" = u0se)

colnames(u0tab)[2] <- "u0"

u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$level2ID), ]
colnames(u0tab)[4] <- "u0rank"

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for comm_id:_cons", ylim = c(-4, 4))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")

fit1 <- lmer(support ~ (1|respondent_id) + ego.male + alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit1)
tidy(fit1,conf.int=TRUE  ,effect="fixed") 

fit2 <- lmer(support ~ (1|respondent_id) + ego.male * alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit2)
tidy(fit2,conf.int=TRUE  ,effect="fixed") 


fit3 <- lmer(support ~  person + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 

fit3 <- lmer(support ~  factor(person) + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 

table(emo.att.el$person)

summary(fit3)
tidy(fit3,conf.int=TRUE  ,effect="fixed") 

fit4 <- lmer(support ~  person + ego.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 

fit4 <- lmer(support ~  person + ego.male + alter.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 


fit4 <- lmer(support ~  factor(person , exclude = "School Friend")  + (1|respondent_id),
             data = emo.att.el, REML=T) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 



##########################################
#   Sharing-coruminating interactions


fit1 <- lmer(emotions ~ (1|respondent_id), data = emo.att.el, REML=FALSE) # fit a var. components model
summary(fit1)

fit <- fit1
u0 <- ranef(fit, condVar = TRUE) 
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

level2ID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("level2ID" = level2ID, "u0" = u0[[1]], "u0se" = u0se)

colnames(u0tab)[2] <- "u0"

u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$level2ID), ]
colnames(u0tab)[4] <- "u0rank"

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for comm_id:_cons", ylim = c(-4, 4))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")

fit1 <- lmer(emotions ~ (1|respondent_id) + ego.male + alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit1)
tidy(fit1,conf.int=TRUE  ,effect="fixed") 

fit2 <- lmer(emotions ~ (1|respondent_id) + ego.male * alter.male,
             data = emo.att.el, REML=FALSE) 
summary(fit2)
tidy(fit2,conf.int=TRUE  ,effect="fixed") 


fit3 <- lmer(emotions ~  person + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit3)
tidy(fit3,conf.int=TRUE  ,effect="fixed") 

fit3.intercept <- lmer(emotions ~  person + (1|respondent_id),
             data = emo.att.el, REML=FALSE) 

fit3.intercept <- lmer(emotions ~  factor(person) + (1|respondent_id)-1,
                       data = emo.att.el, REML=FALSE) 


summary(fit3.intercept)
tidy(fit3.intercept,conf.int=TRUE  ,effect="fixed") 

fit4 <- lmer(emotions ~  person + ego.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 




fit4 <- lmer(emotions ~  person + ego.male + (1|respondent_id),
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 



fit4 <- lmer(emotions ~  person + ego.male + alter.male + (1|respondent_id)-1,
             data = emo.att.el, REML=FALSE) 
summary(fit4)
tidy(fit4,conf.int=TRUE  ,effect="fixed") 

####################################################
##      Table for briefing
###################################################
sup.fit4.intercept <- lmer(support ~  person  + (1|respondent_id),
                           data = emo.att.el, REML=FALSE) 
summary(sup.fit4.intercept)
tidy(sup.fit4.intercept,conf.int=TRUE  ,effect="fixed") 

##Models above:
#   Marginal evidence of difference lower support from friends compared to Family

sup.fit4.only.egomale <- lmer(support ~ ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 
summary(sup.fit4.intercept.egomale)
tidy(sup.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 


sup.fit4.intercept.egomale <- lmer(support ~  person + ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 
summary(sup.fit4.intercept.egomale)
tidy(sup.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 

###Boys less likely to report support, independent of who they spoke to


emot.fit4.intercept <- lmer(emotions ~  person  + (1|respondent_id),
                           data = emo.att.el, REML=FALSE) 
summary(emot.fit4.intercept)
tidy(emot.fit4.intercept,conf.int=TRUE  ,effect="fixed") 

####More likely to report emotion sharing with friends in and outside school, 
#   compared to family

emot.fit4.intercept.egomale <- lmer(emotions ~  ego.male + (1|respondent_id),
                                    data = emo.att.el, REML=FALSE) 


emot.fit4.intercept.egomale <- lmer(emotions ~  person + ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 
summary(emot.fit4.intercept.egomale)
tidy(emot.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 

####Boys less likely to report emotion sharing, independent of who they speak to

dis.fit4.intercept <- lmer(dismiss ~  person  + (1|respondent_id),
                           data = emo.att.el, REML=FALSE) 
summary(dis.fit4.intercept)
tidy(dis.fit4.intercept,conf.int=TRUE  ,effect="fixed") 

####School friends slightly more likely than parents to report dismissal

dis.fit4.intercept.egomale <- lmer(dismiss ~  
                                     ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 

dis.fit4.intercept.egomale <- lmer(dismiss ~  person + ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 
summary(dis.fit4.intercept.egomale)
tidy(dis.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 


##Boys more likely to report dismissal. 

##Check with individual items

joke.fit4.intercept <- lmer(jokes ~  person  + (1|respondent_id),
                           data = emo.att.el, REML=FALSE) 

tidy(joke.fit4.intercept,conf.int=TRUE  ,effect="fixed") 

####No difference in jokes

joke.fit4.intercept.egomale <- lmer(jokes ~  ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 

joke.fit4.intercept.egomale <- lmer(jokes ~  person + ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 

tidy(joke.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 


##Boys more likely to report jokes 


subj.fit4.intercept <- lmer(subject ~  person  + (1|respondent_id),
                           data = emo.att.el, REML=FALSE) 

tidy(subj.fit4.intercept,conf.int=TRUE  ,effect="fixed") 

####No difference in jokes

subj.fit4.intercept.egomale <- lmer(subject ~  
                                     ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 

subj.fit4.intercept.egomale <- lmer(subject ~  person + ego.male + (1|respondent_id),
                                   data = emo.att.el, REML=FALSE) 

tidy(subj.fit4.intercept.egomale,conf.int=TRUE  ,effect="fixed") 


##Boys more likely to report jokes 





#############

sup.fit4  <- lmer(support ~  person  + (1|respondent_id)-1,
                                   data = emo.att.el, REML=FALSE) 

emot.fit4 <- lmer(emotions ~  person + (1|respondent_id)-1,
                                    data = emo.att.el, REML=FALSE) 

#dis.fit4  <- lmer(dismiss ~  person  + (1|respondent_id)-1,
  #                data = emo.att.el, REML=FALSE) 

joke.fit4  <- lmer(jokes ~  person  + (1|respondent_id)-1,
                  data = emo.att.el, REML=FALSE) 

subj.fit4  <- lmer(subject ~  person  + (1|respondent_id)-1,
                   data = emo.att.el, REML=FALSE) 






sup.tbl  <- tidy(sup.fit4,conf.int=TRUE  ,effect="fixed") 
emot.tbl <- tidy(emot.fit4,conf.int=TRUE  ,effect="fixed") 
dis.tbl  <- tidy(dis.fit4,conf.int=TRUE  ,effect="fixed") 
joke.tbl  <- tidy(joke.fit4,conf.int=TRUE  ,effect="fixed") 
subj.tbl  <- tidy(subj.fit4,conf.int=TRUE  ,effect="fixed") 



oneway.test(emotions ~ person, data = emo.att.el)

install.packages("data.table")

library(data.table)

dt <- data.table(emo.att.el)

sup <- dt[,list(n=.N,
                mean=round(mean(support),2),
                sd=round(sd(support),2),
                mean=round(mean(emotions),2),
                sd=round(sd(emotions),2),
                mean=round(mean(jokes),2),
                sd=round(sd(jokes),2),
                mean=round(mean(subject),2),
                sd=round(sd(subject),2)),
          ,by=person]


dis.tbl <- dt[,list(n=.N,
                mean=round(mean(support),2),
                sd=round(sd(support),2),
                mean=round(mean(emotions),2),
                sd=round(sd(emotions),2),
                mean=round(mean(dismiss),2),
                sd=round(sd(dismiss),2)),
          ,by=person]

dis.tbl
dis.tbl <- dt[,list(n=.N,
                    mean=round(mean(support),2),
                    sd=round(sd(support),2),
                    mean=round(mean(emotions),2),
                    sd=round(sd(emotions),2),
                    mean=round(mean(dismiss),2),
                    sd=round(sd(dismiss),2)),
              ,by=ego.male]
dis.tbl

emo.att.el

sup.tbl
emot.tbl
dis.tbl
joke.tbl
subj.tbl

###################################################################################
# check missing
###################################################################################

test.dfCheck <- apply(test.df, 2, function(x) ifelse(x == " " | x == "", NA, x))

missing <- list()

for (i in 1: ncol(test.dfCheck)) {
  temp <- list(table(is.na(test.dfCheck[,i])))
  missing <- append(missing, temp)
}

names(missing) <- colnames(test.dfCheck)
# if TRUE is missing, if FALSE it's there


# how to check missingness 

missing[139] # by col number

missing[1:100]
missing[100:300]


###Plots for var missingness
library(mice)


######
md.pattern(test.df[,1:100])
md.pattern(test.df[,100:120])
md.pattern(test.df[,200:300])
md.pattern(test.df[,300:400])
md.pattern(test.df[,440:450])
table(test.df$q_71_a)

table(test.df$)
table(test.df$q_net_friend_4_hobby)


missing$last_updated.2 # by col name 
table(test.df$q_net_friend_1_music)

#####################################################################################

# Print text variables in case they are going to kill themselves

opentext <- paste(test.df$respondent_id,
                  test.df$q_6_other,
                  test.df$q_8_c_other,
                  test.df$q_8_d_other,
                  test.df$q_8_e_other,
                  test.df$q_8_f_other,
                  test.df$q_8_g_other,
                  test.df$q_8_h_other,
                  test.df$q_8_i_other,
                  test.df$q_10_disability,
                  test.df$q_11_religion,
                  test.df$q_12_other,
                  test.df$q_12b_j_other,
                  test.df$q_12b_k_other,
                  test.df$q_15_other,
                  test.df$q_26_g_text,
                  test.df$q_27_l_text,
                  test.df$q_29_i_text,
                  test.df$q_33_i_text,
                  test.df$q_38_g_text,
                  test.df$q_39_o_text,
                  test.df$q_47_ac_text,
                  test.df$q_48_v_text,
                  test.df$q_54_smoke,
                  test.df$q_61b_g_who,
                  test.df$q_73_text,
                  test.df$q_77_comment,
                  test.df$q_78_comment,
                  test.df$q_80_other,
                  test.df$q_net_friend_people_you_dont_like_1_reason,
                  test.df$q_net_friend_people_you_dont_like_2_reason,
                  test.df$q_net_friend_people_you_dont_like_3_reason,
                  test.df$q_net_friend_1_other_text_1,
                  test.df$q_net_friend_1_other_text_2,
                  test.df$q_net_friend_1_other_text_3,
                  test.df$q_net_friend_2_other_text_1,
                  test.df$q_net_friend_2_other_text_2,
                  test.df$q_net_friend_2_other_text_3,
                  test.df$q_net_friend_3_other_text_1,
                  test.df$q_net_friend_3_other_text_2,
                  test.df$q_net_friend_3_other_text_3,
                  test.df$q_net_friend_4_other_text_1,
                  test.df$q_net_friend_4_other_text_2,
                  test.df$q_net_friend_4_other_text_3,
                  test.df$q_net_friend_5_other_text_1,
                  test.df$q_net_friend_5_other_text_2,
                  test.df$q_net_friend_5_other_text_3,
                  test.df$q_net_friend_6_other_text_1,
                  test.df$q_net_friend_6_other_text_2,
                  test.df$q_net_friend_6_other_text_3,
                  test.df$q_net_friend_7_other_text_1,
                  test.df$q_net_friend_7_other_text_2,
                  test.df$q_net_friend_7_other_text_3,
                  test.df$q_net_friend_8_other_text_1,
                  test.df$q_net_friend_8_other_text_2,
                  test.df$q_net_friend_8_other_text_3,
                  test.df$q_net_friend_9_other_text_1,
                  test.df$q_net_friend_9_other_text_2,
                  test.df$q_net_friend_9_other_text_3,
                  test.df$q_net_friend_10_other_text_1,
                  test.df$q_net_friend_10_other_text_2,
                  test.df$q_net_friend_10_other_text_3,
                  test.df$q_net_friend_1_together_text_1,
                  test.df$q_net_friend_1_together_text_2,
                  test.df$q_net_friend_1_together_text_3,
                  test.df$q_net_friend_2_together_text_1,
                  test.df$q_net_friend_2_together_text_2,
                  test.df$q_net_friend_2_together_text_3,
                  test.df$q_net_friend_3_together_text_1,
                  test.df$q_net_friend_3_together_text_2,
                  test.df$q_net_friend_3_together_text_3,
                  test.df$q_net_friend_4_together_text_1,
                  test.df$q_net_friend_4_together_text_2,
                  test.df$q_net_friend_4_together_text_3,
                  test.df$q_net_friend_5_together_text_1,
                  test.df$q_net_friend_5_together_text_2,
                  test.df$q_net_friend_5_together_text_3,
                  test.df$q_net_friend_6_together_text_1,
                  test.df$q_net_friend_6_together_text_2,
                  test.df$q_net_friend_6_together_text_3,
                  test.df$q_net_friend_7_together_text_1,
                  test.df$q_net_friend_7_together_text_2,
                  test.df$q_net_friend_7_together_text_3,
                  test.df$q_net_friend_8_together_text_1,
                  test.df$q_net_friend_8_together_text_2,
                  test.df$q_net_friend_8_together_text_3,
                  test.df$q_net_friend_9_together_text_1,
                  test.df$q_net_friend_9_together_text_2,
                  test.df$q_net_friend_9_together_text_3,
                  test.df$q_net_friend_10_together_text_1,
                  test.df$q_net_friend_10_together_text_2,
                  test.df$q_net_friend_10_together_text_3,
                  test.df$q_net_friend_1_mood_text_1,
                  test.df$q_net_friend_1_mood_text_2,
                  test.df$q_net_friend_1_mood_text_3,
                  test.df$q_net_friend_2_mood_text_1,
                  test.df$q_net_friend_2_mood_text_2,
                  test.df$q_net_friend_2_mood_text_3,
                  test.df$q_net_friend_3_mood_text_1,
                  test.df$q_net_friend_3_mood_text_2,
                  test.df$q_net_friend_3_mood_text_3,
                  test.df$q_net_friend_1_you_respond_mood_text_1,
                  test.df$q_net_friend_1_you_respond_mood_text_2,
                  test.df$q_net_friend_1_you_respond_mood_text_3,
                  test.df$q_net_friend_2_you_respond_mood_text_1,
                  test.df$q_net_friend_2_you_respond_mood_text_2,
                  test.df$q_net_friend_2_you_respond_mood_text_3,
                  test.df$q_net_friend_3_you_respond_mood_text_1,
                  test.df$q_net_friend_3_you_respond_mood_text_2,
                  test.df$q_net_friend_3_you_respond_mood_text_3,
                  test.df$q_gangs_text,
                  test.df$q_other_gangs_text,
                  test.df$q_net_friend_1_same_clubs_other_text,
                  test.df$q_net_friend_2_same_clubs_other_text,
                  test.df$q_net_friend_3_same_clubs_other_text,
                  test.df$q_net_friend_4_same_clubs_other_text,
                  test.df$q_net_friend_5_same_clubs_other_text,
                  test.df$q_net_friend_6_same_clubs_other_text,
                  test.df$q_net_friend_7_same_clubs_other_text,
                  test.df$q_net_friend_8_same_clubs_other_text,
                  test.df$q_net_friend_9_same_clubs_other_text,
                  test.df$q_net_friend_10_same_clubs_other_text, sep = "-")




summary(test.df)
opentext[157:303]


one <- test.df[57,]

length(table(test.df$q_net_friend_1_hidden_id, useNA = "ifany"))

#####################################################################################

# Separate networks using var respondent_school

# year two 

colnames(test.df[,980:1020])

summary(test.df) 

drop <- c()



YearTwo <- test.df[test.df$respondent_school == "22", ]
YearFour <- test.df[test.df$respondent_school == "24", ]

# ALTERNATIVE
# Separate networks using var respondent_id

YearTwo <- test.df[grepl("WHS2", test.df$respondent_id), ]
YearFour <- test.df[grepl("WHS4", test.df$respondent_id), ]






# select colums to make the network Year Two

###Melt data into  edgelist based on edges only
edge.dfY2 <- YearTwo[,c("id",
                        "q_net_friend_1_hidden_id",
                        "q_net_friend_2_hidden_id",
                        "q_net_friend_3_hidden_id",
                        "q_net_friend_4_hidden_id",
                        "q_net_friend_5_hidden_id",
                        "q_net_friend_6_hidden_id",
                        "q_net_friend_7_hidden_id",
                        "q_net_friend_8_hidden_id",
                        "q_net_friend_9_hidden_id",
                        "q_net_friend_10_hidden_id")]

# make year two network

edgeY2 <- data.frame()
temp <- data.frame()
for (i in 2:ncol(edge.dfY2)) {
  temp <- cbind(edge.dfY2[, 1], edge.dfY2[, i])
  edgeY2 <- rbind(edgeY2, temp)
}
colnames(edgeY2) <- c("respondent_id", "alter")
edgeY2$alter <- as.character(edgeY2$alter)
edgeY2$alter <- ifelse(edgeY2$alter == "" | edgeY2$alter == "0", NA, edgeY2$alter)

edgecleanY2 <- edgeY2[which(!is.na(edgeY2$alter)),] 
edgecleanY2$respondent_id <- as.numeric(edgecleanY2$respondent_id)
edgecleanY2$alter <- as.numeric(edgecleanY2$alter)

test.netY2 <-network(edgecleanY2,matrix.type='edgelist',ignore.eval=FALSE)

plot(test.netY2)

library(texreg)

summary(test.netY2)
test.netY2


# select colums to make the network Year Two

###Melt data into  edgelist based on edges only
edge.dfY4 <- YearFour[,c("id",
                         "q_net_friend_1_hidden_id",
                         "q_net_friend_2_hidden_id",
                         "q_net_friend_3_hidden_id",
                         "q_net_friend_4_hidden_id",
                         "q_net_friend_5_hidden_id",
                         "q_net_friend_6_hidden_id",
                         "q_net_friend_7_hidden_id",
                         "q_net_friend_8_hidden_id",
                         "q_net_friend_9_hidden_id",
                         "q_net_friend_10_hidden_id")]

# make year four network

edgeY4 <- data.frame()
temp <- data.frame()
for (i in 2:ncol(edge.dfY4)) {
  temp <- cbind(edge.dfY4[, 1], edge.dfY4[, i])
  edgeY4 <- rbind(edgeY4, temp)
}
colnames(edgeY4) <- c("respondent_id", "alter")
edgeY4$alter <- as.character(edgeY4$alter)
table(edgeY4$alter, useNA = "ifany")
edgeY4$alter <- ifelse(edgeY4$alter == "\\N" | edgeY4$alter == "0", NA, edgeY4$alter)

edgecleanY4 <- edgeY4[which(!is.na(edgeY4$alter)),] 
edgecleanY4$respondent_id <- as.numeric(edgecleanY4$respondent_id)
edgecleanY4$alter <- as.numeric(edgecleanY4$alter)

test.netY4 <-network(edgecleanY4,matrix.type='edgelist',ignore.eval=FALSE)

plot(test.netY4)

summary(test.netY4)
test.netY4

#########################################################################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    +    +    +       CONTROL Net4Health   +    +    +    +    +    +    +
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#########################################################################


# ##Raw data
# raw.sch.list      <-list()
# raw.sch.el        <-list()
# school            <-list()
# net.sch           <-list()
# vert.attr         <-list()
# net               <-list()
# 
# 
# load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/recoded_control.rdata")
# 
# raw.sch.list <- list()
# for (i in 1:6){
#   raw.sch.list[[i]] <-  filter(recoded.control, respondent_school==i)
# }
# 
# lapply(raw.sch.list, function(x) dim(x))
# 
# 
#   ###Strip out all the variables I need for each school
# for (i in 1:6){
#   raw.sch.list[[i]] <- raw.sch.list[[i]][, c("respondent_id","q_1", "q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
#                                              "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", "q_19_friend_1_q_1", "q_19_friend_1_q_2", "q_19_friend_1_q_3", 
#                                              "q_19_friend_1_q_4", "q_19_friend_1_q_5", "q_19_friend_1_q_6", "q_19_friend_1_q_7",
#                                              "q_19_friend_2_q_1",	"q_19_friend_2_q_2", "q_19_friend_2_q_3", "q_19_friend_2_q_4", 	"q_19_friend_2_q_5", "q_19_friend_2_q_6", "q_19_friend_2_q_7", 
#                                              "q_19_friend_3_q_1",	"q_19_friend_3_q_2",	"q_19_friend_3_q_3",	"q_19_friend_3_q_4",	"q_19_friend_3_q_5",	"q_19_friend_3_q_6",	"q_19_friend_3_q_7",
#                                              "q_19_friend_4_q_1",	"q_19_friend_4_q_2",	"q_19_friend_4_q_3",	"q_19_friend_4_q_4",	"q_19_friend_4_q_5", 	"q_19_friend_4_q_6", 	"q_19_friend_4_q_7",
#                                              "q_19_friend_5_q_1",	"q_19_friend_5_q_2"	, "q_19_friend_5_q_3",	"q_19_friend_5_q_4",	"q_19_friend_5_q_5",	"q_19_friend_5_q_6",	"q_19_friend_5_q_7",
#                                              "q_19_friend_6_q_1",	"q_19_friend_6_q_2",	"q_19_friend_6_q_3",	"q_19_friend_6_q_4",	"q_19_friend_6_q_5",	"q_19_friend_6_q_6",	"q_19_friend_6_q_7"
#   )]
# }
# 
# # rename columns
# for (i in 1:6){
#   colnames(raw.sch.list[[i]]) <- c("respondent_id", "respondent_sex","q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
#                                    "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", 
#                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
#                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
#                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
#                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
#                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
#                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
#                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
#                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
#                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
#                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
#                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
#                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
# }                                 
# colnames(raw.sch.list[[1]])                               
# 
# # Set missing values to NA
# for (i in 1:6){
#   is.na(raw.sch.list[[i]]) <- !raw.sch.list[[i]]
# } 
# 
# raw.control.network.questions <- raw.sch.list
# 
# save(raw.control.network.questions, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_raw_net_qs.rdata")
# 
# #########################################################################
# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# #    +    +    +       Baseline Net4Health    +    +    +    +    +    +    +
# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 
# 
# ##Raw data
# raw.sch.list      <-list()
# raw.sch.el        <-list()
# school            <-list()
# net.sch           <-list()
# vert.attr         <-list()
# net               <-list()
# 
# load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/recoded_baseline.rdata")
# 
# raw.sch.list <- list()
# for (i in 1:6){
#   raw.sch.list[[i]] <-  filter(recoded.baseline, respondent_school==i)
# }
# 
# ###Strip out all the variables I need for each school
# for (i in 1:6){
#   raw.sch.list[[i]] <- raw.sch.list[[i]][, c("respondent_id","q_1", "q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
#                                              "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", "q_19_friend_1_q_1", "q_19_friend_1_q_2", "q_19_friend_1_q_3", 
#                                              "q_19_friend_1_q_4", "q_19_friend_1_q_5", "q_19_friend_1_q_6", "q_19_friend_1_q_7",
#                                              "q_19_friend_2_q_1",	"q_19_friend_2_q_2", "q_19_friend_2_q_3", "q_19_friend_2_q_4", 	"q_19_friend_2_q_5", "q_19_friend_2_q_6", "q_19_friend_2_q_7", 
#                                              "q_19_friend_3_q_1",	"q_19_friend_3_q_2",	"q_19_friend_3_q_3",	"q_19_friend_3_q_4",	"q_19_friend_3_q_5",	"q_19_friend_3_q_6",	"q_19_friend_3_q_7",
#                                              "q_19_friend_4_q_1",	"q_19_friend_4_q_2",	"q_19_friend_4_q_3",	"q_19_friend_4_q_4",	"q_19_friend_4_q_5", 	"q_19_friend_4_q_6", 	"q_19_friend_4_q_7",
#                                              "q_19_friend_5_q_1",	"q_19_friend_5_q_2"	, "q_19_friend_5_q_3",	"q_19_friend_5_q_4",	"q_19_friend_5_q_5",	"q_19_friend_5_q_6",	"q_19_friend_5_q_7",
#                                              "q_19_friend_6_q_1",	"q_19_friend_6_q_2",	"q_19_friend_6_q_3",	"q_19_friend_6_q_4",	"q_19_friend_6_q_5",	"q_19_friend_6_q_6",	"q_19_friend_6_q_7"
#   )]
# }
# 
# 
# dim(raw.sch.list[[1]])
# dim(raw.sch.list[[2]])
# dim(raw.sch.list[[3]])
# dim(raw.sch.list[[4]])
# dim(raw.sch.list[[5]])
# dim(raw.sch.list[[6]])
# 
# 
# 
# 
# # rename columns
# for (i in 1:6){
#   colnames(raw.sch.list[[i]]) <- c("respondent_id", "respondent_sex","q_19_friend_1_hidden_id", "q_19_friend_2_hidden_id", "q_19_friend_3_hidden_id",
#                                    "q_19_friend_4_hidden_id", "q_19_friend_5_hidden_id", "q_19_friend_6_hidden_id", 
#                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
#                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
#                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
#                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
#                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
#                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
#                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
#                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
#                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
#                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
#                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
#                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
# }                                 
# colnames(raw.sch.list[[1]])                               
# 
# # Set missing values to NA
# for (i in 1:6){
#   is.na(raw.sch.list[[i]]) <- !raw.sch.list[[i]]
# } 
# 
# 
# raw.baseline.network.questions <- raw.sch.list
# 
# save(raw.baseline.network.questions, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_raw_net_qs.rdata")
# 


#############################################################################
#
#             Networks with edge attributes 
#
#
#############################################################################
# for one school PILOT
colnames(test.df)

# block one 13 questions about each relationship:
# 1. "q_net_friend_1_current_affairs"  
# 2. "q_net_friend_1_music"            
# 3. "q_net_friend_1_school"           
# 4. "q_net_friend_1_other_pupils" 
# 5. "q_net_friend_1_other_people"     
# 6. "q_net_friend_1_sports"          
# 7. "q_net_friend_1_alchohol"         
# 8. "q_net_friend_1_relationships" 
# 9. "q_net_friend_1_keeping_fit"      
# 10. "q_net_friend_1_eating"           
# 11."q_net_friend_1_other_text_1"     
# 12. "q_net_friend_1_other_text_2"
# 13. "q_net_friend_1_other_text_3" 


# Year two
colnames(test.df)
E.attrY2 <- as.data.frame(YearTwo[, c(931:935,942:946,953:957)])

colnames(E.attrY2)

# recoding missing data and open text as NAs
E.attrY2 <- apply(E.attrY2, 2, function(x) ifelse(x != "0" &  x != "1", NA, x))

table(E.attrY2[,7], useNA = "ifany")
ncol(E.attrY2)

Themes.dfY2 <- NULL
temp <- NULL
n.items <- 5
n.friends <- 0:3
(progr <- n.items*n.friends)

# makes data frame with one column for each attribute
for (i in 1:5) {
  temp <- as.vector(E.attrY2[, i + progr])
  Themes.dfY2 <- data.frame(cbind(Themes.dfY2, temp))
}
Themes.dfY2 <- E.attrY2
# rename columns by theme

colnames(Themes.dfY2) <- c("q_net_friend_1_current_affairs",
                           "q_net_friend_1_music",            
                           "q_net_friend_1_school",          
                           "q_net_friend_1_other_pupils",
                           "q_net_friend_1_other_people",    
                           "q_net_friend_1_sports",         
                           "q_net_friend_1_alchohol",        
                           "q_net_friend_1_relationships", 
                           "q_net_friend_1_keeping_fit",      
                           "q_net_friend_1_eating",         
                           "q_net_friend_1_other_text_1",     
                           "q_net_friend_1_other_text_2",
                           "q_net_friend_1_other_text_3" )

# c.bind edge to each column of Themes.df

# separated in list form

# edgesAttrToLoad <- list()
# for (i in 1:ncol(Themes.df)) {
#   temp <- cbind(edge, Themes.df[, i])
#   edgesAttrToLoad[[i]] <- temp
# }
# 
# edgesAttrToLoad[[1]]
# 
# 
# i <-1 
# 
# 
# names(edgesAttrToLoad) <- c("q_net_friend_1_current_affairs",
#                             "q_net_friend_1_music",            
#                             "q_net_friend_1_school",          
#                             "q_net_friend_1_other_pupils",
#                             "q_net_friend_1_other_people",    
#                             "q_net_friend_1_sports",         
#                             "q_net_friend_1_alchohol",        
#                             "q_net_friend_1_relationships", 
#                             "q_net_friend_1_keeping_fit",      
#                             "q_net_friend_1_eating",         
#                             "q_net_friend_1_other_text_1",     
#                             "q_net_friend_1_other_text_2",
#                             "q_net_friend_1_other_text_3" )


# to be loaded in block 
attributes.edgelistY2 <- cbind(edgeY2, Themes.dfY2)
attributes.edgelistY2 <- attributes.edgelistY2[!is.na(attributes.edgelistY2$alter), ]

att.netY2 <-network(attributes.edgelistY2,matrix.type='edgelist',ignore.eval=FALSE)

install.packages("intergraph")
library(intergraph)

plot.network(att.netY2)


h <- asIgraph(att.netY2)


library(igraph)

V(h)$size <- degree(h)

V(net.igraph)

rescale = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

V(h)$size <- rescale(degree(h),
                     min(degree(h)),
                     max(degree(h)),
                     1,
                     15 )

plot(as.undirected(h))

ClusterH <- cluster_louvain(as.undirected(h))

plot(ClusterH, as.undirected(h),
     #vertex.label=  ifelse(V(df.graph)$color =="red","X",""),
     vertex.size = degree(h)*0.5, 
     edge.color="grey", 
     edge.width= 0.1,
     sub= "Clusters", 
     margin = 0)

################
# Year four    #
################
colnames(test.df)
E.attrY4 <- as.data.frame(YearFour[, 678:807])


# recoding missing data and open text as NAs
E.attrY4 <- apply(E.attrY4, 2, function(x) ifelse(x != "0" &  x != "1", NA, x))

Themes.dfY4 <- NULL
temp <- NULL
n.items <- 13
n.friends <- 0:9
(progr <- n.items*n.friends)

# makes data frame with one column for each attribute
for (i in 1:13) {
  temp <- as.vector(E.attrY4[, i + progr])
  Themes.dfY4 <- data.frame(cbind(Themes.dfY4, temp))
}

# rename columns by theme

colnames(Themes.dfY4) <- c("q_net_friend_1_current_affairs",
                           "q_net_friend_1_music",            
                           "q_net_friend_1_school",          
                           "q_net_friend_1_other_pupils",
                           "q_net_friend_1_other_people",    
                           "q_net_friend_1_sports",         
                           "q_net_friend_1_alchohol",        
                           "q_net_friend_1_relationships", 
                           "q_net_friend_1_keeping_fit",      
                           "q_net_friend_1_eating",         
                           "q_net_friend_1_other_text_1",     
                           "q_net_friend_1_other_text_2",
                           "q_net_friend_1_other_text_3" )

# c.bind edge to each column of Themes.df

# separated in list form

# edgesAttrToLoad <- list()
# for (i in 1:ncol(Themes.df)) {
#   temp <- cbind(edge, Themes.df[, i])
#   edgesAttrToLoad[[i]] <- temp
# }
# 
# edgesAttrToLoad[[1]]
# 
# 
# i <-1 
# 
# 
# names(edgesAttrToLoad) <- c("q_net_friend_1_current_affairs",
#                             "q_net_friend_1_music",            
#                             "q_net_friend_1_school",          
#                             "q_net_friend_1_other_pupils",
#                             "q_net_friend_1_other_people",    
#                             "q_net_friend_1_sports",         
#                             "q_net_friend_1_alchohol",        
#                             "q_net_friend_1_relationships", 
#                             "q_net_friend_1_keeping_fit",      
#                             "q_net_friend_1_eating",         
#                             "q_net_friend_1_other_text_1",     
#                             "q_net_friend_1_other_text_2",
#                             "q_net_friend_1_other_text_3" )


# to be loaded in block 
attributes.edgelistY4 <- cbind(edgeY4, Themes.dfY4)

test <- c(edgeY4$respondent_id, edgeY4$alter)
length(unique(test))

attributes.edgelistY4 <- attributes.edgelistY4[!is.na(attributes.edgelistY4$alter), ]

att.netY4 <-network(attributes.edgelistY4,matrix.type='edgelist',ignore.eval=FALSE)
class(att.netY4)


plot.network(att.netY4)


plot.network(att.netY4)


h <- asIgraph(att.netY4)

class(h)
library(intergraph)

V(h)$size <- degree(h)

plot(delete_vertices(simplify(h), degree(h)==0))



rescale = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

V(h)$size <- rescale(degree(h),
                     min(degree(h)),
                     max(degree(h)),
                     1,
                     15 )

plot(as.undirected(h))
plot(as.undirected(delete_vertices(simplify(h), degree(h)==0)))

ClusterH <- cluster_louvain(as.undirected(delete_vertices(simplify(h), degree(h)==0)))

plot(ClusterH, as.undirected(delete_vertices(simplify(h), degree(h)==0)),
     #vertex.label=  ifelse(V(df.graph)$color =="red","X",""),
     vertex.size = degree(h)*0.5, 
     edge.color="grey", 
     edge.width= 0.1,
     sub= "Clusters", 
     margin = 0)


table(degree(h))

