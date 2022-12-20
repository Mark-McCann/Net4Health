rm(list = ls())

#################
#               #
#      Notes    #
#               #
#################

# Mark McCann developed this script

# Search for three asterisks to find things to be checked ***

# The edge attributes haven't been added. 


# ***  Still contains a lot of visualisations that should live in a separate script 

#############
#  Purpose  #
#############

 # prepare network data from coded data

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(network)
library(dplyr)
library(intergraph)

#########################
#                       #
#     Load functions    #
#                       #
#########################

#####Function to create network objects, requires the data frame and the relevant columns from the adjacency list
get.plex <- function(file = NULL, edges = c("q_net_friend_1_hidden_id",
                                            "q_net_friend_2_hidden_id",
                                            "q_net_friend_3_hidden_id")){
  df <- file
  ###Melt data into  edgelist based on edges only
  edge.df <- df[,c("id", edges)]
  # make network
  edge <- data.frame()
  temp <- data.frame()
  
  for (i in 2:ncol(edge.df)) {
    temp <- cbind(edge.df[, 1], edge.df[, i])
    edge <- rbind(edge, temp)
  }
  colnames(edge) <- c("respondent_id", "alter")
  edge$alter <- as.character(edge$alter)
  edge$alter <- ifelse(edge$alter == "" | edge$alter == "0", NA, edge$alter)
  edgeclean <- edge[which(!is.na(edge$alter)),] 
  edgeclean$respondent_id <- as.numeric(edgeclean$respondent_id)
  edgeclean$alter <- as.numeric(edgeclean$alter)
  ####Removing 1s, not sure why the 1s appeared to begin with, check please ***
  edgeclean <- edgeclean[which(!edgeclean$alter == 1),] 
  edgeclean <- edgeclean[which(!edgeclean$alter == 0),] 
  net <-network(edgeclean,matrix.type='edgelist',ignore.eval=FALSE)
return(net)
}


#########################
#                       #
#  Main body of script  #
#                       #
#########################

setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")
#setwd("/home/claudia/Desktop/Net4healthTaken")

setwd("C:/Users/mmc78h/Documents/A Work/Net4Health/Data")

test.df <- read.csv("N4H extract 12-02-2020 3 - Anonymised.csv", stringsAsFactors = FALSE)


#####################################################################################
# recode missing to NA

test.df <- as.data.frame(apply(test.df, 2, function(x) ifelse(x == " " | x == "" | x == 0, NA, x)))
# Separate networks using var respondent_school

# year two 

YearTwo <- test.df[test.df$respondent_school == "22", ]
YearFour <- test.df[test.df$respondent_school == "24", ]

# ALTERNATIVE
# Separate networks using var respondent_id

YearTwo <- test.df[grepl("WHS2", test.df$respondent_id), ]
YearFour <- test.df[grepl("WHS4", test.df$respondent_id), ]


#########


friend.3names <- get.plex(YearFour)
friend.10names <- get.plex(YearFour, edges = c("q_net_friend_1_hidden_id",
                                               "q_net_friend_2_hidden_id",
                                               "q_net_friend_3_hidden_id",
                                               "q_net_friend_4_hidden_id",
                                               "q_net_friend_5_hidden_id",
                                               "q_net_friend_6_hidden_id",
                                               "q_net_friend_7_hidden_id",
                                               "q_net_friend_8_hidden_id",
                                               "q_net_friend_9_hidden_id",
                                               "q_net_friend_10_hidden_id"))



emo.seek    <- get.plex(YearFour, edges = c("q_net_friend_emotional_support_outward_1_hidden_id",
                                         "q_net_friend_emotional_support_outward_2_hidden_id",
                                         "q_net_friend_emotional_support_outward_3_hidden_id"))

emo.provide <- get.plex(YearFour, edges = c("q_net_friend_emotional_support_inward_1_hidden_id",
                                            "q_net_friend_emotional_support_inward_2_hidden_id",
                                            "q_net_friend_emotional_support_inward_3_hidden_id") )

trust       <- get.plex(YearFour, edges = c("q_net_friend_trust_1_hidden_id",
                                            "q_net_friend_trust_2_hidden_id",
                                            "q_net_friend_trust_3_hidden_id"))

grades      <- get.plex(YearFour, edges = c("q_net_friend_best_grades_pals_ladder_1_hidden_id",
                                            "q_net_friend_best_grades_pals_ladder_2_hidden_id",
                                            "q_net_friend_best_grades_pals_ladder_3_hidden_id"))

respect     <- get.plex(YearFour, edges = c("q_net_friend_respect_pals_ladder_1_hidden_id",
                                             "q_net_friend_respect_pals_ladder_2_hidden_id",
                                             "q_net_friend_respect_pals_ladder_3_hidden_id"))

popular     <- get.plex(YearFour, edges = c("q_net_friend_pals_ladder_1_hidden_id",
                                            "q_net_friend_pals_ladder_2_hidden_id",
                                            "q_net_friend_pals_ladder_3_hidden_id"))

own.gang   <- get.plex(YearFour, edges = c("q_gangs_person_1_hidden_id","q_gangs_person_2_hidden_id"))

other.gang   <- get.plex(YearFour, edges = c("q_other_gangs_person_1_hidden_id","q_other_gangs_person_2_hidden_id"))

all.gang   <- get.plex(YearFour, edges = c("q_gangs_person_1_hidden_id",
                                           "q_gangs_person_2_hidden_id",
                                           "q_other_gangs_person_1_hidden_id",
                                           "q_other_gangs_person_2_hidden_id"))


dont.like <- get.plex(YearFour, edge = c("q_net_friend_people_you_dont_like_1_hidden_id",
                                         "q_net_friend_people_you_dont_like_2_hidden_id",
                                         "q_net_friend_people_you_dont_like_3_hidden_id") )



friend.3names.net     <- asIgraph(friend.3names)
friend.10names.net    <- asIgraph(friend.10names)
emo.seek.net          <- asIgraph(emo.seek)
emo.provide.net       <- asIgraph(emo.provide)
trust.net             <- asIgraph(trust)
grades.net            <- asIgraph(grades)
respect.net           <- asIgraph(respect)
popular.net           <- asIgraph(popular)
own.gang.net          <- asIgraph(own.gang)
other.gang.net        <- asIgraph(other.gang)
all.gang.net          <- asIgraph(all.gang)
dont.like.net          <- asIgraph(dont.like)

library(igraph)


par(mfrow = c(2,2))
par(mfrow = c(1,1))

lay <- layout_with_fr(friend.3names.net)


########################################

#   Each graph with its own FR layout  #

########################################

lay <- layout_with_fr(friend.10names.net)

par(mfrow = c(3,3), mar=c(0,0,1,0)+.1)

plot.igraph(friend.3names.net,  vertex.label = "",vertex.size=3,
            edge.width = 2,
            edge.arrow.size = 0.2,
            edge.arrow.width = 0.8,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "Friends - 3 names")

plot(friend.10names.net,  vertex.label = "",vertex.size=3,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Friends")

plot(emo.seek.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - Seeking")

plot(emo.provide.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - providing")

plot(trust.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Trust")

plot(grades.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "grades")

plot(respect.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Respect")

plot(popular.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Popular")

plot(own.gang.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Own Gang")

plot(other.gang.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Other Gang")

plot(all.gang.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "All Gang")

plot(dont.like.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "All Gang")





##########################################

#   All graphs with 10 friend FR layout  #

##########################################

par(mfrow = c(3,3), mar=c(0,0,1,0)+.1)

lay <- layout_with_fr(friend.10names.net)

plot.igraph(friend.3names.net, layout = lay, vertex.label = "",vertex.size=3,
            edge.width = 2,
            edge.arrow.size = 0.2,
            edge.arrow.width = 0.8,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "Friends - 3 names")

plot(friend.10names.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Friends")

plot(emo.seek.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - Seeking")

plot(emo.provide.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - providing")

plot(trust.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Trust")

plot(grades.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "grades")

plot(respect.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Respect")

plot(popular.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Popular")

plot(own.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Own Gang")

plot(other.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Other Gang")

plot(all.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "All Gang")

plot(dont.like.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Don't get on with")







##########################################

#   All graphs with emo seek layout  #

##########################################

lay <- layout_with_fr(emo.seek.net)

plot.igraph(friend.3names.net, layout = lay, vertex.label = "",vertex.size=3,
            edge.width = 2,
            edge.arrow.size = 0.2,
            edge.arrow.width = 0.8,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "Friends - 3 names")

plot(friend.10names.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Friends")

plot(emo.seek.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - Seeking")

plot(emo.provide.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Emotion - providing")

plot(trust.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Trust")

plot(grades.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "grades")

plot(respect.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Respect")

plot(popular.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Popular")

plot(own.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Own Gang")

plot(other.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Other Gang")

plot(all.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "All Gang")

plot(dont.like.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "Don't get on with")




















###########################Blinded version for quiz


##########################################

#   All graphs with 10 friend FR layout  #

##########################################

lay <- layout_with_fr(friend.10names.net)

par(mfrow = c(3,3), mar=c(0,0,1,0)+.1)

plot.igraph(friend.3names.net, layout = lay, vertex.label = "",vertex.size=3,
            edge.width = 1,
            edge.arrow.size = 0.02,
            edge.arrow.width = 0.05,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "A")

plot(emo.seek.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "B")

plot(emo.provide.net, layout = lay, vertex.label = "",vertex.size=4,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "C")

plot(trust.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "D")

plot(grades.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "E")

plot(respect.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "F")

plot(popular.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "G")

plot(own.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "H")

plot(other.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "I")


##########################################

#   All graphs with emo seek layout  #

##########################################

lay <- layout_with_fr(emo.seek.net)

par(mfrow = c(3,3), mar=c(0,0,1,0)+.1)


plot(emo.seek.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "1")


plot(trust.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "2")

plot(grades.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "3")


plot.igraph(friend.3names.net, layout = lay, vertex.label = "",vertex.size=3,
            edge.width = 1,
            edge.arrow.size = 0.02,
            edge.arrow.width = 0.05,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "4")

plot(popular.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "5")



plot(emo.provide.net, layout = lay, vertex.label = "",vertex.size=4,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "6")


plot(own.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "7")


plot(respect.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "8")



plot(other.gang.net, layout = lay, vertex.label = "",vertex.size=3,
     edge.width = 1,
     edge.arrow.size = 0.02,
     edge.arrow.width = 0.05,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "9")



########################################

#   Each graph with its own FR layout  #

########################################

par(mfrow = c(3,3), mar=c(0,0,1,0)+.1)


plot.igraph(friend.3names.net,  vertex.label = "",vertex.size=3,
            edge.width = 2,
            edge.arrow.size = 0.2,
            edge.arrow.width = 0.8,
            vertex.color = "dark gray",
            edge.color = "black",
            main = "A")


plot(emo.seek.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "B")

plot(emo.provide.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "C")

plot(trust.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "D")

plot(grades.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "E")

plot(respect.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "F")

plot(popular.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "G")

plot(own.gang.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "H")

plot(other.gang.net,  vertex.label = "",vertex.size=3,
     edge.width = 2,
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.8,
     vertex.color = "dark gray",
     edge.color = "black",
     main = "I")





########################################
#                                      #
#                                      #
#             Friendships              #
#                                      #
#                                      #
########################################
###    ****Still to be added  

####Add in edge attributes  - Not in this school year, Gender , older younger

YearFour$q_net_friend_1_how_close
YearFour$q_net_friend_10_how_long
YearFour$q_net_friend_1_hang_out
YearFour$q_net_friend_1_weekend
YearFour$q_net_friend_1_internet
YearFour$q_net_friend_1_social_media
YearFour$q_net_friend_1_listen_music
YearFour$q_net_friend_1_school
YearFour$q_net_friend_1_internet
YearFour$q_net_friend_1_play_board_games
YearFour$q_net_friend_1_same_clubs_yes_no
YearFour$q_net_friend_1_same_clubs_activity_1
YearFour$q_net_friend_1_same_clubs_activity_2
YearFour$q_net_friend_1_same_clubs_activity_3



#q_net_friend_10_school_year_1
#q_net_friend_10_gender_1
#q_net_friend_10_older_younger_1



#How close do you feel to them?
# q_close_10
  
  
#How long have you known them?
# q_net_friend_10_how_long


#How often do you and this friend hang out after school (when not at clubs or teams)?
#     q_net_friend_10_hang_out

  
# How often do you spend time with this friend on social media or online?
#    q_net_friend_10_social_media_1
 
 
# What do you and this friend talk about?
#q_net_friend_1_current_affairs
#q_net_friend_1_music
#q_net_friend_1_school
#q_net_friend_1_other_pupils
#q_net_friend_1_other_people
#q_net_friend_1_sports
#q_net_friend_1_alchohol
#q_net_friend_1_relationships
#q_net_friend_1_keeping_fit
#q_net_friend_1_eating
#q_net_friend_1_mental_health
#q_net_friend_1_day
#q_net_friend_1_thoughts
#q_net_friend_1_plans
#q_net_friend_1_other_text_1
#q_net_friend_1_other_text_2
#q_net_friend_1_other_text_3

  
# What do you and this friend do when you're together (when not at clubs or teams)?

# q_net_friend_1_listen_music
# q_net_friend_1_shops
# q_net_friend_1_clubs
# q_net_friend_1_cinema
# q_net_friend_1_books
# q_net_friend_1_hobby
# q_net_friend_1_scouts
# q_net_friend_1_watch_sports
# q_net_friend_1_street
# q_net_friend_1_nowhere
# q_net_friend_1_play_board_games
# q_net_friend_1_play_computer_games
# q_net_friend_1_internet
# q_net_friend_1_use_social_media
# q_net_friend_1_drink_alchohol
# q_net_friend_1_smoke_cigarettes
# q_net_friend_1_together_text_1
# q_net_friend_1_together_text_2
# q_net_friend_1_together_text_3



########################################
#                                      #
#                                      #
#   Providers of emotional support     #
#                                      #
#                                      #
########################################

# Edge attributes 

# Have you talked to them about emotions before? 

#  q_net_friend_emotional_support_outward_3_talk_1


#q_net_friend_1_support
#q_net_friend_1_emotions
#q_net_friend_1_makes_jokes
#q_net_friend_1_changes_subject
#q_net_friend_1_mood_text_1
#q_net_friend_1_mood_1
#q_net_friend_1_mood_text_2
#q_net_friend_1_mood_2
#q_net_friend_1_mood_text_3
#q_net_friend_1_mood_3

#How often do you talk to them about how you're feeling?

#q_net_friend_1_talk


########################################
#                                      #
#                                      #
#     Seekers of emotional support     #
#                                      #
#                                      #
########################################

#q_net_friend_1_you_respond_support
#q_net_friend_1_you_respond_emotions
#q_net_friend_1_you_respond_makes_jokes
#q_net_friend_1_you_respond_changes_subject
#q_net_friend_1_you_respond_mood_text_1
#q_net_friend_1_you_respond_mood_1
#q_net_friend_1_you_respond_mood_text_2
#q_net_friend_1_you_respond_mood_2
#q_net_friend_1_you_respond_mood_text_3
#q_net_friend_1_you_respond_mood_3

#How often do they talk to you about how they're feeling?		
# q_net_friend_1_you_respond_talk


#q_agree_with_the_most

#"q_agree_with_the_most_1= All of my closest friends are in my school year
#q_agree_with_the_most_2 = Most of my closest friends are in my school year
#q_agree_with_the_most_3 = About half of closest friends are in my school year
#q_agree_with_the_most_4 = Very few of my closest friends are in my school year
#q_agree_with_the_most_5= None of my closest friends are in my school year"




########################################
#                                      #
#                                      #
#            Other layers              #
#                                      #
#                                      #
########################################


#################################################
#                                               #
#            Trust                              #
#                                               #
#  https://www.youtube.com/watch?v=VGnIZx00_ho  #
#                                               #
#    Been listening to the band  TR/ST a lot    #      
#      Link to the song 'Gone' above            #
#                                               #
#################################################

#Whose opinion do you trust / value most at your school?


########################################
#                                      #
#            Best grades               #
#                                      #
########################################

########################################
#                                      #
#         Most respected               #
#                                      #
########################################


########################################
#                                      #
#         Don't get along with         #
#                                      #
########################################

# Which people in your year are the most respected by others?

# Which people in your year do you not get along with?

# q_net_friend_people_you_dont_like_3_reason


########################################
#                                      #
#           Look up to                 #

#Not inlcuded in the dataset - double check


#                                      #
########################################

#df$q_net_friend_assist_peer_leader_question_1_hidden_id#

#df$q_net_friend_assist_peer_leader_question_1_hidden_id

#df$q_net_friend_trouble_maker_pals_ladder_3_hidden_id

# Which people in your year are the most respected by others?

###Melt data into  edgelist based on edges only
#edge.df <- df[,c("id",
#                 "q_net_friend_assist_peer_leader_question_1_hidden_id",
#                 "q_net_friend_assist_peer_leader_question_2_hidden_id",
#                 "q_net_friend_assist_peer_leader_question_3_hidden_id")]#

# make network
#edge <- data.frame()
#temp <- data.frame()
#for (i in 2:ncol(edge.df)) {
#  temp <- cbind(edge.df[, 1], edge.df[, i])
#  edge <- rbind(edge, temp)
#}
#colnames(edge) <- c("respondent_id", "alter")
#edge$alter <- as.character(edge$alter)
#edge$alter <- ifelse(edge$alter == "" | edge$alter == "0", NA, edge$alter)

#edgeclean <- edge[which(!is.na(edge$alter)),] 
#edgeclean$respondent_id <- as.numeric(edgeclean$respondent_id)
#edgeclean$alter <- as.numeric(edgeclean$alter)

#look.up.to.net <-network(edgeclean,matrix.type='edgelist',ignore.eval=FALSE)


########################################
#                                      #
#                Popular               #
#                                      #
########################################

# Who do you look up to at your school?

##################################
##################################
#       Dropped questions       ##
##################################
##################################

##Trouble maker dropped
#df$q_net_friend_trouble_maker_pals_ladder_1_hidden_id

# Which people in your year are the trouble makers (who make the most trouble)?
#q_net_friend_trouble_maker_pals_ladder_3_first_name
# q_net_friend_pals_ladder_3_first_name
  
########################################
#                                      #
#                                      #
#               Gangs                  #
#                                      #
#                                      #
########################################

#First, think of yourself. Do you have a group or groups that you hang out with at school?

#"q_gangs_1= Yes
##q_gangs_2 = No"

#q_gangs_text	

#What sort of group is this, what are you called, or what do you have in common?
  
#q_gangs_sporty
#q_gangs_popular
#q_gangs_powerfull
#q_gangs_doing_well
#q_gangs_stylish
#q_gangs_laugh
#q_gangs_trouble
#q_gangs_ordinary
#q_gangs_respected
#q_gangs_person_1_first_name
#q_gangs_person_1_second_name
#q_gangs_person_2_first_name
#q_gangs_person_2_second_name


#q_other_gangs_text
#q_other_gangs_sporty
#q_other_gangs_popular
#q_other_gangs_powerfull
#q_other_gangs_doing_well
#q_other_gangs_stylish
#q_other_gangs_laugh
#q_other_gangs_trouble
#q_other_gangs_ordinary
#q_other_gangs_respected
#q_other_gangs_like
#q_other_gangs_person_1_first_name
#q_other_gangs_person_1_second_name
#q_other_gangs_person_2_first_name
#q_other_gangs_person_2_second_name