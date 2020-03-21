rm(list = ls())

#################
#               #
#      Name     #
#               #
#################


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
install.packages("mi")
library(mi)
# library(dplyr)
# library(reshape2)

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

# setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")
setwd("/home/claudia/Desktop/Net4healthTaken")
test.df <- read.csv("N4H extract 12-02-2020 3 - Anonymised.csv", stringsAsFactors = FALSE)

###################################################################################
# check missing
###################################################################################

test.dfCheck <- apply(test.df, 2, function(x) ifelse(x == " " | x == "" | x == 0, NA, x))

summary(test.dfCheck)

mdf <- missing_data.frame(test.dfCheck)
windows()
image(mdf)

missing <- list()

for (i in 1: ncol(test.dfCheck)) {
  temp <- list(table(is.na(test.dfCheck[,i])))
  missing <- append(missing, temp)
  }

names(missing) <- colnames(test.dfCheck)
# if TRUE is missing, if FALSE it's there


# how to check missingness 

missing[1516] # by col number

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

length(missing)

# dataframe with open questions
opentextcsv <- NULL
opentextcsv <- as.data.frame(cbind(test.df$respondent_id,
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
                                   test.df$q_net_friend_1_same_clubs_other_text,
                                   test.df$q_net_friend_2_same_clubs_other_text,
                                   test.df$q_net_friend_3_same_clubs_other_text,
                                   test.df$q_net_friend_4_same_clubs_other_text,
                                   test.df$q_net_friend_5_same_clubs_other_text,
                                   test.df$q_net_friend_6_same_clubs_other_text,
                                   test.df$q_net_friend_7_same_clubs_other_text,
                                   test.df$q_net_friend_8_same_clubs_other_text,
                                   test.df$q_net_friend_9_same_clubs_other_text,
                                   test.df$q_net_friend_10_same_clubs_other_text))

class(opentextcsv)
opentextcsv[1,1]
namescol[1]
colnames(opentextcsv) 
opentextcsv <- subset(test.df, select = c(respondent_id,
                      q_6_other,
                      q_8_c_other,
                      q_8_d_other,
                      q_8_e_other,
                      q_8_f_other,
                      q_8_g_other,
                      q_8_h_other,
                      q_8_i_other,
                      q_10_disability,
                      q_11_religion,
                      q_12_other,
                      q_12b_j_other,
                      q_12b_k_other,
                      q_15_other,
                      q_26_g_text,
                      q_27_l_text,
                      q_29_i_text,
                      q_33_i_text,
                      q_38_g_text,
                      q_39_o_text,
                      q_47_ac_text,
                      q_48_v_text,
                      q_54_smoke,
                      q_61b_g_who,
                      q_73_text,
                      q_77_comment,
                      q_78_comment,
                      q_80_other,
                      q_net_friend_people_you_dont_like_1_reason,
                      q_net_friend_people_you_dont_like_2_reason,
                      q_net_friend_people_you_dont_like_3_reason,
                      q_net_friend_1_other_text_1,
                      q_net_friend_1_other_text_2,
                      q_net_friend_1_other_text_3,
                      q_net_friend_2_other_text_1,
                      q_net_friend_2_other_text_2,
                      q_net_friend_2_other_text_3,
                      q_net_friend_3_other_text_1,
                      q_net_friend_3_other_text_2,
                      q_net_friend_3_other_text_3,
                      q_net_friend_4_other_text_1,
                      q_net_friend_4_other_text_2,
                      q_net_friend_4_other_text_3,
                      q_net_friend_5_other_text_1,
                      q_net_friend_5_other_text_2,
                      q_net_friend_5_other_text_3,
                      q_net_friend_6_other_text_1,
                      q_net_friend_6_other_text_2,
                      q_net_friend_6_other_text_3,
                      q_net_friend_7_other_text_1,
                      q_net_friend_7_other_text_2,
                      q_net_friend_7_other_text_3,
                      q_net_friend_8_other_text_1,
                      q_net_friend_8_other_text_2,
                      q_net_friend_8_other_text_3,
                      q_net_friend_9_other_text_1,
                      q_net_friend_9_other_text_2,
                      q_net_friend_9_other_text_3,
                      q_net_friend_10_other_text_1,
                      q_net_friend_10_other_text_2,
                      q_net_friend_10_other_text_3,
                      q_net_friend_1_together_text_1,
                      q_net_friend_1_together_text_2,
                      q_net_friend_1_together_text_3,
                      q_net_friend_2_together_text_1,
                      q_net_friend_2_together_text_2,
                      q_net_friend_2_together_text_3,
                      q_net_friend_3_together_text_1,
                      q_net_friend_3_together_text_2,
                      q_net_friend_3_together_text_3,
                      q_net_friend_4_together_text_1,
                      q_net_friend_4_together_text_2,
                      q_net_friend_4_together_text_3,
                      q_net_friend_5_together_text_1,
                      q_net_friend_5_together_text_2,
                      q_net_friend_5_together_text_3,
                      q_net_friend_6_together_text_1,
                      q_net_friend_6_together_text_2,
                      q_net_friend_6_together_text_3,
                      q_net_friend_7_together_text_1,
                      q_net_friend_7_together_text_2,
                      q_net_friend_7_together_text_3,
                      q_net_friend_8_together_text_1,
                      q_net_friend_8_together_text_2,
                      q_net_friend_8_together_text_3,
                      q_net_friend_9_together_text_1,
                      q_net_friend_9_together_text_2,
                      q_net_friend_9_together_text_3,
                      q_net_friend_10_together_text_1,
                      q_net_friend_10_together_text_2,
                      q_net_friend_10_together_text_3,
                      q_net_friend_1_mood_text_1,
                      q_net_friend_1_mood_text_2,
                      q_net_friend_1_mood_text_3,
                      q_net_friend_2_mood_text_1,
                      q_net_friend_2_mood_text_2,
                      q_net_friend_2_mood_text_3,
                      q_net_friend_3_mood_text_1,
                      q_net_friend_3_mood_text_2,
                      q_net_friend_3_mood_text_3,
                      q_net_friend_1_you_respond_mood_text_1,
                      q_net_friend_1_you_respond_mood_text_2,
                      q_net_friend_1_you_respond_mood_text_3,
                      q_net_friend_2_you_respond_mood_text_1,
                      q_net_friend_2_you_respond_mood_text_2,
                      q_net_friend_2_you_respond_mood_text_3,
                      q_net_friend_3_you_respond_mood_text_1,
                      q_net_friend_3_you_respond_mood_text_2,
                      q_net_friend_3_you_respond_mood_text_3,
                      q_gangs_text,
                      q_net_friend_1_same_clubs_other_text,
                      q_net_friend_2_same_clubs_other_text,
                      q_net_friend_3_same_clubs_other_text,
                      q_net_friend_4_same_clubs_other_text,
                      q_net_friend_5_same_clubs_other_text,
                      q_net_friend_6_same_clubs_other_text,
                      q_net_friend_7_same_clubs_other_text,
                      q_net_friend_8_same_clubs_other_text,
                      q_net_friend_9_same_clubs_other_text,
                      q_net_friend_10_same_clubs_other_text))


write.csv(opentextcsv, "opentextDF.csv")

length(unique(namescol))
class(opentextcsv)
#####################################################################################
# recode missing to NA

test.df <- as.data.frame(apply(test.df, 2, function(x) ifelse(x == " " | x == "" | x == 0, NA, x)))
class(test.df)
# Separate networks using var respondent_school

# year two 

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



############################################
#               SCALES                     #
############################################

##### !!!!!!!!!!!!! BEFORE RUNNING THE SCALES REMOVE NA ACCORDING TO A CRITERIA !!!!!!!!!!!!!!


# Year Two

# GHQ items 42a-42k

GHQY2 <- subset(YearTwo, select = c(q_42_a, q_42_b, q_42_c, q_42_d, q_42_e, 
                                    q_42_f, q_42_g, q_42_h, q_42_i, q_42_j, q_42_k))

table(GHQY2Lik$q_42_k, useNA = "ifany")

# Likert summative scale ranging 0-33

GHQY2Lik <- as.data.frame(apply(GHQY2, 2, function(x) ifelse(x == 1, 0, 
                                                             ifelse(x == 2, 1,
                                                                    ifelse(x == 3, 2,
                                                                           ifelse(x == 4,3, x))))))
GHQY2Lik <- as.data.frame(apply(GHQY2Lik, 2, function(x) as.numeric(x)))

  
GHQY2LikScale <- rowSums(GHQY2Lik)


YearTwo<- cbind(YearTwo, GHQY2LikScale)



# GHQ Binary ranging 0-11
GHQY2Bin <- as.data.frame(apply(GHQY2, 2, function(x) ifelse(x == 1, 0, 
                                                             ifelse(x == 2, 0,
                                                                    ifelse(x == 3, 1,
                                                                           ifelse(x == 4,1, x))))))
GHQY2Bin <- as.data.frame(apply(GHQY2Bin, 2, function(x) as.numeric(x)))


GHQY2BinScale <- rowSums(GHQY2Bin)


YearTwo<- cbind(YearTwo, GHQY2BinScale)

# caseness
GHQY2CaseScale <- ifelse(GHQY2BinScale >= 3, 1, 0)

YearTwo<- cbind(YearTwo, GHQY2CaseScale)


# Self Esteem 43a-43i

# ranging 0-36
selfEstY2dir <- subset(YearTwo, select = c(q_43_a, q_43_b, q_43_d, q_43_f, q_43_g))
                          

selfEstY2rev <- subset(YearTwo, select = c(q_43_c, q_43_e, q_43_h, q_43_i, q_43_j))
                                           


selfEstY2dirRec <- as.data.frame(apply(selfEstY2dir, 2, function(x) ifelse(x == 1, 0, 
                                                                           ifelse(x == 2, 1,
                                                                                  ifelse(x == 3, 2,
                                                                                         ifelse(x == 4, 3, 
                                                                                                ifelse(x == 5, 4, x)))))))

selfEstY2revRec <- as.data.frame(apply(selfEstY2rev, 2, function(x) ifelse(x == 5, 0, 
                                                                           ifelse(x == 4, 1,
                                                                                  ifelse(x == 3, 2,
                                                                                         ifelse(x == 2, 3, 
                                                                                                ifelse(x == 1, 4, x)))))))
selfEstY2rec <- cbind(selfEstY2dirRec, selfEstY2revRec)

selfEstY2rec <- as.data.frame(apply(selfEstY2rec, 2, function(x) as.numeric(x)))


selfEstY2Scale <- rowSums(selfEstY2rec)


YearTwo<- cbind(YearTwo, selfEstY2Scale)


# Loneliness UCLA 45a-45c 

# range 0-6

LonUCLAY2 <- subset(YearTwo, select = c(q_45_a, q_45_b, q_45_c))




LonUCLAY2rec <- as.data.frame(apply(LonUCLAY2, 2, function(x) ifelse(x == 1, 0, 
                                                                     ifelse(x == 2, 1,
                                                                            ifelse(x == 3, 2, x)))))

LonUCLAY2rec <- as.data.frame(apply(LonUCLAY2rec, 2, function(x) as.numeric(x)))


LonUCLAY2Scale <- rowSums(LonUCLAY2rec)


YearTwo<- cbind(YearTwo, LonUCLAY2Scale)


# Loneliness Direct 45d


LonDIRY2Scale <- ifelse(YearTwo$q_45_d == 1, 0,
                        ifelse(YearTwo$q_45_d == 2, 1,
                               ifelse(YearTwo$q_45_d == 3, 2, YearTwo$q_45_d)))

YearTwo<- cbind(YearTwo, LonDIRY2Scale)


# Stigma Awareness

StiAwY2 <- subset(YearTwo, select = c(q_51b_b, q_51b_d, q_51b_e, q_51b_h ))

StiAwY2 <- as.data.frame(apply(StiAwY2, 2, function(x) ifelse(x == 1, 0, 
                                                              ifelse(x == 2, 1,
                                                                     ifelse(x == 3, 2,
                                                                            ifelse(x == 4, 3, 
                                                                                   ifelse(x == 5, 4, x)))))))

StiAwY2 <- as.data.frame(apply(StiAwY2, 2, function(x) as.numeric(x)))

StiAwY2Scale <- rowSums(StiAwY2)

# Stigma Agreement

StiAgrY2 <- subset(YearTwo, select = c(q_51b_m, q_51b_o, q_51b_p))


StiAgrY2 <- as.data.frame(apply(StiAgrY2, 2, function(x) ifelse(x == 1, 0, 
                                                              ifelse(x == 2, 1,
                                                                     ifelse(x == 3, 2,
                                                                            ifelse(x == 4, 3, 
                                                                                   ifelse(x == 5, 4, x)))))))

StiAgr2 <- as.data.frame(apply(StiAgrY2, 2, function(x) as.numeric(x)))

StiAgrY2Scale <- rowSums(StiAgr2)


# intelligence

IntelY2 <- subset(YearTwo, select = c(q_51b_a, q_51b_g, q_51b_i, q_51b_m))


IntelY2 <- as.data.frame(apply(IntelY2, 2, function(x) ifelse(x == 1, 0, 
                                                                ifelse(x == 2, 1,
                                                                       ifelse(x == 3, 2,
                                                                              ifelse(x == 4, 3, 
                                                                                     ifelse(x == 5, 4, x)))))))

IntelY2 <- as.data.frame(apply(IntelY2, 2, function(x) as.numeric(x)))

IntelY2Scale <- rowSums(IntelY2)



# Recovery

RecY2 <- subset(YearTwo, select = c(q_51b_f, q_51b_l))


RecY2 <- as.data.frame(apply(RecY2, 2, function(x) ifelse(x == 1, 0, 
                                                              ifelse(x == 2, 1,
                                                                     ifelse(x == 3, 2,
                                                                            ifelse(x == 4, 3, 
                                                                                   ifelse(x == 5, 4, x)))))))

RecY2 <- as.data.frame(apply(RecY2, 2, function(x) as.numeric(x)))

RecY2Scale <- rowSums(RecY2)


# Friendship

friY2 <- subset(YearTwo, select = c(q_51b_c, q_51b_j))


friY2 <- as.data.frame(apply(friY2, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

friY2 <- as.data.frame(apply(friY2, 2, function(x) as.numeric(x)))

friY2Scale <- rowSums(friY2)

# Drinking motivation social subscale
DriY2 <- subset(YearTwo, select = c(q_68_c, q_68_f, q_68_l, q_68_o, q_68_q))
table(DriY2$q_68_c)

DriY2 <- as.data.frame(apply(DriY2, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

DriY2 <- as.data.frame(apply(DriY2, 2, function(x) as.numeric(x)))

DriY2Scale <- rowSums(DriY2)


# Drinking Coping
CopY2 <- subset(YearTwo, select = c(q_68_a, q_68_d, q_68_e, q_68_g, q_68_p, q_68_r))


CopY2 <- as.data.frame(apply(CopY2, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

CopY2 <- as.data.frame(apply(CopY2, 2, function(x) as.numeric(x)))

CopY2Scale <- rowSums(CopY2)

# Drinking -Enhancement

EnhY2 <- subset(YearTwo, select = c(q_68_h, q_68_j, q_68_k, q_68_n, q_68_s))


EnhY2 <- as.data.frame(apply(EnhY2, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

EnhY2 <- as.data.frame(apply(EnhY2, 2, function(x) as.numeric(x)))

EnhY2Scale <- rowSums(EnhY2)


# Drinking -Conformity

ConfY2 <- subset(YearTwo, select = c(q_68_b, q_68_i, q_68_m, q_68_t, q_68_u))


ConfY2 <- as.data.frame(apply(ConfY2, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

ConfY2 <- as.data.frame(apply(ConfY2, 2, function(x) as.numeric(x)))

ConfY2Scale <- rowSums(ConfY2)

# Offered Drugs

table(YearTwo$q_69)
YearTwo$q_69 <-  ifelse(YearTwo$q_69 == 1, "YES", 
                        ifelse(YearTwo$q_69 == 2, "YES",
                               ifelse(YearTwo$q_69 == 3, "NO", YearTwo$q_69)))

# Drug abuse Screaning test
# NOT SURE ABOUT HOW TO RECODE IT



# Year Four

# GHQ items 42a-42k

GHQY4 <- subset(YearFour, select = c(q_42_a, q_42_b, q_42_c, q_42_d, q_42_e, 
                                    q_42_f, q_42_g, q_42_h, q_42_i, q_42_j, q_42_k))



# Likert summative scale ranging 0-33

GHQY4Lik <- as.data.frame(apply(GHQY4, 2, function(x) ifelse(x == 1, 0, 
                                                             ifelse(x == 2, 1,
                                                                    ifelse(x == 3, 2,
                                                                           ifelse(x == 4,3, x))))))
GHQY4Lik <- as.data.frame(apply(GHQY4Lik, 2, function(x) as.numeric(x)))


GHQY4LikScale <- rowSums(GHQY4Lik)


YearFour<- cbind(YearFour, GHQY4LikScale)

# GHQ Binary ranging 0-11

GHQY4Bin <- as.data.frame(apply(GHQY4, 2, function(x) ifelse(x == 1, 0, 
                                                             ifelse(x == 2, 0,
                                                                    ifelse(x == 3, 1,
                                                                           ifelse(x == 4,1, x))))))
GHQY4Bin <- as.data.frame(apply(GHQY4Bin, 2, function(x) as.numeric(x)))


GHQY4BinScale <- rowSums(GHQY4Bin)


YearFour<- cbind(YearFour, GHQY4BinScale)


# caseness
GHQY4CaseScale <- ifelse(GHQY4BinScale >= 3, 1, 0)

YearFour <- cbind(YearFour, GHQY4CaseScale)

# Self Esteem 43a-43i

# ranging 0-36
selfEstY4dir <- subset(YearFour, select = c(q_43_a, q_43_b, q_43_d, q_43_f, q_43_g))


selfEstY4rev <- subset(YearFour, select = c(q_43_c, q_43_e, q_43_h, q_43_i, q_43_j))



selfEstY4dirRec <- as.data.frame(apply(selfEstY4dir, 2, function(x) ifelse(x == 1, 0, 
                                                                           ifelse(x == 2, 1,
                                                                                  ifelse(x == 3, 2,
                                                                                         ifelse(x == 4, 3, 
                                                                                                ifelse(x == 5, 4, x)))))))

selfEstY4revRec <- as.data.frame(apply(selfEstY4rev, 2, function(x) ifelse(x == 5, 0, 
                                                                           ifelse(x == 4, 1,
                                                                                  ifelse(x == 3, 2,
                                                                                         ifelse(x == 2, 3, 
                                                                                                ifelse(x == 1, 4, x)))))))
selfEstY4rec <- cbind(selfEstY4dirRec, selfEstY4revRec)

selfEstY4rec <- as.data.frame(apply(selfEstY4rec, 2, function(x) as.numeric(x)))


selfEstY4Scale <- rowSums(selfEstY4rec)


YearFour<- cbind(YearFour, selfEstY4Scale)

# Loneliness UCLA 45a-45c 

# range 0-6

LonUCLAY4 <- subset(YearFour, select = c(q_45_a, q_45_b, q_45_c))


LonUCLAY4rec <- as.data.frame(apply(LonUCLAY4, 2, function(x) ifelse(x == 1, 0, 
                                                                     ifelse(x == 2, 1,
                                                                            ifelse(x == 3, 2, x)))))

LonUCLAY4rec <- as.data.frame(apply(LonUCLAY4rec, 2, function(x) as.numeric(x)))


LonUCLAY4Scale <- rowSums(LonUCLAY4rec)


YearFour<- cbind(YearFour, LonUCLAY4Scale)


# Loneliness Direct 45d


LonDIRY4Scale <- ifelse(YearFour$q_45_d == 1, 0,
                        ifelse(YearFour$q_45_d == 2, 1,
                               ifelse(YearFour$q_45_d == 3, 2, YearFour$q_45_d)))

YearFour<- cbind(YearFour, LonDIRY4Scale)

# Stigma Awareness

StiAwY4 <- subset(YearFour, select = c(q_51b_b, q_51b_d, q_51b_e, q_51b_l ))

StiAwY4 <- as.data.frame(apply(StiAwY4, 2, function(x) ifelse(x == 1, 0, 
                                                              ifelse(x == 2, 1,
                                                                     ifelse(x == 3, 2,
                                                                            ifelse(x == 4, 3, 
                                                                                   ifelse(x == 5, 4, x)))))))

StiAwY4 <- as.data.frame(apply(StiAwY4, 2, function(x) as.numeric(x)))

StiAwY4Scale <- rowSums(StiAwY4)

# Stigma Agreement

StiAgrY4 <- subset(YearFour, select = c(q_51b_m, q_51b_o, q_51b_p))


StiAgrY4 <- as.data.frame(apply(StiAgrY4, 2, function(x) ifelse(x == 1, 0, 
                                                                ifelse(x == 2, 1,
                                                                       ifelse(x == 3, 2,
                                                                              ifelse(x == 4, 3, 
                                                                                     ifelse(x == 5, 4, x)))))))

StiAgr4 <- as.data.frame(apply(StiAgrY4, 2, function(x) as.numeric(x)))

StiAgrY4Scale <- rowSums(StiAgr4)

# intelligence

IntelY4 <- subset(YearFour, select = c(q_51b_a, q_51b_g, q_51b_i, q_51b_m))


IntelY4 <- as.data.frame(apply(IntelY4, 2, function(x) ifelse(x == 1, 0, 
                                                              ifelse(x == 2, 1,
                                                                     ifelse(x == 3, 2,
                                                                            ifelse(x == 4, 3, 
                                                                                   ifelse(x == 5, 4, x)))))))

IntelY4 <- as.data.frame(apply(IntelY4, 2, function(x) as.numeric(x)))

IntelY4Scale <- rowSums(IntelY4)

# Recovery

RecY4 <- subset(YearFour, select = c(q_51b_f, q_51b_l))


RecY4 <- as.data.frame(apply(RecY4, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

RecY4 <- as.data.frame(apply(RecY4, 2, function(x) as.numeric(x)))

RecY4Scale <- rowSums(RecY4)


# Friendship

friY4 <- subset(YearFour, select = c(q_51b_c, q_51b_j))


friY4 <- as.data.frame(apply(friY4, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

friY4 <- as.data.frame(apply(friY4, 2, function(x) as.numeric(x)))

friY4Scale <- rowSums(friY4)

# Drinking motivation social subscale

DriY4 <- subset(YearFour, select = c(q_68_c, q_68_f, q_68_l, q_68_o, q_68_q))


DriY4 <- as.data.frame(apply(DriY4, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

DriY4 <- as.data.frame(apply(DriY4, 2, function(x) as.numeric(x)))

DriY4Scale <- rowSums(DriY4)


# Drinking Coping
CopY4 <- subset(YearFour, select = c(q_68_a, q_68_d, q_68_e, q_68_g, q_68_p, q_68_r))


CopY4 <- as.data.frame(apply(CopY4, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

CopY4 <- as.data.frame(apply(CopY4, 2, function(x) as.numeric(x)))

CopY4Scale <- rowSums(CopY4)

# Drinking -Enhancement

EnhY4 <- subset(YearFour, select = c(q_68_h, q_68_j, q_68_k, q_68_n, q_68_s))


EnhY4 <- as.data.frame(apply(EnhY4, 2, function(x) ifelse(x == 1, 0, 
                                                          ifelse(x == 2, 1,
                                                                 ifelse(x == 3, 2,
                                                                        ifelse(x == 4, 3, 
                                                                               ifelse(x == 5, 4, x)))))))

EnhY4 <- as.data.frame(apply(EnhY4, 2, function(x) as.numeric(x)))

EnhY4Scale <- rowSums(EnhY4)


# Drinking -Conformity

ConfY4 <- subset(YearFour, select = c(q_68_b, q_68_i, q_68_m, q_68_t, q_68_u))


ConfY4 <- as.data.frame(apply(ConfY4, 2, function(x) ifelse(x == 1, 0, 
                                                            ifelse(x == 2, 1,
                                                                   ifelse(x == 3, 2,
                                                                          ifelse(x == 4, 3, 
                                                                                 ifelse(x == 5, 4, x)))))))

ConfY4 <- as.data.frame(apply(ConfY4, 2, function(x) as.numeric(x)))

ConfY4Scale <- rowSums(ConfY4)

# Offered Drugs


YearFour$q_69 <-  ifelse(YearFour$q_69 == 1, "YES", 
                         ifelse(YearFour$q_69 == 2, "YES",
                                ifelse(YearFour$q_69 == 3, "NO", YearFour$q_69)))


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
E.attrY2 <- as.data.frame(YearTwo[, 678:807])


missing[678:807]

colnames(E.attrY2)

# recoding missing data and open text as NAs
E.attrY2 <- apply(E.attrY2, 2, function(x) ifelse(x != "0" &  x != "1", NA, x))

table(E.attrY2[,116], useNA = "ifany")
ncol(E.attrY2)

Themes.dfY2 <- NULL
temp <- NULL
n.items <- 13
n.friends <- 0:9
(progr <- n.items*n.friends)

# makes data frame with one column for each attribute
for (i in 1:13) {
    temp <- as.vector(E.attrY2[, i + progr])
    Themes.dfY2 <- data.frame(cbind(Themes.dfY2, temp))
  }
 
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

#############################################################################################################
# for several schools 

####Creating nomination question attributes 

##friend_*_sex       Are they male or female?
##friend_*_school    Are they in s4 at this school (can ignore maybe if linked via ID)?
##friend_*_FB        Are you facebook friends with them?
##friend_*_talk      Are tehy someone you would talk to about something private?
#                    Do you spend time with this person
##friend_*_timein       in school
##friend_*_timeout      outside school
##friend_*_timeonline   online

control.edge.att.edgelist <- list()
control.edge.att.network  <- list()

for (i in 1:6){
  colnames(raw.control.network.questions[[i]]) <- c("respondent_id", "respondent_sex",
                                                    "friend_1_hidden_id", "friend_2_hidden_id", "friend_3_hidden_id",
                                                    "friend_4_hidden_id", "friend_5_hidden_id", "friend_6_hidden_id", 
                                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
                                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
                                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
                                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
                                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
                                 
###add school id var
raw.control.network.questions[[i]]$respondent_school <- i
df <- raw.control.network.questions[[i]]
###Melt data into  edgelist based on edges only
edge.df <- df[,c("respondent_id","friend_1_hidden_id",  "friend_2_hidden_id", 
                 "friend_3_hidden_id","friend_4_hidden_id","friend_5_hidden_id", "friend_6_hidden_id")]

edge.el <- melt(edge.df, id.vars = "respondent_id", variable.name = "friend.order")

colnames(edge.el) <- c("respondent_id","friend.order","to.id")

###Melt data into attributes based on first attr
fsex.df <- df[,c("respondent_id","friend_1_sex",  "friend_2_sex", 
                 "friend_3_sex","friend_4_sex","friend_5_sex", "friend_6_sex")]

colnames(fsex.df)
fsex.el <- melt(fsex.df, id.vars = "respondent_id", variable.name = "friend.order")

##trim out the friend.order column
colnames(fsex.el) <- c("respondent_id","friend.order","fsex")

###Melt data into attributes based on second attr
fschool.df <- df[,c("respondent_id","friend_1_school",  "friend_2_school", 
                    "friend_3_school","friend_4_school","friend_5_school", "friend_6_school")]

colnames(fschool.df)
fschool.el <- melt(fschool.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(fschool.el) <- c("respondent_id","friend.order","fschool")

###Melt data into attributes based on third attr
fFB.df <- df[,c("respondent_id","friend_1_FB",  "friend_2_FB", 
                "friend_3_FB","friend_4_FB","friend_5_FB", "friend_6_FB")]

colnames(fFB.df)
fFB.el <- melt(fFB.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(fFB.el) <- c("respondent_id","friend.order","fFB")

###Melt data into attributes based on fourth attr
ftalk.df <- df[,c("respondent_id","friend_1_talk",  "friend_2_talk", 
                  "friend_3_talk","friend_4_talk","friend_5_talk", "friend_6_talk")]
colnames(ftalk.df)
ftalk.el <- melt(ftalk.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftalk.el) <- c("respondent_id","friend.order","ftalk")

###Melt data into attributes based on fifth attr
ftimein.df <- df[,c("respondent_id","friend_1_timein",  "friend_2_timein", 
                    "friend_3_timein","friend_4_timein","friend_5_timein", "friend_6_timein")]

colnames(ftimein.df)
ftimein.el <- melt(ftimein.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimein.el) <- c("respondent_id","friend.order","ftimein")

###Melt data into attributes based on sixth attr
ftimeout.df <- df[,c("respondent_id","friend_1_timeout",  "friend_2_timeout", 
                     "friend_3_timeout","friend_4_timeout","friend_5_timeout", "friend_6_timeout")]

colnames(ftimeout.df)
ftimeout.el <- melt(ftimeout.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimeout.el) <- c("respondent_id","friend.order","ftimeout")

###Melt data into attributes based on seventh attr
ftimeonline.df <- df[,c("respondent_id","friend_1_timeonline",  "friend_2_timeonline", 
                        "friend_3_timeonline","friend_4_timeonline","friend_5_timeonline", "friend_6_timeonline")]

colnames(ftimeonline.df)
ftimeonline.el <- melt(ftimeonline.df, id.vars = "respondent_id", variable.name = "friend.order")
colnames(ftimeonline.el) <- c("respondent_id","friend.order","ftimeonline")

##Second attr
head(edge.el)
head(edge.df)
head(fsex.df)
head(fsex.el)

###Check the lists

dim(fsex.el      )
dim(fschool.el)
dim(fFB.el)
dim(ftalk.el)
dim(ftimein.el)
dim(ftimeout.el)
dim(ftimeonline.el)

##All the same dimensions
head(edge.el      )
head(fsex.el      )
head(fschool.el)
head(fFB.el)
head(ftalk.el)
head(ftimein.el)
tail(edge.el      )
tail(ftimeout.el)
tail(ftimeonline.el)

###ID orders are consistent. Ok to merge columns on 

attributes.edgelist <- bind_cols(edge.el    ,   
                                 fsex.el    ,  
                                 fschool.el ,
                                 fFB.el     ,
                                 ftalk.el   ,  
                                 ftimein.el ,
                                 ftimeout.el,
                                 ftimeonline.el)

colnames(attributes.edgelist)
head(attributes.edgelist)
#View(attributes.edgelist)

##All looks fine, strip out the redundant variables

attributes.edgelist <- select(attributes.edgelist,
                              respondent_id,  friend.order ,  to.id,
                              fsex,fschool,fFB,ftalk,ftimein,
                              ftimeout,ftimeonline)

##Reorder columns
attributes.edgelist <- attributes.edgelist[,c("respondent_id" ,"to.id"      ,
                                              "friend.order"  ,"fsex"       ,
                                              "fschool"       ,"fFB"        ,
                                              "ftalk"         ,"ftimein"    ,
                                              "ftimeout"      ,"ftimeonline")] 

###Drop any edges to outside school friends

table(attributes.edgelist$to.id, useNA = "always")

attributes.edgelist <- attributes.edgelist[!is.na(attributes.edgelist$to.id),]

#str(attributes.edgelist)
#summary(attributes.edgelist)

###The network package will create an edge attribute from a matrix. 
att.net <-network(attributes.edgelist,matrix.type='edgelist',ignore.eval=FALSE)


control.edge.att.edgelist[[i]] <- attributes.edgelist 
control.edge.att.network[[i]]  <- att.net

}


#plot(control.edge.att.network[[1]])
#plot(control.edge.att.network[[2]])
#plot(control.edge.att.network[[3]])
#plot(control.edge.att.network[[4]])
#plot(control.edge.att.network[[5]])
#plot(control.edge.att.network[[6]])

save(control.edge.att.network, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_edge_att_networks.rdata")





##########################Baseline nets

baseline.edge.att.edgelist <- list()
baseline.edge.att.network  <- list()

for (i in 1:6){
  colnames(raw.baseline.network.questions[[i]]) <- c("respondent_id", "respondent_sex",
                                                    "friend_1_hidden_id", "friend_2_hidden_id", "friend_3_hidden_id",
                                                    "friend_4_hidden_id", "friend_5_hidden_id", "friend_6_hidden_id", 
                                                    "friend_1_sex", "friend_1_school", "friend_1_FB", 
                                                    "friend_1_talk", "friend_1_timein", "friend_1_timeout", 
                                                    "friend_1_timeonline", "friend_2_sex", "friend_2_school", "friend_2_FB",
                                                    "friend_2_talk", "friend_2_timein", "friend_2_timeout", "friend_2_timeonline", 
                                                    "friend_3_sex", "friend_3_school", "friend_3_FB",
                                                    "friend_3_talk", "friend_3_timein", "friend_3_timeout", "friend_3_timeonline",
                                                    "friend_4_sex", "friend_4_school", "friend_4_FB",
                                                    "friend_4_talk", "friend_4_timein", "friend_4_timeout", "friend_4_timeonline", 
                                                    "friend_5_sex", "friend_5_school", "friend_5_FB",
                                                    "friend_5_talk", "friend_5_timein", "friend_5_timeout", "friend_5_timeonline", 
                                                    "friend_6_sex", "friend_6_school", "friend_6_FB",
                                                    "friend_6_talk", "friend_6_timein", "friend_6_timeout", "friend_6_timeonline")
  
  ###add school id var
  raw.baseline.network.questions[[i]]$respondent_school <- i
  
  df <- raw.baseline.network.questions[[i]]
  
  ###Melt data into  edgelist based on edges only
  edge.df <- df[,c("respondent_id","friend_1_hidden_id",  "friend_2_hidden_id", 
                   "friend_3_hidden_id","friend_4_hidden_id","friend_5_hidden_id", "friend_6_hidden_id")]
  
  edge.el <- melt(edge.df, id.vars = "respondent_id", variable.name = "friend.order")
  
  colnames(edge.el) <- c("respondent_id","friend.order","to.id")
  
  ###Melt data into attributes based on first attr
  fsex.df <- df[,c("respondent_id","friend_1_sex",  "friend_2_sex", 
                   "friend_3_sex","friend_4_sex","friend_5_sex", "friend_6_sex")]
  
  colnames(fsex.df)
  fsex.el <- melt(fsex.df, id.vars = "respondent_id", variable.name = "friend.order")
  
  ##trim out the friend.order column
  colnames(fsex.el) <- c("respondent_id","friend.order","fsex")
  
  ###Melt data into attributes based on second attr
  fschool.df <- df[,c("respondent_id","friend_1_school",  "friend_2_school", 
                      "friend_3_school","friend_4_school","friend_5_school", "friend_6_school")]
  
  colnames(fschool.df)
  fschool.el <- melt(fschool.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(fschool.el) <- c("respondent_id","friend.order","fschool")
  
  ###Melt data into attributes based on third attr
  fFB.df <- df[,c("respondent_id","friend_1_FB",  "friend_2_FB", 
                  "friend_3_FB","friend_4_FB","friend_5_FB", "friend_6_FB")]
  
  colnames(fFB.df)
  fFB.el <- melt(fFB.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(fFB.el) <- c("respondent_id","friend.order","fFB")
  
  ###Melt data into attributes based on fourth attr
  ftalk.df <- df[,c("respondent_id","friend_1_talk",  "friend_2_talk", 
                    "friend_3_talk","friend_4_talk","friend_5_talk", "friend_6_talk")]
  colnames(ftalk.df)
  ftalk.el <- melt(ftalk.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftalk.el) <- c("respondent_id","friend.order","ftalk")
  
  ###Melt data into attributes based on fifth attr
  ftimein.df <- df[,c("respondent_id","friend_1_timein",  "friend_2_timein", 
                      "friend_3_timein","friend_4_timein","friend_5_timein", "friend_6_timein")]
  
  colnames(ftimein.df)
  ftimein.el <- melt(ftimein.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimein.el) <- c("respondent_id","friend.order","ftimein")
  
  ###Melt data into attributes based on sixth attr
  ftimeout.df <- df[,c("respondent_id","friend_1_timeout",  "friend_2_timeout", 
                       "friend_3_timeout","friend_4_timeout","friend_5_timeout", "friend_6_timeout")]
  
  colnames(ftimeout.df)
  ftimeout.el <- melt(ftimeout.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimeout.el) <- c("respondent_id","friend.order","ftimeout")
  
  ###Melt data into attributes based on seventh attr
  ftimeonline.df <- df[,c("respondent_id","friend_1_timeonline",  "friend_2_timeonline", 
                          "friend_3_timeonline","friend_4_timeonline","friend_5_timeonline", "friend_6_timeonline")]
  
  colnames(ftimeonline.df)
  ftimeonline.el <- melt(ftimeonline.df, id.vars = "respondent_id", variable.name = "friend.order")
  colnames(ftimeonline.el) <- c("respondent_id","friend.order","ftimeonline")
  
  ##Second attr
  head(edge.el)
  head(edge.df)
  head(fsex.df)
  head(fsex.el)
  
  ###Check the lists
  
  dim(fsex.el      )
  dim(fschool.el)
  dim(fFB.el)
  dim(ftalk.el)
  dim(ftimein.el)
  dim(ftimeout.el)
  dim(ftimeonline.el)
  
  ##All the same dimensions
  head(edge.el      )
  head(fsex.el      )
  head(fschool.el)
  head(fFB.el)
  head(ftalk.el)
  head(ftimein.el)
  tail(edge.el      )
  tail(ftimeout.el)
  tail(ftimeonline.el)
  
  ###ID orders are consistent. Ok to merge columns on 
  
  attributes.edgelist <- bind_cols(edge.el    ,   
                                   fsex.el    ,  
                                   fschool.el ,
                                   fFB.el     ,
                                   ftalk.el   ,  
                                   ftimein.el ,
                                   ftimeout.el,
                                   ftimeonline.el)
  
  colnames(attributes.edgelist)
  head(attributes.edgelist)
  #View(attributes.edgelist)
  
  ##All looks fine, strip out the redundant variables
  
  attributes.edgelist <- select(attributes.edgelist,
                                respondent_id,  friend.order ,  to.id,
                                fsex,fschool,fFB,ftalk,ftimein,
                                ftimeout,ftimeonline)
  
  ##Reorder columns
  attributes.edgelist <- attributes.edgelist[,c("respondent_id" ,"to.id"      ,
                                                "friend.order"  ,"fsex"       ,
                                                "fschool"       ,"fFB"        ,
                                                "ftalk"         ,"ftimein"    ,
                                                "ftimeout"      ,"ftimeonline")] 
  
  ###Drop any edges to outside school friends
  
  table(attributes.edgelist$to.id, useNA = "always")
  
  attributes.edgelist <- attributes.edgelist[!is.na(attributes.edgelist$to.id),]
  
  ###The network package will create an edge attribute from a matrix. 
  att.net <-network(attributes.edgelist,matrix.type='edgelist',ignore.eval=FALSE)
  
  
  baseline.edge.att.edgelist[[i]] <- attributes.edgelist 
  baseline.edge.att.network[[i]]  <- att.net
  
}

#plot(baseline.edge.att.network[[1]])
#plot(baseline.edge.att.network[[2]])
#plot(baseline.edge.att.network[[3]])
#plot(baseline.edge.att.network[[4]])
#plot(baseline.edge.att.network[[5]])
#plot(baseline.edge.att.network[[6]])


save(baseline.edge.att.network, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_edge_att_networks.rdata")


