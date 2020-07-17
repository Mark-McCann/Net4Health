rm(list = ls())

#################
#               #
#      Name     #
#               #
#################


# Search for three asterisks to find things to be checked ***

## At the moment", the code assumes it's NA and deletes. 

# The edge attributes haven't been added. 

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
library(intergraph)

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

#setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")
#setwd("/home/claudia/Desktop/Net4healthTaken")

setwd("C:/Users/mmc78h/Documents/A Work/Net4Health/Data")

test.df <- read.csv("N4H extract 12-02-2020 3 - Anonymised.csv", stringsAsFactors = FALSE)

test.df <- subset(test.df, select = -c(q_1_day,	q_1_month,	q_1_year, q_3_town, q_4_postcode ,
q_8_c_other, q_8_f_other,	q_8_g_other,	q_8_h_other,	q_8_i_other, q_13_b,
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
q_other_gangs_text,
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


write.csv(test.df, file=  "N4H anonymised extract 12-02-2020.csv")
