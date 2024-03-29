rm(list = ls())

#################
#               #
#      Name     #
#               #
#################

# File not developed yet


#############
#  Purpose  #
#############

# To add labels to the raw data

# To explore the datasets 

# To prepare extracts for sending to others

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(dplyr)
library(ggplot2)


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

setwd("C:/Users/mmc78h/Documents/A Work/Net4Health/Data")

test.df <- read.csv("N4H extract 12-02-2020 3 - Anonymised.csv", stringsAsFactors = FALSE)


#Remove a test completion
test.df <- filter(test.df, respondent_id != "testid1")


###############################

##    Exploring variables

###############################

#############
hist(test.df$q_36_a)
hist(test.df$q_36_b)
hist(test.df$q_36_c)
hist(test.df$q_36_d)


###How _____ compared with the rest of your year group 
#How sporty 
ggplot(test.df, aes(x=q_36_a, color=as.factor(respondent_school))) +
  geom_density()

#Popular
ggplot(test.df, aes(x=q_36_b, color=as.factor(respondent_school))) +
  geom_density()

#Doing well at school
ggplot(test.df, aes(x=q_36_c, color=as.factor(respondent_school))) +
  geom_density()

#Troublemaker
ggplot(test.df, aes(x=q_36_d, color=as.factor(respondent_school))) +
  geom_density()


#How well off in Scottish Society
ggplot(test.df, aes(x=q_24, color=as.factor(respondent_school))) +
  geom_density()

####How well off by dishwasher question
ggplot(test.df, aes(x=q_24, color=as.factor(q_23))) +
  geom_density()
#############


##################################################
###   Create data extract with diet variables  ###
##################################################

##########

diet.df <- select(test.df,  c(respondent_id,
                              respondent_school
                              ,q_6
                              ,q_26_a
                              ,q_26_b
                              ,q_26_c
                              ,q_26_d
                              ,q_26_e
                              ,q_26_f
                              ,q_27_a
                              ,q_27_b
                              ,q_27_c
                              ,q_27_d
                              ,q_27_e
                              ,q_27_f
                              ,q_27_g
                              ,q_27_h
                              ,q_27_i
                              ,q_27_j
                              ,q_27_k
                              ,q_34_a
                              ,q_34_b
                              ,q_34_c
                              ,q_34_d
                              ,q_34_e
                              ,q_34_f
                              ,q_34_g
                              ,q_34_h
                              ,q_34_i
                              ,q_34_j
                              ,q_34_k
                              ,q_34_l
                              ,q_43_a
                              ,q_43_b
                              ,q_43_c
                              ,q_43_d
                              ,q_43_e
                              ,q_43_f
                              ,q_43_g
                              ,q_43_h
                              ,q_43_i
                              ,q_43_j
                              ,q_44_a
                              ,q_44_b
                              ,q_44_c
                              ,q_44_d
                              ,q_44_e
                              ,q_45_a
                              ,q_45_b
                              ,q_45_c
                              ,q_45_d
                              ,q_46_a
                              ,q_46_b
                              ,q_46_c
                              ,q_46_d
                              ,q_46_e
                              ,q_47_a
                              ,q_47_b
                              ,q_47_c
                              ,q_47_d
                              ,q_47_e
                              ,q_47_f
                              ,q_47_g
                              ,q_47_h
                              ,q_47_i
                              ,q_47_j
                              ,q_47_k
                              ,q_47_l
                              ,q_47_m
                              ,q_47_n
                              ,q_47_o
                              ,q_47_p
                              ,q_47_q
                              ,q_47_r
                              ,q_47_s
                              ,q_47_t
                              ,q_47_u
                              ,q_47_v
                              ,q_47_w
                              ,q_47_x
                              ,q_47_y
                              ,q_47_z
                              ,q_47_aa
                              ,q_47_ab
                              ,q_75_a
                              ,q_75_b
                              ,q_75_c
                              ,q_75_d
                              ,q_75_e
                              ,q_75_f
                              ,q_75_g
                              ,q_76
                              ,q_net_friend_1_hidden_id
                              ,q_net_friend_2_hidden_id
                              ,q_net_friend_3_hidden_id
                              ,q_net_friend_4_hidden_id
                              ,q_net_friend_5_hidden_id
                              ,q_net_friend_6_hidden_id
                              ,q_net_friend_7_hidden_id
                              ,q_net_friend_8_hidden_id
                              ,q_net_friend_9_hidden_id
                              ,q_net_friend_10_hidden_id
                              ,q_net_friend_1_how_close
                              ,q_net_friend_2_how_close                            
                              ,q_net_friend_3_how_close
                              ,q_net_friend_4_how_close
                              ,q_net_friend_5_how_close
                              ,q_net_friend_6_how_close
                              ,q_net_friend_7_how_close
                              ,q_net_friend_8_how_close
                              ,q_net_friend_8_how_close
                              ,q_net_friend_9_how_close
                              ,q_net_friend_10_how_close
                              ,q_net_friend_1_how_long
                              ,q_net_friend_2_how_long
                              ,q_net_friend_3_how_long
                              ,q_net_friend_4_how_long
                              ,q_net_friend_5_how_long
                              ,q_net_friend_6_how_long
                              ,q_net_friend_7_how_long
                              ,q_net_friend_8_how_long
                              ,q_net_friend_9_how_long
                              ,q_net_friend_10_how_long
                              )
)


write.csv(diet.df, file = "Net4Health Diet variables extract 16072020.csv")

##########



#############################################################################
###         Create data extract for BSc Psych Medicine Project            ###
#############################################################################
duplicated(test.df$id)
##########
psychmed.df <- select(test.df,c(id
                              , respondent_school
                              , q_6
                              , q_7
                              , q_8
                              , q_18
                              , q_19
                              , q_20
                              , q_21
                              , q_22
                              , q_23
                              , q_25_a, q_25_b, q_25_c, q_25_d, q_25_e, q_25_f, q_25_g, q_25_h
                              , q_26_a, q_26_b, q_26_c, q_26_d, q_26_e, q_26_f
                              , q_27_a, q_27_b, q_27_c, q_27_d, q_27_e, q_27_f, q_27_g, q_27_h
                              , q_27_i, q_27_j, q_27_k
                              , q_35_a, q_35_b, q_35_c, q_35_d, q_35_e, q_35_f, q_35_g, q_35_h
                              , q_35_i, q_35_j, q_35_k, q_35_l, q_35_m, q_35_n, q_35_o
                              , q_37_a, q_37_b, q_37_c, q_37_d, q_37_e, q_37_f, q_37_g
                              , q_net_friend_1_hidden_id, q_net_friend_2_hidden_id
                              , q_net_friend_3_hidden_id, q_net_friend_4_hidden_id
                              , q_net_friend_5_hidden_id, q_net_friend_6_hidden_id
                              , q_net_friend_7_hidden_id, q_net_friend_8_hidden_id
                              , q_net_friend_9_hidden_id, q_net_friend_10_hidden_id
                              , q_net_friend_emotional_support_outward_1_hidden_id
                              , q_net_friend_emotional_support_outward_2_hidden_id
                              , q_net_friend_emotional_support_outward_3_hidden_id
                              , q_net_friend_emotional_support_inward_1_hidden_id
                              , q_net_friend_emotional_support_inward_2_hidden_id
                              , q_net_friend_emotional_support_inward_3_hidden_id
                              , q_net_friend_emotional_support_outward_1_person
                              , q_net_friend_emotional_support_outward_2_person
                              , q_net_friend_emotional_support_outward_3_person
                              , q_net_friend_1_support
                              , q_net_friend_2_support
                              , q_net_friend_3_support
                              , q_net_friend_1_emotions
                              , q_net_friend_2_emotions
                              , q_net_friend_3_emotions
                              , q_net_friend_1_makes_jokes
                              , q_net_friend_2_makes_jokes
                              , q_net_friend_3_makes_jokes
                              , q_net_friend_1_changes_subject
                              , q_net_friend_2_changes_subject
                              , q_net_friend_3_changes_subject
                              , q_51b_a, q_51b_b, q_51b_c, q_51b_d, q_51b_e, q_51b_f
                              , q_51b_g, q_51b_h, q_51b_i, q_51b_j, q_51b_k, q_51b_l
                              , q_51b_m, q_51b_n, q_51b_o, q_51b_p 
                              , q_42_a, q_42_b, q_42_c, q_42_d, q_42_e
                              , q_42_f, q_42_g, q_42_h, q_42_i, q_42_j
                              , q_42_k, q_42_l
                              ))
write.csv(psychmed.df, file = "Net4Health Psych Med variables extract 200121.csv")

##########
