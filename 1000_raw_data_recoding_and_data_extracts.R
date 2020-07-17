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

require(dplyr)

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

library(ggplot2)

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
