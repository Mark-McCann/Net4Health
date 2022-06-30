rm(list = ls()) # clearing memory space


#################
#               #
#      Name     #
#               #
#################

# Srebrenka Letina developed this script


#############
#  Purpose  #
#############

# General inspection of the dataset

# Explore different sets of variables and prepare extracts

# Check missingness

#########################
#                       #
#    Load packages      #
#                       #
#########################
###### PUT PACKAGES HERE

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

setwd("T:/projects/Net4Health S00371/Data/AnonymisedData/pilot_school_data/working data")

library(readxl)
library(dplyr) 
library(car)

test.df <- read_excel("Final changes without Names N4H extract.xlsx", 1)


dim(test.df) # 302

###############################

##    Dataset properties

###############################

######### 0 are "NA"
######### NOT ALWAYS THOUGH -> Bullying (q 50 and q 51) !!!! ##########
# replacing 0s with NA
test.df_na <- na_if(test.df, 0)

# overestimates NA bc:
# some are conditionals
# q 50 and q 51

#### very raw AND WRONG analysis - whole dataset ###### ???

rawna <- read.csv("Data Structure Net4Health with NA 200121.csv", 
                  stringsAsFactors = FALSE)
colnames(rawna)
View(rawna) 

# Simple Scatterplot
# better ones: http://www.sthda.com/english/wiki/ggplot2-scatterplot-easy-scatter-plot-using-ggplot2-and-r-statistical-software

png(file = "propor missing v1.png", width = 800 , height=523)

plot(rawna$Index, rawna$Per_of_Missing, main="Percentage of missing by order", 
     xlab="Order", ylab="% missing", pch=19)
# Add fit lines
abline(lm(rawna$Per_of_Missing ~ rawna$Index), col="red") # regression line (y~x) 
lines(lowess(rawna$Index,rawna$Per_of_Missing), col="blue") # lowess line (x,y)
dev.off()

# distribution
mean(rawna$Per_of_Missing)#0.77

png(file="distribution missing v1.png", width = 800 , height=523)

ggplot(rawna, aes(x=rawna$Per_of_Missing)) +
  geom_histogram(binwidth=.1, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(rawna$Per_of_Missing, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)
dev.off()
# correlation
cor(rawna$Index, rawna$Per_of_Missing, method = "spearman")# 0.70
cor(rawna$Index, rawna$Per_of_Missing, method = "pearson") # 0.64


# Missing values per case 
mean(rowSums(is.na(test.df_na)))
max(rowSums(is.na(test.df_na)))
min(rowSums(is.na(test.df_na)))
mean(rowSums(is.na(test.df)))

# missing values per dataset unit

#dplyr approach to finding the number of NAs for each column
df %>% 
  summarise_all((funs(sum(is.na(.))))) 


library(data.table)

library(naniar)
library(tidyr)
vis_miss(part1.1)
gg_miss_var(part1.1)



###### CLEANING THE DATASET AND PREPARING IT FOR MISSING DATA ANALYSIS

# dropping id and non-existent and other variables


df = subset(test.df_na , select = -c(
  # ids - not for missing data analysis
  #id,
 id1, 
 id2,
  
  #complete,
  #last_updated,
  #id.2,
  #respondent_id.2,
  
  #complete.1,
  #last_updated.1,
  #id.3,
  #respondent_id.3,
  
  #complete.2,
  #last_updated.2,
  
  
  # missing from dataset
  #q_12b_i - just zero responses
  q_46_a,
  q_46_b,
  q_46_c,
  q_46_d,
  q_46_e,
  q_63,
  q_74_a,
  q_74_b,
  q_74_c,
  q_74_d,
  
  
  q_net_friend_assist_peer_leader_question_1_first_name,
  q_net_friend_assist_peer_leader_question_1_hidden_id,
  q_net_friend_assist_peer_leader_question_2_first_name,
  q_net_friend_assist_peer_leader_question_2_hidden_id,
  q_net_friend_assist_peer_leader_question_3_first_name,
  q_net_friend_assist_peer_leader_question_3_hidden_id,
  q_net_friend_trouble_maker_pals_ladder_1_first_name,
  q_net_friend_trouble_maker_pals_ladder_1_hidden_id,
  q_net_friend_trouble_maker_pals_ladder_2_first_name,
  q_net_friend_trouble_maker_pals_ladder_2_hidden_id,
  q_net_friend_trouble_maker_pals_ladder_3_first_name,
  q_net_friend_trouble_maker_pals_ladder_3_hidden_id,
  
  q_net_friend_1_hang_out,
  q_net_friend_2_hang_out,
  q_net_friend_3_hang_out,
  q_net_friend_4_hang_out,
  q_net_friend_5_hang_out,
  q_net_friend_6_hang_out,
  q_net_friend_7_hang_out,
  q_net_friend_8_hang_out,
  q_net_friend_9_hang_out,
  q_net_friend_10_hang_out,
  
  
  q_net_friend_1_social_media,
  q_net_friend_2_social_media,
  q_net_friend_3_social_media,
  q_net_friend_4_social_media,
  q_net_friend_5_social_media,
  q_net_friend_6_social_media,
  q_net_friend_7_social_media,
  q_net_friend_8_social_media,
  q_net_friend_9_social_media,
  q_net_friend_10_social_media,
  q_net_friend_1_current_affairs,
  q_net_friend_1_music,
  q_net_friend_1_school,
  q_net_friend_1_other_pupils,
  q_net_friend_1_other_people,
  q_net_friend_1_sports,
  q_net_friend_1_alchohol,
  q_net_friend_1_relationships,
  q_net_friend_1_keeping_fit,
  q_net_friend_1_eating,
  q_net_friend_1_other_text_1,
  q_net_friend_1_other_text_2,
  q_net_friend_1_other_text_3,
  q_net_friend_2_current_affairs,
  q_net_friend_2_music,
  q_net_friend_2_school,
  q_net_friend_2_other_pupils,
  q_net_friend_2_other_people,
  q_net_friend_2_sports,
  q_net_friend_2_alchohol,
  q_net_friend_2_relationships,
  q_net_friend_2_keeping_fit,
  q_net_friend_2_eating,
  q_net_friend_2_other_text_1,
  q_net_friend_2_other_text_2,
  q_net_friend_2_other_text_3,
  q_net_friend_3_current_affairs,
  q_net_friend_3_music,
  q_net_friend_3_school,
  q_net_friend_3_other_pupils,
  q_net_friend_3_other_people,
  q_net_friend_3_sports,
  q_net_friend_3_alchohol,
  q_net_friend_3_relationships,
  q_net_friend_3_keeping_fit,
  q_net_friend_3_eating,
  q_net_friend_3_other_text_1,
  q_net_friend_3_other_text_2,
  q_net_friend_3_other_text_3,
  q_net_friend_4_current_affairs,
  q_net_friend_4_music,
  q_net_friend_4_school,
  q_net_friend_4_other_pupils,
  q_net_friend_4_other_people,
  q_net_friend_4_sports,
  q_net_friend_4_alchohol,
  q_net_friend_4_relationships,
  q_net_friend_4_keeping_fit,
  q_net_friend_4_eating,
  q_net_friend_4_other_text_1,
  q_net_friend_4_other_text_2,
  q_net_friend_4_other_text_3,
  q_net_friend_5_current_affairs,
  q_net_friend_5_music,
  q_net_friend_5_school,
  q_net_friend_5_other_pupils,
  q_net_friend_5_other_people,
  q_net_friend_5_sports,
  q_net_friend_5_alchohol,
  q_net_friend_5_relationships,
  q_net_friend_5_keeping_fit,
  q_net_friend_5_eating,
  q_net_friend_5_other_text_1,
  q_net_friend_5_other_text_2,
  q_net_friend_5_other_text_3,
  q_net_friend_6_current_affairs,
  q_net_friend_6_music,
  q_net_friend_6_school,
  q_net_friend_6_other_pupils,
  q_net_friend_6_other_people,
  q_net_friend_6_sports,
  q_net_friend_6_alchohol,
  q_net_friend_6_relationships,
  q_net_friend_6_keeping_fit,
  q_net_friend_6_eating,
  q_net_friend_6_other_text_1,
  q_net_friend_6_other_text_2,
  q_net_friend_6_other_text_3,
  q_net_friend_7_current_affairs,
  q_net_friend_7_music,
  q_net_friend_7_school,
  q_net_friend_7_other_pupils,
  q_net_friend_7_other_people,
  q_net_friend_7_sports,
  q_net_friend_7_alchohol,
  q_net_friend_7_relationships,
  q_net_friend_7_keeping_fit,
  q_net_friend_7_eating,
  q_net_friend_7_other_text_1,
  q_net_friend_7_other_text_2,
  q_net_friend_7_other_text_3,
  q_net_friend_8_current_affairs,
  q_net_friend_8_music,
  q_net_friend_8_school,
  q_net_friend_8_other_pupils,
  q_net_friend_8_other_people,
  q_net_friend_8_sports,
  q_net_friend_8_alchohol,
  q_net_friend_8_relationships,
  q_net_friend_8_keeping_fit,
  q_net_friend_8_eating,
  q_net_friend_8_other_text_1,
  q_net_friend_8_other_text_2,
  q_net_friend_8_other_text_3,
  q_net_friend_9_current_affairs,
  q_net_friend_9_music,
  q_net_friend_9_school,
  q_net_friend_9_other_pupils,
  q_net_friend_9_other_people,
  q_net_friend_9_sports,
  q_net_friend_9_alchohol,
  q_net_friend_9_relationships,
  q_net_friend_9_keeping_fit,
  q_net_friend_9_eating,
  q_net_friend_9_other_text_1,
  q_net_friend_9_other_text_2,
  q_net_friend_9_other_text_3,
  q_net_friend_10_current_affairs,
  q_net_friend_10_music,
  q_net_friend_10_school,
  q_net_friend_10_other_pupils,
  q_net_friend_10_other_people,
  q_net_friend_10_sports,
  q_net_friend_10_alchohol,
  q_net_friend_10_relationships,
  q_net_friend_10_keeping_fit,
  q_net_friend_10_eating,
  q_net_friend_10_other_text_1,
  q_net_friend_10_other_text_2,
  q_net_friend_10_other_text_3,
  q_net_friend_1_listen_music,
  q_net_friend_1_shops,
  q_net_friend_1_clubs,
  q_net_friend_1_cinema,
  q_net_friend_1_books,
  q_net_friend_1_hobby,
  q_net_friend_1_scouts,
  q_net_friend_1_watch_sports,
  q_net_friend_1_street,
  q_net_friend_1_nowhere,
  q_net_friend_1_play_board_games,
  q_net_friend_1_play_computer_games,
  q_net_friend_1_internet,
  q_net_friend_1_use_social_media,
  q_net_friend_1_drink_alchohol,
  q_net_friend_1_smoke_cigarettes,
  q_net_friend_1_together_text_1,
  q_net_friend_1_together_text_2,
  q_net_friend_1_together_text_3,
  q_net_friend_2_listen_music,
  q_net_friend_2_shops,
  q_net_friend_2_clubs,
  q_net_friend_2_cinema,
  q_net_friend_2_books,
  q_net_friend_2_hobby,
  q_net_friend_2_scouts,
  q_net_friend_2_watch_sports,
  q_net_friend_2_street,
  q_net_friend_2_nowhere,
  q_net_friend_2_play_board_games,
  q_net_friend_2_play_computer_games,
  q_net_friend_2_internet,
  q_net_friend_2_use_social_media,
  q_net_friend_2_drink_alchohol,
  q_net_friend_2_smoke_cigarettes,
  q_net_friend_2_together_text_1,
  q_net_friend_2_together_text_2,
  q_net_friend_2_together_text_3,
  q_net_friend_3_listen_music,
  q_net_friend_3_shops,
  q_net_friend_3_clubs,
  q_net_friend_3_cinema,
  q_net_friend_3_books,
  q_net_friend_3_hobby,
  q_net_friend_3_scouts,
  q_net_friend_3_watch_sports,
  q_net_friend_3_street,
  q_net_friend_3_nowhere,
  q_net_friend_3_play_board_games,
  q_net_friend_3_play_computer_games,
  q_net_friend_3_internet,
  q_net_friend_3_use_social_media,
  q_net_friend_3_drink_alchohol,
  q_net_friend_3_smoke_cigarettes,
  q_net_friend_3_together_text_1,
  q_net_friend_3_together_text_2,
  q_net_friend_3_together_text_3,
  q_net_friend_4_listen_music,
  q_net_friend_4_shops,
  q_net_friend_4_clubs,
  q_net_friend_4_cinema,
  q_net_friend_4_books,
  q_net_friend_4_hobby,
  q_net_friend_4_scouts,
  q_net_friend_4_watch_sports,
  q_net_friend_4_street,
  q_net_friend_4_nowhere,
  q_net_friend_4_play_board_games,
  q_net_friend_4_play_computer_games,
  q_net_friend_4_internet,
  q_net_friend_4_use_social_media,
  q_net_friend_4_drink_alchohol,
  q_net_friend_4_smoke_cigarettes,
  q_net_friend_4_together_text_1,
  q_net_friend_4_together_text_2,
  q_net_friend_4_together_text_3,
  q_net_friend_5_listen_music,
  q_net_friend_5_shops,
  q_net_friend_5_clubs,
  q_net_friend_5_cinema,
  q_net_friend_5_books,
  q_net_friend_5_hobby,
  q_net_friend_5_scouts,
  q_net_friend_5_watch_sports,
  q_net_friend_5_street,
  q_net_friend_5_nowhere,
  q_net_friend_5_play_board_games,
  q_net_friend_5_play_computer_games,
  q_net_friend_5_internet,
  q_net_friend_5_use_social_media,
  q_net_friend_5_drink_alchohol,
  q_net_friend_5_smoke_cigarettes,
  q_net_friend_5_together_text_1,
  q_net_friend_5_together_text_2,
  q_net_friend_5_together_text_3,
  q_net_friend_6_listen_music,
  q_net_friend_6_shops,
  q_net_friend_6_clubs,
  q_net_friend_6_cinema,
  q_net_friend_6_books,
  q_net_friend_6_hobby,
  q_net_friend_6_scouts,
  q_net_friend_6_watch_sports,
  q_net_friend_6_street,
  q_net_friend_6_nowhere,
  q_net_friend_6_play_board_games,
  q_net_friend_6_play_computer_games,
  q_net_friend_6_internet,
  q_net_friend_6_use_social_media,
  q_net_friend_6_drink_alchohol,
  q_net_friend_6_smoke_cigarettes,
  q_net_friend_6_together_text_1,
  q_net_friend_6_together_text_2,
  q_net_friend_6_together_text_3,
  q_net_friend_7_listen_music,
  q_net_friend_7_shops,
  q_net_friend_7_clubs,
  q_net_friend_7_cinema,
  q_net_friend_7_books,
  q_net_friend_7_hobby,
  q_net_friend_7_scouts,
  q_net_friend_7_watch_sports,
  q_net_friend_7_street,
  q_net_friend_7_nowhere,
  q_net_friend_7_play_board_games,
  q_net_friend_7_play_computer_games,
  q_net_friend_7_internet,
  q_net_friend_7_use_social_media,
  q_net_friend_7_drink_alchohol,
  q_net_friend_7_smoke_cigarettes,
  q_net_friend_7_together_text_1,
  q_net_friend_7_together_text_2,
  q_net_friend_7_together_text_3,
  q_net_friend_8_listen_music,
  q_net_friend_8_shops,
  q_net_friend_8_clubs,
  q_net_friend_8_cinema,
  q_net_friend_8_books,
  q_net_friend_8_hobby,
  q_net_friend_8_scouts,
  q_net_friend_8_watch_sports,
  q_net_friend_8_street,
  q_net_friend_8_nowhere,
  q_net_friend_8_play_board_games,
  q_net_friend_8_play_computer_games,
  q_net_friend_8_internet,
  q_net_friend_8_use_social_media,
  q_net_friend_8_drink_alchohol,
  q_net_friend_8_smoke_cigarettes,
  q_net_friend_8_together_text_1,
  q_net_friend_8_together_text_2,
  q_net_friend_8_together_text_3,
  q_net_friend_9_listen_music,
  q_net_friend_9_shops,
  q_net_friend_9_clubs,
  q_net_friend_9_cinema,
  q_net_friend_9_books,
  q_net_friend_9_hobby,
  q_net_friend_9_scouts,
  q_net_friend_9_watch_sports,
  q_net_friend_9_street,
  q_net_friend_9_nowhere,
  q_net_friend_9_play_board_games,
  q_net_friend_9_play_computer_games,
  q_net_friend_9_internet,
  q_net_friend_9_use_social_media,
  q_net_friend_9_drink_alchohol,
  q_net_friend_9_smoke_cigarettes,
  q_net_friend_9_together_text_1,
  q_net_friend_9_together_text_2,
  q_net_friend_9_together_text_3,
  q_net_friend_10_listen_music,
  q_net_friend_10_shops,
  q_net_friend_10_clubs,
  q_net_friend_10_cinema,
  q_net_friend_10_books,
  q_net_friend_10_hobby,
  q_net_friend_10_scouts,
  q_net_friend_10_watch_sports,
  q_net_friend_10_street,
  q_net_friend_10_nowhere,
  q_net_friend_10_play_board_games,
  q_net_friend_10_play_computer_games,
  q_net_friend_10_internet,
  q_net_friend_10_use_social_media,
  q_net_friend_10_drink_alchohol,
  q_net_friend_10_smoke_cigarettes,
  q_net_friend_10_together_text_1,
  q_net_friend_10_together_text_2,
  q_net_friend_10_together_text_3,
  
  q_gangs_like,
  
  # all na
  q_15_other,
  #q_51_e, - but it gets excluded later anyway
  q_activities_28,  #####???????
  q_activities_29,
  q_activities_30,
  q_activities_31,
  q_activities_37,
  
  # same club activity - later
  
  #text and comments
  q_10_disability,
  q_11_religion,
  q_26_g_text,
  q_27_l_text,
  q_29_i_text,
  q_33_i_text,
  q_38_g_text,
  q_39_o_text,
  q_47_ac_text,
  q_48_v_text,
  q_61b_g_who ,##### ????
  q_73_text,
  q_77_comment,
  q_78_comment,
  q_80_other,
  
  #q_net_friend_people_you_dont_like_1_reason, ###### ????
  #q_net_friend_people_you_dont_like_2_reason,
  #q_net_friend_people_you_dont_like_3_reason,
  
  # problematic
  q_8_c_other,
  q_8_d_other,
  q_8_e_other,
  q_8_f_other,
  q_8_g_other,
  q_8_h_other,
  q_8_i_other,
  
  q_12_other) )

dim(df) #  302 1126/1133

# the problematic variable q_13_b --> NEEDS TO BE DONE FOR ALL ANALYSIS


# recoding unique values
uniquevalues_q13 <- c(unique(df$q_13_b))
# length(uniquevalues_q13) # 31
df$q_13_b = car::recode(df$q_13_b, "uniquevalues_q13[1] = 2; 
uniquevalues_q13[2] = 4;
uniquevalues_q13[3] = 4; 
uniquevalues_q13[4] = 3;
uniquevalues_q13[5] = NA;
uniquevalues_q13[6] = 7;
uniquevalues_q13[7] = 1;
uniquevalues_q13[8] = 6;
uniquevalues_q13[9] = NA;
uniquevalues_q13[10] = 2;
uniquevalues_q13[11] = 2;
uniquevalues_q13[12] = NA;
uniquevalues_q13[13] = 1;
uniquevalues_q13[14] = NA;
uniquevalues_q13[15] = 1;
uniquevalues_q13[16] = NA;
uniquevalues_q13[17] = 5;
uniquevalues_q13[18] = 2;
uniquevalues_q13[19] = 1; 
uniquevalues_q13[20] = 1;
uniquevalues_q13[21] = 14;
uniquevalues_q13[22] = 2;
uniquevalues_q13[23] = NA;
uniquevalues_q13[24] = 2;
uniquevalues_q13[25] = NA;
uniquevalues_q13[26] = 1;
uniquevalues_q13[27] = 1")

# JUST FOR MISSING DATA ANALYSIS -  "tick" variables
# first compose new variable from them

# Which people live in your home most of the time (tick ALL that apply)
# at least one needed to be ticked unless a pupil is living alone 
nv1 <- subset(df, select = c(q_12b_a,
                             q_12b_b,
                             q_12b_c,
                             q_12b_d,
                             q_12b_e,
                             q_12b_f,
                             q_12b_g,
                             q_12b_h,
                             q_12b_i,
                             q_12b_j,
                             q_12b_j_other,
                             q_12b_k,
                             q_12b_k_other))
#as.numeric.result=TRUE))
nv1$q_12b_j_other = car::recode(nv1$q_12b_j_other, "unique(nv1$q_12b_j_other)[1]=NA")
nv1$q_12b_k_other = car::recode(nv1$q_12b_k_other, "unique(nv1$q_12b_k_other)[1]=NA")

nv1$q_12b_j_other[!is.na(nv1$q_12b_j_other)] <- 1
nv1$q_12b_k_other[!is.na(nv1$q_12b_k_other)] <- 1

# since they often don't click on other but just fill it in
nv1$q_12b_j[nv1$q_12b_j_other == 1] <- 1
nv1$q_12b_k[nv1$q_12b_k_other == 1] <- 1

# now I can exclude "other" - they are redundant
nv1$q_12b_j_other <- NULL
nv1$q_12b_k_other <- NULL

# creating new variable for missing analysis, but first to numeric:
nv1 <- as.data.frame(apply(nv1, 2, function(x) as.numeric(x)))
nv1 <- transform(nv1, q_12b_sumAll = rowSums(nv1, na.rm = TRUE))


# binarize it to 0s and NAs
nv1$q_12b_sumAll[nv1$q_12b_sumAll == 0] <- NA
#nv1$q_12b_sumAll[nv1$q_12b_sumAll >= 1] <- 1 #####################################

# then delete the "bad" ones (no sense to predict them) in the dataset 
df$q_12b_j_other <- NULL
df$q_12b_k_other <- NULL

# change the "wrong ones"
df$q_12b_j <- nv1$q_12b_j
df$q_12b_k <- nv1$q_12b_k

#and add this new variable
df$q_12b_sumAll <- nv1$q_12b_sumAll



# another "tick" - BUT ALSO CONDITIONAL VARIABLE - q_59_a,b,c,d
# what substance e-cigarettes contained
nv2 <- subset(df, select = c(q_59_a,
                             q_59_b,
                             q_59_c,
                             q_59_d))
nv2[!is.na(nv2)] <- 1
nv2[is.na(nv2)] <- NA

# creating new variable for missing analysis, but first to numeric:
nv2 <- as.data.frame(apply(nv2, 2, function(x) as.numeric(x)))
nv2 <- transform(nv2, q_59_abcd_sumAll = rowSums(nv2, na.rm = TRUE))

# binarize it to 0s and NAs
nv2$q_59_abcd_sumAll[nv2$q_59_abcd_sumAll == 0] <- NA
#nv2$q_59_abcd_sumAll[nv2$q_59_abcd_sumAll >= 1] <- 1

# "old" ones in df - do not delete, they may have some sense
# but add this new one - DON'T FORGET IT IS CONDITIONAL
df$q_59_abcd_sumAll <- nv2$q_59_abcd_sumAll

# activities variable 
nv3 <- subset(df, select = c(q_activities_1,
                             q_activities_2,
                             q_activities_3,
                             q_activities_4,
                             q_activities_5,
                             q_activities_6,
                             q_activities_7,
                             q_activities_8,
                             q_activities_9,
                             q_activities_10,
                             q_activities_11,
                             q_activities_12,
                             q_activities_13,
                             q_activities_14,
                             q_activities_15,
                             q_activities_16,
                             q_activities_17,
                             q_activities_18,
                             q_activities_19,
                             q_activities_20,
                             q_activities_21,
                             q_activities_22,
                             q_activities_23,
                             q_activities_24,
                             q_activities_25,
                             q_activities_26,
                             q_activities_27,
                             #q_activities_28, - THEY HAD ALL-NA SO EXCLUDED BEFORE
                             #q_activities_29,
                             #q_activities_30,
                             #q_activities_31,
                             q_activities_32,
                             q_activities_33,
                             q_activities_34,
                             q_activities_35,
                             q_activities_36,
                             #q_activities_37,
                             q_activities_38,
                             q_activities_39,
                             q_activities_40,
                             q_activities_41,
                             q_activities_42,
                             q_activities_43,
                             q_activities_44,
                             q_activities_other_1,
                             q_activities_other_2,
                             q_activities_other_3,
                             q_activities_other_4,
                             q_activities_other_5))
# THERE ARE EMPY PLACES NOT NA!!!
library(dplyr)
nv3 <- nv3 %>% mutate_all(na_if,"")
nv3<- as.data.frame(nv3)
# they are characters, so:
nv3[!is.na(nv3)] <- 1
nv3[is.na(nv3)] <- NA
nv3 <- as.data.frame(apply(nv3, 2, function(x) as.numeric(x)))
nv3 <- transform(nv3, q_activities_sumAll = rowSums(nv3, na.rm = TRUE))

# binarize it to 0s and NAs
nv3$q_activities_sumAll[nv3$q_activities_sumAll == 0] <- NA


# bc we are deleting all activities from MISSING ADATA ANALYSIS DATASET
df <- subset(df, select = -c(q_activities_1,
                             q_activities_2,
                             q_activities_3,
                             q_activities_4,
                             q_activities_5,
                             q_activities_6,
                             q_activities_7,
                             q_activities_8,
                             q_activities_9,
                             q_activities_10,
                             q_activities_11,
                             q_activities_12,
                             q_activities_13,
                             q_activities_14,
                             q_activities_15,
                             q_activities_16,
                             q_activities_17,
                             q_activities_18,
                             q_activities_19,
                             q_activities_20,
                             q_activities_21,
                             q_activities_22,
                             q_activities_23,
                             q_activities_24,
                             q_activities_25,
                             q_activities_26,
                             q_activities_27,
                             #q_activities_28, - THEY HAD ALL-NA SO EXCLUDED BEFORE
                             #q_activities_29,
                             #q_activities_30,
                             #q_activities_31,
                             q_activities_32,
                             q_activities_33,
                             q_activities_34,
                             q_activities_35,
                             q_activities_36,
                             #q_activities_37,
                             q_activities_38,
                             q_activities_39,
                             q_activities_40,
                             q_activities_41,
                             q_activities_42,
                             q_activities_43,
                             q_activities_44,
                             q_activities_other_1,
                             q_activities_other_2,
                             q_activities_other_3,
                             q_activities_other_4,
                             q_activities_other_5))



# and adding the new "good" one ### NOTE - HAVING ZERO ACTIVITIES WAS NOT AN OPTION - MEANING 0 AND NA ARE NA
df$q_activities_sumAll <- nv3$q_activities_sumAll 

# "OTHER"

# gender identity Q6
nv4 <- subset(df, select = c(q_6,
                             q_6_other))
nv4$q_6[nv4$q_6_other == "Male"] <- 1 # BUT MAYBE IT IS A FEMALE biologically
df$q_6 <- nv4$q_6
df$q_6_other <- NULL

# ethnicity Q8 -already excluded in the start
#nv5 <- subset(df, select = c(q_8,
 #                            q_8_c_other,
#                              q_8_d_other,
#                             q_8_e_other,
#                             q_8_f_other,
#                             q_8_g_other,
#                             q_8_h_other,
#                             q_8_i_other))

# gangs_text and gangs_other_text stays

#dim(df) 302 1056/1089????


df <- df %>% select(-contains("complete"))

df <- df %>% select(-contains("last_updated"))

df <- df %>% mutate_all(na_if,"")  


##################### CONDITIONAL QUESTIONS ########################

# 742 IN THE WHOLE DATASET; 93 NOT IN SOC. NET. BLOCK

df$q_3a_move <- NULL # we excluded it from na-analysis bc it can't be recoded
df$q_14b <- NULL # to small N - 1 non-NA
df$q_14c <- NULL # to small N - 2 non-NA

df$q_16b <- NULL # to small N  - 2 non-NA
df$q_16c <- NULL # to small N  - 2 non-NA


# going out - I will not treat this

nv5 <- subset(df, select = c(q_28_a,
                             q_28_b,
                             q_28_c,
                             q_28_d,
                             q_28_e,
                             q_28_f,
                             q_28_g))
# something is not adding up ?
df$q_28_b[df$q_28_a == 2] <- 0 
df$q_28_c[df$q_28_a == 2] <- 0
df$q_28_d[df$q_28_a == 2] <- 0
df$q_28_e[df$q_28_a == 2] <- 0
df$q_28_f[df$q_28_a == 2] <- 0
df$q_28_g[df$q_28_a == 2] <- 0

# smoking
# wrong info - cond 53, 53, 55, if 52 and 61b= 1
df$q_53[df$q_52 == 1] <- 0 # not adding up completely

uniquevalues_q54smoke <- c(unique(df$q_54_smoke))
length(uniquevalues_q54smoke) # 12
# had to cahnge this
df$q_54_smoke = car::recode(df$q_54_smoke, "uniquevalues_q54smoke[1] = NA; 
                        uniquevalues_q54smoke[2] = 0;
                        uniquevalues_q54smoke[3] = 0; 


                        uniquevalues_q54smoke[4] = 40;
                        uniquevalues_q54smoke[5] = 0;
                        uniquevalues_q54smoke[6] = 5;
                        uniquevalues_q54smoke[7] = 0;
                        uniquevalues_q54smoke[8] = 0;
                        uniquevalues_q54smoke[9] = 3;
                        uniquevalues_q54smoke[10] = 0;
                        uniquevalues_q54smoke[11] = 1;
                        uniquevalues_q54smoke[12] = 30")
df$q_54_smoke[df$q_52 == 1] <- 0

df$q_58[df$q_57 == 1] <- 0 # not adding up completely

df$q_59_a[df$q_57 == 1] <- 0
df$q_59_b[df$q_57 == 1] <- 0
df$q_59_c[df$q_57 == 1] <- 0
df$q_59_d[df$q_57 == 1] <- 0

df$q_59_abcd_sumAll[df$q_57 == 1] <- 0 # not adding up completely
df$q_60[df$q_57 == 1] <- 0 # not adding up completely

df$q_61b_a[df$q_57 == 1] <- 0# not adding up completely
df$q_61b_b[df$q_57 == 1] <- 0# not adding up completely
df$q_61b_c[df$q_57 == 1] <- 0# not adding up completely
df$q_61b_d[df$q_57 == 1] <- 0# not adding up completely
df$q_61b_e[df$q_57 == 1] <- 0# not adding up completely
df$q_61b_f[df$q_57 == 1] <- 0# not adding up completely
# df$q_61b_g[df$q_57 == 1] <- 0 - zero values or excluded?

# drinking
uniquevalues_q_63_age <- c(unique(df$q_63_age))
length(uniquevalues_q_63_age) # 19
# had to change
df$q_63_age = car::recode(df$q_63_age, "uniquevalues_q_63_age[1] = NA; 
                          uniquevalues_q_63_age[2] = 11;
                          uniquevalues_q_63_age[3] = 12; 
                          uniquevalues_q_63_age[4] = 13;
                          uniquevalues_q_63_age[5] = 4;
                          uniquevalues_q_63_age[6] = 11;
                          uniquevalues_q_63_age[7] = NA;
                          uniquevalues_q_63_age[8] = 7;
                          uniquevalues_q_63_age[9] = 10;
                          uniquevalues_q_63_age[10] = NA;
                          uniquevalues_q_63_age[11] = 8;
                          uniquevalues_q_63_age[12] = NA;
                          uniquevalues_q_63_age[13] = 14;
                          uniquevalues_q_63_age[14] = 14;
                          uniquevalues_q_63_age[15] = 15;
                          uniquevalues_q_63_age[16] = 13;
                          uniquevalues_q_63_age[17] = NA;
                          uniquevalues_q_63_age[18] = NA;
                          uniquevalues_q_63_age[19] = NA")


# df$q_63_age # adding zero makes no sense, stays conditional

df$q_64[df$q_62 == 2] <- 0

df$q_65[df$q_62 == 2] <- 0  # not adding up completely, but ok 

df$q_66[df$q_62 == 2] <- 0 # adds up completely

# dfq_67 will be tricky
# 5 is never so conditional missing should be NEVER (5) also

df$q_67_a[df$q_62 == 2] <- 5

df$q_67_b[df$q_62 == 2] <- 5

df$q_67_c[df$q_62 == 2] <- 5

df$q_67_d[df$q_62 == 2] <- 5

# dfq_68 will be tricky
# 1 is never so conditional missing should be 1 or 0 --> decided 0

df$q_68_a[df$q_62 == 2] <- 0
df$q_68_b[df$q_62 == 2] <- 0
df$q_68_c[df$q_62 == 2] <- 0
df$q_68_d[df$q_62 == 2] <- 0
df$q_68_e[df$q_62 == 2] <- 0

df$q_68_f[df$q_62 == 2] <- 0
df$q_68_g[df$q_62 == 2] <- 0
df$q_68_h[df$q_62 == 2] <- 0
df$q_68_i[df$q_62 == 2] <- 0
df$q_68_j[df$q_62 == 2] <- 0
df$q_68_k[df$q_62 == 2] <- 0
df$q_68_l[df$q_62 == 2] <- 0
df$q_68_m[df$q_62 == 2] <- 0
df$q_68_n[df$q_62 == 2] <- 0

df$q_68_o[df$q_62 == 2] <- 0
df$q_68_p[df$q_62 == 2] <- 0
df$q_68_q[df$q_62 == 2] <- 0
df$q_68_r[df$q_62 == 2] <- 0
df$q_68_s[df$q_62 == 2] <- 0
df$q_68_t[df$q_62 == 2] <- 0
df$q_68_u[df$q_62 == 2] <- 0


# DRUGS

# q 70: never used = 5, change conditional NAs to 5
df$q_70_a[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_b[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_c[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_d[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_e[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_f[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_g[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_h[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_i[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_j[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_k[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_l[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_70_m[df$q_69 == 1 | df$q_69 == 3 ] <- 5

df$q_71_a[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_71_b[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_71_c[df$q_69 == 1 | df$q_69 == 3 ] <- 5
df$q_71_d[df$q_69 == 1 | df$q_69 == 3 ] <- 5

dim(df)# 302 1077

### SOCIAL NETWORK BLOCK 

# same clubs activity with friends -> NOTE: NA may be that they not share 
# any activities together (although there is an option "other")

fr1 <- subset(df, select = c(q_net_friend_1_same_clubs_activity_1,
                             q_net_friend_1_same_clubs_activity_2,
                             q_net_friend_1_same_clubs_activity_3,
                             q_net_friend_1_same_clubs_activity_4,
                             q_net_friend_1_same_clubs_activity_5,
                             q_net_friend_1_same_clubs_activity_6,
                             q_net_friend_1_same_clubs_activity_7,
                             q_net_friend_1_same_clubs_activity_8,
                             q_net_friend_1_same_clubs_activity_9,
                             q_net_friend_1_same_clubs_activity_10,
                             q_net_friend_1_same_clubs_activity_11,
                             q_net_friend_1_same_clubs_activity_12,
                             q_net_friend_1_same_clubs_activity_13,
                             q_net_friend_1_same_clubs_activity_14,
                             q_net_friend_1_same_clubs_activity_15,
                             q_net_friend_1_same_clubs_activity_16,
                             q_net_friend_1_same_clubs_activity_17,
                             q_net_friend_1_same_clubs_activity_18,
                             q_net_friend_1_same_clubs_activity_19,
                             q_net_friend_1_same_clubs_activity_20,
                             q_net_friend_1_same_clubs_activity_21,
                             q_net_friend_1_same_clubs_activity_22,
                             q_net_friend_1_same_clubs_activity_23,
                             q_net_friend_1_same_clubs_activity_24,
                             q_net_friend_1_same_clubs_activity_25,
                             q_net_friend_1_same_clubs_activity_26,
                             q_net_friend_1_same_clubs_activity_27,
                             q_net_friend_1_same_clubs_activity_28,
                             q_net_friend_1_same_clubs_activity_29,
                             q_net_friend_1_same_clubs_activity_30,
                             q_net_friend_1_same_clubs_activity_31,
                             q_net_friend_1_same_clubs_activity_32,
                             q_net_friend_1_same_clubs_activity_33,
                             q_net_friend_1_same_clubs_activity_34,
                             q_net_friend_1_same_clubs_activity_35,
                             q_net_friend_1_same_clubs_activity_36,
                             q_net_friend_1_same_clubs_activity_37,
                             q_net_friend_1_same_clubs_activity_38,
                             q_net_friend_1_same_clubs_activity_39,
                             q_net_friend_1_same_clubs_activity_40,
                             q_net_friend_1_same_clubs_activity_41,
                             q_net_friend_1_same_clubs_activity_42,
                             q_net_friend_1_same_clubs_activity_43,
                             q_net_friend_1_same_clubs_activity_44,
                             q_net_friend_1_same_clubs_other_1,
                             q_net_friend_1_same_clubs_other_2,
                             q_net_friend_1_same_clubs_other_3,
                             q_net_friend_1_same_clubs_other_4))#,
                            # q_net_friend_1_same_clubs_other_text))

fr1 <- as.data.frame(fr1)
head(fr1)
fr1[!is.na(fr1)] <- 1
fr1[is.na(fr1)] <- 0
fr1 <- as.data.frame(apply(fr1, 2, function(x) as.numeric(x)))
fr1 <- transform(fr1, q_net_friend_1_same_clubs_sumAll = rowSums(fr1, na.rm = TRUE))

# binarize it to 0s and NAs
fr1$q_net_friend_1_same_clubs_sumAll[fr1$q_net_friend_1_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_1_same_clubs_sumAll <- fr1$q_net_friend_1_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_1_same_clubs_activity_1,
                             q_net_friend_1_same_clubs_activity_2,
                             q_net_friend_1_same_clubs_activity_3,
                             q_net_friend_1_same_clubs_activity_4,
                             q_net_friend_1_same_clubs_activity_5,
                             q_net_friend_1_same_clubs_activity_6,
                             q_net_friend_1_same_clubs_activity_7,
                             q_net_friend_1_same_clubs_activity_8,
                             q_net_friend_1_same_clubs_activity_9,
                             q_net_friend_1_same_clubs_activity_10,
                             q_net_friend_1_same_clubs_activity_11,
                             q_net_friend_1_same_clubs_activity_12,
                             q_net_friend_1_same_clubs_activity_13,
                             q_net_friend_1_same_clubs_activity_14,
                             q_net_friend_1_same_clubs_activity_15,
                             q_net_friend_1_same_clubs_activity_16,
                             q_net_friend_1_same_clubs_activity_17,
                             q_net_friend_1_same_clubs_activity_18,
                             q_net_friend_1_same_clubs_activity_19,
                             q_net_friend_1_same_clubs_activity_20,
                             q_net_friend_1_same_clubs_activity_21,
                             q_net_friend_1_same_clubs_activity_22,
                             q_net_friend_1_same_clubs_activity_23,
                             q_net_friend_1_same_clubs_activity_24,
                             q_net_friend_1_same_clubs_activity_25,
                             q_net_friend_1_same_clubs_activity_26,
                             q_net_friend_1_same_clubs_activity_27,
                             q_net_friend_1_same_clubs_activity_28,
                             q_net_friend_1_same_clubs_activity_29,
                             q_net_friend_1_same_clubs_activity_30,
                             q_net_friend_1_same_clubs_activity_31,
                             q_net_friend_1_same_clubs_activity_32,
                             q_net_friend_1_same_clubs_activity_33,
                             q_net_friend_1_same_clubs_activity_34,
                             q_net_friend_1_same_clubs_activity_35,
                             q_net_friend_1_same_clubs_activity_36,
                             q_net_friend_1_same_clubs_activity_37,
                             q_net_friend_1_same_clubs_activity_38,
                             q_net_friend_1_same_clubs_activity_39,
                             q_net_friend_1_same_clubs_activity_40,
                             q_net_friend_1_same_clubs_activity_41,
                             q_net_friend_1_same_clubs_activity_42,
                             q_net_friend_1_same_clubs_activity_43,
                             q_net_friend_1_same_clubs_activity_44,
                             q_net_friend_1_same_clubs_other_1,
                             q_net_friend_1_same_clubs_other_2,
                             q_net_friend_1_same_clubs_other_3,
                             q_net_friend_1_same_clubs_other_4,
                             q_net_friend_1_same_clubs_other_text))


fr2 <- subset(df, select = c(q_net_friend_2_same_clubs_activity_1,
                             q_net_friend_2_same_clubs_activity_2,
                             q_net_friend_2_same_clubs_activity_3,
                             q_net_friend_2_same_clubs_activity_4,
                             q_net_friend_2_same_clubs_activity_5,
                             q_net_friend_2_same_clubs_activity_6,
                             q_net_friend_2_same_clubs_activity_7,
                             q_net_friend_2_same_clubs_activity_8,
                             q_net_friend_2_same_clubs_activity_9,
                             q_net_friend_2_same_clubs_activity_10,
                             q_net_friend_2_same_clubs_activity_11,
                             q_net_friend_2_same_clubs_activity_12,
                             q_net_friend_2_same_clubs_activity_13,
                             q_net_friend_2_same_clubs_activity_14,
                             q_net_friend_2_same_clubs_activity_15,
                             q_net_friend_2_same_clubs_activity_16,
                             q_net_friend_2_same_clubs_activity_17,
                             q_net_friend_2_same_clubs_activity_18,
                             q_net_friend_2_same_clubs_activity_19,
                             q_net_friend_2_same_clubs_activity_20,
                             q_net_friend_2_same_clubs_activity_21,
                             q_net_friend_2_same_clubs_activity_22,
                             q_net_friend_2_same_clubs_activity_23,
                             q_net_friend_2_same_clubs_activity_24,
                             q_net_friend_2_same_clubs_activity_25,
                             q_net_friend_2_same_clubs_activity_26,
                             q_net_friend_2_same_clubs_activity_27,
                             q_net_friend_2_same_clubs_activity_28,
                             q_net_friend_2_same_clubs_activity_29,
                             q_net_friend_2_same_clubs_activity_30,
                             q_net_friend_2_same_clubs_activity_31,
                             q_net_friend_2_same_clubs_activity_32,
                             q_net_friend_2_same_clubs_activity_33,
                             q_net_friend_2_same_clubs_activity_34,
                             q_net_friend_2_same_clubs_activity_35,
                             q_net_friend_2_same_clubs_activity_36,
                             q_net_friend_2_same_clubs_activity_37,
                             q_net_friend_2_same_clubs_activity_38,
                             q_net_friend_2_same_clubs_activity_39,
                             q_net_friend_2_same_clubs_activity_40,
                             q_net_friend_2_same_clubs_activity_41,
                             q_net_friend_2_same_clubs_activity_42,
                             q_net_friend_2_same_clubs_activity_43,
                             q_net_friend_2_same_clubs_activity_44,
                             q_net_friend_2_same_clubs_other_1,
                             q_net_friend_2_same_clubs_other_2,
                             q_net_friend_2_same_clubs_other_3,
                             q_net_friend_2_same_clubs_other_4))#,
                             ##q_net_friend_2_same_clubs_other_text))

fr2[!is.na(fr2)] <- 1
fr2[is.na(fr2)] <- 0
fr2 <- as.data.frame(apply(fr2, 2, function(x) as.numeric(x)))
fr2 <- transform(fr2, q_net_friend_2_same_clubs_sumAll = rowSums(fr2, na.rm = TRUE))

# binarize it to 0s and NAs
fr2$q_net_friend_2_same_clubs_sumAll[fr2$q_net_friend_2_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_2_same_clubs_sumAll <- fr2$q_net_friend_2_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_2_same_clubs_activity_1,
                             q_net_friend_2_same_clubs_activity_2,
                             q_net_friend_2_same_clubs_activity_3,
                             q_net_friend_2_same_clubs_activity_4,
                             q_net_friend_2_same_clubs_activity_5,
                             q_net_friend_2_same_clubs_activity_6,
                             q_net_friend_2_same_clubs_activity_7,
                             q_net_friend_2_same_clubs_activity_8,
                             q_net_friend_2_same_clubs_activity_9,
                             q_net_friend_2_same_clubs_activity_10,
                             q_net_friend_2_same_clubs_activity_11,
                             q_net_friend_2_same_clubs_activity_12,
                             q_net_friend_2_same_clubs_activity_13,
                             q_net_friend_2_same_clubs_activity_14,
                             q_net_friend_2_same_clubs_activity_15,
                             q_net_friend_2_same_clubs_activity_16,
                             q_net_friend_2_same_clubs_activity_17,
                             q_net_friend_2_same_clubs_activity_18,
                             q_net_friend_2_same_clubs_activity_19,
                             q_net_friend_2_same_clubs_activity_20,
                             q_net_friend_2_same_clubs_activity_21,
                             q_net_friend_2_same_clubs_activity_22,
                             q_net_friend_2_same_clubs_activity_23,
                             q_net_friend_2_same_clubs_activity_24,
                             q_net_friend_2_same_clubs_activity_25,
                             q_net_friend_2_same_clubs_activity_26,
                             q_net_friend_2_same_clubs_activity_27,
                             q_net_friend_2_same_clubs_activity_28,
                             q_net_friend_2_same_clubs_activity_29,
                             q_net_friend_2_same_clubs_activity_30,
                             q_net_friend_2_same_clubs_activity_31,
                             q_net_friend_2_same_clubs_activity_32,
                             q_net_friend_2_same_clubs_activity_33,
                             q_net_friend_2_same_clubs_activity_34,
                             q_net_friend_2_same_clubs_activity_35,
                             q_net_friend_2_same_clubs_activity_36,
                             q_net_friend_2_same_clubs_activity_37,
                             q_net_friend_2_same_clubs_activity_38,
                             q_net_friend_2_same_clubs_activity_39,
                             q_net_friend_2_same_clubs_activity_40,
                             q_net_friend_2_same_clubs_activity_41,
                             q_net_friend_2_same_clubs_activity_42,
                             q_net_friend_2_same_clubs_activity_43,
                             q_net_friend_2_same_clubs_activity_44,
                             q_net_friend_2_same_clubs_other_1,
                             q_net_friend_2_same_clubs_other_2,
                             q_net_friend_2_same_clubs_other_3,
                             q_net_friend_2_same_clubs_other_4,
                             q_net_friend_2_same_clubs_other_text))


fr3 <- subset(df, select = c(q_net_friend_3_same_clubs_activity_1,
                             q_net_friend_3_same_clubs_activity_2,
                             q_net_friend_3_same_clubs_activity_3,
                             q_net_friend_3_same_clubs_activity_4,
                             q_net_friend_3_same_clubs_activity_5,
                             q_net_friend_3_same_clubs_activity_6,
                             q_net_friend_3_same_clubs_activity_7,
                             q_net_friend_3_same_clubs_activity_8,
                             q_net_friend_3_same_clubs_activity_9,
                             q_net_friend_3_same_clubs_activity_10,
                             q_net_friend_3_same_clubs_activity_11,
                             q_net_friend_3_same_clubs_activity_12,
                             q_net_friend_3_same_clubs_activity_13,
                             q_net_friend_3_same_clubs_activity_14,
                             q_net_friend_3_same_clubs_activity_15,
                             q_net_friend_3_same_clubs_activity_16,
                             q_net_friend_3_same_clubs_activity_17,
                             q_net_friend_3_same_clubs_activity_18,
                             q_net_friend_3_same_clubs_activity_19,
                             q_net_friend_3_same_clubs_activity_20,
                             q_net_friend_3_same_clubs_activity_21,
                             q_net_friend_3_same_clubs_activity_22,
                             q_net_friend_3_same_clubs_activity_23,
                             q_net_friend_3_same_clubs_activity_24,
                             q_net_friend_3_same_clubs_activity_25,
                             q_net_friend_3_same_clubs_activity_26,
                             q_net_friend_3_same_clubs_activity_27,
                             q_net_friend_3_same_clubs_activity_28,
                             q_net_friend_3_same_clubs_activity_29,
                             q_net_friend_3_same_clubs_activity_30,
                             q_net_friend_3_same_clubs_activity_31,
                             q_net_friend_3_same_clubs_activity_32,
                             q_net_friend_3_same_clubs_activity_33,
                             q_net_friend_3_same_clubs_activity_34,
                             q_net_friend_3_same_clubs_activity_35,
                             q_net_friend_3_same_clubs_activity_36,
                             q_net_friend_3_same_clubs_activity_37,
                             q_net_friend_3_same_clubs_activity_38,
                             q_net_friend_3_same_clubs_activity_39,
                             q_net_friend_3_same_clubs_activity_40,
                             q_net_friend_3_same_clubs_activity_41,
                             q_net_friend_3_same_clubs_activity_42,
                             q_net_friend_3_same_clubs_activity_43,
                             q_net_friend_3_same_clubs_activity_44,
                             q_net_friend_3_same_clubs_other_1,
                             q_net_friend_3_same_clubs_other_2,
                             q_net_friend_3_same_clubs_other_3,
                             q_net_friend_3_same_clubs_other_4))#,
                             #q_net_friend_3_same_clubs_other_text))

fr3[!is.na(fr3)] <- 1
fr3[is.na(fr3)] <- 0
fr3 <- as.data.frame(apply(fr3, 2, function(x) as.numeric(x)))
fr3 <- transform(fr3, q_net_friend_3_same_clubs_sumAll = rowSums(fr3, na.rm = TRUE))

# binarize it to 0s and NAs
fr3$q_net_friend_3_same_clubs_sumAll[fr3$q_net_friend_3_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_3_same_clubs_sumAll <- fr3$q_net_friend_3_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_3_same_clubs_activity_1,
                             q_net_friend_3_same_clubs_activity_2,
                             q_net_friend_3_same_clubs_activity_3,
                             q_net_friend_3_same_clubs_activity_4,
                             q_net_friend_3_same_clubs_activity_5,
                             q_net_friend_3_same_clubs_activity_6,
                             q_net_friend_3_same_clubs_activity_7,
                             q_net_friend_3_same_clubs_activity_8,
                             q_net_friend_3_same_clubs_activity_9,
                             q_net_friend_3_same_clubs_activity_10,
                             q_net_friend_3_same_clubs_activity_11,
                             q_net_friend_3_same_clubs_activity_12,
                             q_net_friend_3_same_clubs_activity_13,
                             q_net_friend_3_same_clubs_activity_14,
                             q_net_friend_3_same_clubs_activity_15,
                             q_net_friend_3_same_clubs_activity_16,
                             q_net_friend_3_same_clubs_activity_17,
                             q_net_friend_3_same_clubs_activity_18,
                             q_net_friend_3_same_clubs_activity_19,
                             q_net_friend_3_same_clubs_activity_20,
                             q_net_friend_3_same_clubs_activity_21,
                             q_net_friend_3_same_clubs_activity_22,
                             q_net_friend_3_same_clubs_activity_23,
                             q_net_friend_3_same_clubs_activity_24,
                             q_net_friend_3_same_clubs_activity_25,
                             q_net_friend_3_same_clubs_activity_26,
                             q_net_friend_3_same_clubs_activity_27,
                             q_net_friend_3_same_clubs_activity_28,
                             q_net_friend_3_same_clubs_activity_29,
                             q_net_friend_3_same_clubs_activity_30,
                             q_net_friend_3_same_clubs_activity_31,
                             q_net_friend_3_same_clubs_activity_32,
                             q_net_friend_3_same_clubs_activity_33,
                             q_net_friend_3_same_clubs_activity_34,
                             q_net_friend_3_same_clubs_activity_35,
                             q_net_friend_3_same_clubs_activity_36,
                             q_net_friend_3_same_clubs_activity_37,
                             q_net_friend_3_same_clubs_activity_38,
                             q_net_friend_3_same_clubs_activity_39,
                             q_net_friend_3_same_clubs_activity_40,
                             q_net_friend_3_same_clubs_activity_41,
                             q_net_friend_3_same_clubs_activity_42,
                             q_net_friend_3_same_clubs_activity_43,
                             q_net_friend_3_same_clubs_activity_44,
                             q_net_friend_3_same_clubs_other_1,
                             q_net_friend_3_same_clubs_other_2,
                             q_net_friend_3_same_clubs_other_3,
                             q_net_friend_3_same_clubs_other_4,
                             q_net_friend_3_same_clubs_other_text))

fr4 <- subset(df, select = c(q_net_friend_4_same_clubs_activity_1,
                             q_net_friend_4_same_clubs_activity_2,
                             q_net_friend_4_same_clubs_activity_3,
                             q_net_friend_4_same_clubs_activity_4,
                             q_net_friend_4_same_clubs_activity_5,
                             q_net_friend_4_same_clubs_activity_6,
                             q_net_friend_4_same_clubs_activity_7,
                             q_net_friend_4_same_clubs_activity_8,
                             q_net_friend_4_same_clubs_activity_9,
                             q_net_friend_4_same_clubs_activity_10,
                             q_net_friend_4_same_clubs_activity_11,
                             q_net_friend_4_same_clubs_activity_12,
                             q_net_friend_4_same_clubs_activity_13,
                             q_net_friend_4_same_clubs_activity_14,
                             q_net_friend_4_same_clubs_activity_15,
                             q_net_friend_4_same_clubs_activity_16,
                             q_net_friend_4_same_clubs_activity_17,
                             q_net_friend_4_same_clubs_activity_18,
                             q_net_friend_4_same_clubs_activity_19,
                             q_net_friend_4_same_clubs_activity_20,
                             q_net_friend_4_same_clubs_activity_21,
                             q_net_friend_4_same_clubs_activity_22,
                             q_net_friend_4_same_clubs_activity_23,
                             q_net_friend_4_same_clubs_activity_24,
                             q_net_friend_4_same_clubs_activity_25,
                             q_net_friend_4_same_clubs_activity_26,
                             q_net_friend_4_same_clubs_activity_27,
                             q_net_friend_4_same_clubs_activity_28,
                             q_net_friend_4_same_clubs_activity_29,
                             q_net_friend_4_same_clubs_activity_30,
                             q_net_friend_4_same_clubs_activity_31,
                             q_net_friend_4_same_clubs_activity_32,
                             q_net_friend_4_same_clubs_activity_33,
                             q_net_friend_4_same_clubs_activity_34,
                             q_net_friend_4_same_clubs_activity_35,
                             q_net_friend_4_same_clubs_activity_36,
                             q_net_friend_4_same_clubs_activity_37,
                             q_net_friend_4_same_clubs_activity_38,
                             q_net_friend_4_same_clubs_activity_39,
                             q_net_friend_4_same_clubs_activity_40,
                             q_net_friend_4_same_clubs_activity_41,
                             q_net_friend_4_same_clubs_activity_42,
                             q_net_friend_4_same_clubs_activity_43,
                             q_net_friend_4_same_clubs_activity_44,
                             q_net_friend_4_same_clubs_other_1,
                             q_net_friend_4_same_clubs_other_2,
                             q_net_friend_4_same_clubs_other_3,
                             q_net_friend_4_same_clubs_other_4))#,
                             ##q_net_friend_4_same_clubs_other_text))

fr4[!is.na(fr4)] <- 1
fr4[is.na(fr4)] <- 0
fr4 <- as.data.frame(apply(fr4, 2, function(x) as.numeric(x)))
fr4 <- transform(fr4, q_net_friend_4_same_clubs_sumAll = rowSums(fr4, na.rm = TRUE))

# binarize it to 0s and NAs
fr4$q_net_friend_4_same_clubs_sumAll[fr4$q_net_friend_4_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_4_same_clubs_sumAll <- fr4$q_net_friend_4_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_4_same_clubs_activity_1,
                             q_net_friend_4_same_clubs_activity_2,
                             q_net_friend_4_same_clubs_activity_3,
                             q_net_friend_4_same_clubs_activity_4,
                             q_net_friend_4_same_clubs_activity_5,
                             q_net_friend_4_same_clubs_activity_6,
                             q_net_friend_4_same_clubs_activity_7,
                             q_net_friend_4_same_clubs_activity_8,
                             q_net_friend_4_same_clubs_activity_9,
                             q_net_friend_4_same_clubs_activity_10,
                             q_net_friend_4_same_clubs_activity_11,
                             q_net_friend_4_same_clubs_activity_12,
                             q_net_friend_4_same_clubs_activity_13,
                             q_net_friend_4_same_clubs_activity_14,
                             q_net_friend_4_same_clubs_activity_15,
                             q_net_friend_4_same_clubs_activity_16,
                             q_net_friend_4_same_clubs_activity_17,
                             q_net_friend_4_same_clubs_activity_18,
                             q_net_friend_4_same_clubs_activity_19,
                             q_net_friend_4_same_clubs_activity_20,
                             q_net_friend_4_same_clubs_activity_21,
                             q_net_friend_4_same_clubs_activity_22,
                             q_net_friend_4_same_clubs_activity_23,
                             q_net_friend_4_same_clubs_activity_24,
                             q_net_friend_4_same_clubs_activity_25,
                             q_net_friend_4_same_clubs_activity_26,
                             q_net_friend_4_same_clubs_activity_27,
                             q_net_friend_4_same_clubs_activity_28,
                             q_net_friend_4_same_clubs_activity_29,
                             q_net_friend_4_same_clubs_activity_30,
                             q_net_friend_4_same_clubs_activity_31,
                             q_net_friend_4_same_clubs_activity_32,
                             q_net_friend_4_same_clubs_activity_33,
                             q_net_friend_4_same_clubs_activity_34,
                             q_net_friend_4_same_clubs_activity_35,
                             q_net_friend_4_same_clubs_activity_36,
                             q_net_friend_4_same_clubs_activity_37,
                             q_net_friend_4_same_clubs_activity_38,
                             q_net_friend_4_same_clubs_activity_39,
                             q_net_friend_4_same_clubs_activity_40,
                             q_net_friend_4_same_clubs_activity_41,
                             q_net_friend_4_same_clubs_activity_42,
                             q_net_friend_4_same_clubs_activity_43,
                             q_net_friend_4_same_clubs_activity_44,
                             q_net_friend_4_same_clubs_other_1,
                             q_net_friend_4_same_clubs_other_2,
                             q_net_friend_4_same_clubs_other_3,
                             q_net_friend_4_same_clubs_other_4,
                             q_net_friend_4_same_clubs_other_text))


fr5 <- subset(df, select = c(q_net_friend_5_same_clubs_activity_1,
                             q_net_friend_5_same_clubs_activity_2,
                             q_net_friend_5_same_clubs_activity_3,
                             q_net_friend_5_same_clubs_activity_4,
                             q_net_friend_5_same_clubs_activity_5,
                             q_net_friend_5_same_clubs_activity_6,
                             q_net_friend_5_same_clubs_activity_7,
                             q_net_friend_5_same_clubs_activity_8,
                             q_net_friend_5_same_clubs_activity_9,
                             q_net_friend_5_same_clubs_activity_10,
                             q_net_friend_5_same_clubs_activity_11,
                             q_net_friend_5_same_clubs_activity_12,
                             q_net_friend_5_same_clubs_activity_13,
                             q_net_friend_5_same_clubs_activity_14,
                             q_net_friend_5_same_clubs_activity_15,
                             q_net_friend_5_same_clubs_activity_16,
                             q_net_friend_5_same_clubs_activity_17,
                             q_net_friend_5_same_clubs_activity_18,
                             q_net_friend_5_same_clubs_activity_19,
                             q_net_friend_5_same_clubs_activity_20,
                             q_net_friend_5_same_clubs_activity_21,
                             q_net_friend_5_same_clubs_activity_22,
                             q_net_friend_5_same_clubs_activity_23,
                             q_net_friend_5_same_clubs_activity_24,
                             q_net_friend_5_same_clubs_activity_25,
                             q_net_friend_5_same_clubs_activity_26,
                             q_net_friend_5_same_clubs_activity_27,
                             q_net_friend_5_same_clubs_activity_28,
                             q_net_friend_5_same_clubs_activity_29,
                             q_net_friend_5_same_clubs_activity_30,
                             q_net_friend_5_same_clubs_activity_31,
                             q_net_friend_5_same_clubs_activity_32,
                             q_net_friend_5_same_clubs_activity_33,
                             q_net_friend_5_same_clubs_activity_34,
                             q_net_friend_5_same_clubs_activity_35,
                             q_net_friend_5_same_clubs_activity_36,
                             q_net_friend_5_same_clubs_activity_37,
                             q_net_friend_5_same_clubs_activity_38,
                             q_net_friend_5_same_clubs_activity_39,
                             q_net_friend_5_same_clubs_activity_40,
                             q_net_friend_5_same_clubs_activity_41,
                             q_net_friend_5_same_clubs_activity_42,
                             q_net_friend_5_same_clubs_activity_43,
                             q_net_friend_5_same_clubs_activity_44,
                             q_net_friend_5_same_clubs_other_1,
                             q_net_friend_5_same_clubs_other_2,
                             q_net_friend_5_same_clubs_other_3,
                             q_net_friend_5_same_clubs_other_4))#,
                             ##q_net_friend_5_same_clubs_other_text))

fr5[!is.na(fr5)] <- 1
fr5[is.na(fr5)] <- 0
fr5 <- as.data.frame(apply(fr5, 2, function(x) as.numeric(x)))
fr5 <- transform(fr5, q_net_friend_5_same_clubs_sumAll = rowSums(fr5, na.rm = TRUE))

# binarize it to 0s and NAs
fr5$q_net_friend_5_same_clubs_sumAll[fr5$q_net_friend_5_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_5_same_clubs_sumAll <- fr5$q_net_friend_5_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_5_same_clubs_activity_1,
                             q_net_friend_5_same_clubs_activity_2,
                             q_net_friend_5_same_clubs_activity_3,
                             q_net_friend_5_same_clubs_activity_4,
                             q_net_friend_5_same_clubs_activity_5,
                             q_net_friend_5_same_clubs_activity_6,
                             q_net_friend_5_same_clubs_activity_7,
                             q_net_friend_5_same_clubs_activity_8,
                             q_net_friend_5_same_clubs_activity_9,
                             q_net_friend_5_same_clubs_activity_10,
                             q_net_friend_5_same_clubs_activity_11,
                             q_net_friend_5_same_clubs_activity_12,
                             q_net_friend_5_same_clubs_activity_13,
                             q_net_friend_5_same_clubs_activity_14,
                             q_net_friend_5_same_clubs_activity_15,
                             q_net_friend_5_same_clubs_activity_16,
                             q_net_friend_5_same_clubs_activity_17,
                             q_net_friend_5_same_clubs_activity_18,
                             q_net_friend_5_same_clubs_activity_19,
                             q_net_friend_5_same_clubs_activity_20,
                             q_net_friend_5_same_clubs_activity_21,
                             q_net_friend_5_same_clubs_activity_22,
                             q_net_friend_5_same_clubs_activity_23,
                             q_net_friend_5_same_clubs_activity_24,
                             q_net_friend_5_same_clubs_activity_25,
                             q_net_friend_5_same_clubs_activity_26,
                             q_net_friend_5_same_clubs_activity_27,
                             q_net_friend_5_same_clubs_activity_28,
                             q_net_friend_5_same_clubs_activity_29,
                             q_net_friend_5_same_clubs_activity_30,
                             q_net_friend_5_same_clubs_activity_31,
                             q_net_friend_5_same_clubs_activity_32,
                             q_net_friend_5_same_clubs_activity_33,
                             q_net_friend_5_same_clubs_activity_34,
                             q_net_friend_5_same_clubs_activity_35,
                             q_net_friend_5_same_clubs_activity_36,
                             q_net_friend_5_same_clubs_activity_37,
                             q_net_friend_5_same_clubs_activity_38,
                             q_net_friend_5_same_clubs_activity_39,
                             q_net_friend_5_same_clubs_activity_40,
                             q_net_friend_5_same_clubs_activity_41,
                             q_net_friend_5_same_clubs_activity_42,
                             q_net_friend_5_same_clubs_activity_43,
                             q_net_friend_5_same_clubs_activity_44,
                             q_net_friend_5_same_clubs_other_1,
                             q_net_friend_5_same_clubs_other_2,
                             q_net_friend_5_same_clubs_other_3,
                             q_net_friend_5_same_clubs_other_4,
                             q_net_friend_5_same_clubs_other_text))

fr6 <- subset(df, select = c(q_net_friend_6_same_clubs_activity_1,
                             q_net_friend_6_same_clubs_activity_2,
                             q_net_friend_6_same_clubs_activity_3,
                             q_net_friend_6_same_clubs_activity_4,
                             q_net_friend_6_same_clubs_activity_5,
                             q_net_friend_6_same_clubs_activity_6,
                             q_net_friend_6_same_clubs_activity_7,
                             q_net_friend_6_same_clubs_activity_8,
                             q_net_friend_6_same_clubs_activity_9,
                             q_net_friend_6_same_clubs_activity_10,
                             q_net_friend_6_same_clubs_activity_11,
                             q_net_friend_6_same_clubs_activity_12,
                             q_net_friend_6_same_clubs_activity_13,
                             q_net_friend_6_same_clubs_activity_14,
                             q_net_friend_6_same_clubs_activity_15,
                             q_net_friend_6_same_clubs_activity_16,
                             q_net_friend_6_same_clubs_activity_17,
                             q_net_friend_6_same_clubs_activity_18,
                             q_net_friend_6_same_clubs_activity_19,
                             q_net_friend_6_same_clubs_activity_20,
                             q_net_friend_6_same_clubs_activity_21,
                             q_net_friend_6_same_clubs_activity_22,
                             q_net_friend_6_same_clubs_activity_23,
                             q_net_friend_6_same_clubs_activity_24,
                             q_net_friend_6_same_clubs_activity_25,
                             q_net_friend_6_same_clubs_activity_26,
                             q_net_friend_6_same_clubs_activity_27,
                             q_net_friend_6_same_clubs_activity_28,
                             q_net_friend_6_same_clubs_activity_29,
                             q_net_friend_6_same_clubs_activity_30,
                             q_net_friend_6_same_clubs_activity_31,
                             q_net_friend_6_same_clubs_activity_32,
                             q_net_friend_6_same_clubs_activity_33,
                             q_net_friend_6_same_clubs_activity_34,
                             q_net_friend_6_same_clubs_activity_35,
                             q_net_friend_6_same_clubs_activity_36,
                             q_net_friend_6_same_clubs_activity_37,
                             q_net_friend_6_same_clubs_activity_38,
                             q_net_friend_6_same_clubs_activity_39,
                             q_net_friend_6_same_clubs_activity_40,
                             q_net_friend_6_same_clubs_activity_41,
                             q_net_friend_6_same_clubs_activity_42,
                             q_net_friend_6_same_clubs_activity_43,
                             q_net_friend_6_same_clubs_activity_44,
                             q_net_friend_6_same_clubs_other_1,
                             q_net_friend_6_same_clubs_other_2,
                             q_net_friend_6_same_clubs_other_3,
                             q_net_friend_6_same_clubs_other_4))#,
                             ##q_net_friend_6_same_clubs_other_text))

fr6[!is.na(fr6)] <- 1
fr6[is.na(fr6)] <- 0
fr6 <- as.data.frame(apply(fr6, 2, function(x) as.numeric(x)))
fr6 <- transform(fr6, q_net_friend_6_same_clubs_sumAll = rowSums(fr6, na.rm = TRUE))

# binarize it to 0s and NAs
fr6$q_net_friend_6_same_clubs_sumAll[fr6$q_net_friend_6_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_6_same_clubs_sumAll <- fr6$q_net_friend_6_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_6_same_clubs_activity_1,
                             q_net_friend_6_same_clubs_activity_2,
                             q_net_friend_6_same_clubs_activity_3,
                             q_net_friend_6_same_clubs_activity_4,
                             q_net_friend_6_same_clubs_activity_5,
                             q_net_friend_6_same_clubs_activity_6,
                             q_net_friend_6_same_clubs_activity_7,
                             q_net_friend_6_same_clubs_activity_8,
                             q_net_friend_6_same_clubs_activity_9,
                             q_net_friend_6_same_clubs_activity_10,
                             q_net_friend_6_same_clubs_activity_11,
                             q_net_friend_6_same_clubs_activity_12,
                             q_net_friend_6_same_clubs_activity_13,
                             q_net_friend_6_same_clubs_activity_14,
                             q_net_friend_6_same_clubs_activity_15,
                             q_net_friend_6_same_clubs_activity_16,
                             q_net_friend_6_same_clubs_activity_17,
                             q_net_friend_6_same_clubs_activity_18,
                             q_net_friend_6_same_clubs_activity_19,
                             q_net_friend_6_same_clubs_activity_20,
                             q_net_friend_6_same_clubs_activity_21,
                             q_net_friend_6_same_clubs_activity_22,
                             q_net_friend_6_same_clubs_activity_23,
                             q_net_friend_6_same_clubs_activity_24,
                             q_net_friend_6_same_clubs_activity_25,
                             q_net_friend_6_same_clubs_activity_26,
                             q_net_friend_6_same_clubs_activity_27,
                             q_net_friend_6_same_clubs_activity_28,
                             q_net_friend_6_same_clubs_activity_29,
                             q_net_friend_6_same_clubs_activity_30,
                             q_net_friend_6_same_clubs_activity_31,
                             q_net_friend_6_same_clubs_activity_32,
                             q_net_friend_6_same_clubs_activity_33,
                             q_net_friend_6_same_clubs_activity_34,
                             q_net_friend_6_same_clubs_activity_35,
                             q_net_friend_6_same_clubs_activity_36,
                             q_net_friend_6_same_clubs_activity_37,
                             q_net_friend_6_same_clubs_activity_38,
                             q_net_friend_6_same_clubs_activity_39,
                             q_net_friend_6_same_clubs_activity_40,
                             q_net_friend_6_same_clubs_activity_41,
                             q_net_friend_6_same_clubs_activity_42,
                             q_net_friend_6_same_clubs_activity_43,
                             q_net_friend_6_same_clubs_activity_44,
                             q_net_friend_6_same_clubs_other_1,
                             q_net_friend_6_same_clubs_other_2,
                             q_net_friend_6_same_clubs_other_3,
                             q_net_friend_6_same_clubs_other_4,
                             q_net_friend_6_same_clubs_other_text))

fr7 <- subset(df, select = c(q_net_friend_7_same_clubs_activity_1,
                             q_net_friend_7_same_clubs_activity_2,
                             q_net_friend_7_same_clubs_activity_3,
                             q_net_friend_7_same_clubs_activity_4,
                             q_net_friend_7_same_clubs_activity_5,
                             q_net_friend_7_same_clubs_activity_6,
                             q_net_friend_7_same_clubs_activity_7,
                             q_net_friend_7_same_clubs_activity_8,
                             q_net_friend_7_same_clubs_activity_9,
                             q_net_friend_7_same_clubs_activity_10,
                             q_net_friend_7_same_clubs_activity_11,
                             q_net_friend_7_same_clubs_activity_12,
                             q_net_friend_7_same_clubs_activity_13,
                             q_net_friend_7_same_clubs_activity_14,
                             q_net_friend_7_same_clubs_activity_15,
                             q_net_friend_7_same_clubs_activity_16,
                             q_net_friend_7_same_clubs_activity_17,
                             q_net_friend_7_same_clubs_activity_18,
                             q_net_friend_7_same_clubs_activity_19,
                             q_net_friend_7_same_clubs_activity_20,
                             q_net_friend_7_same_clubs_activity_21,
                             q_net_friend_7_same_clubs_activity_22,
                             q_net_friend_7_same_clubs_activity_23,
                             q_net_friend_7_same_clubs_activity_24,
                             q_net_friend_7_same_clubs_activity_25,
                             q_net_friend_7_same_clubs_activity_26,
                             q_net_friend_7_same_clubs_activity_27,
                             q_net_friend_7_same_clubs_activity_28,
                             q_net_friend_7_same_clubs_activity_29,
                             q_net_friend_7_same_clubs_activity_30,
                             q_net_friend_7_same_clubs_activity_31,
                             q_net_friend_7_same_clubs_activity_32,
                             q_net_friend_7_same_clubs_activity_33,
                             q_net_friend_7_same_clubs_activity_34,
                             q_net_friend_7_same_clubs_activity_35,
                             q_net_friend_7_same_clubs_activity_36,
                             q_net_friend_7_same_clubs_activity_37,
                             q_net_friend_7_same_clubs_activity_38,
                             q_net_friend_7_same_clubs_activity_39,
                             q_net_friend_7_same_clubs_activity_40,
                             q_net_friend_7_same_clubs_activity_41,
                             q_net_friend_7_same_clubs_activity_42,
                             q_net_friend_7_same_clubs_activity_43,
                             q_net_friend_7_same_clubs_activity_44,
                             q_net_friend_7_same_clubs_other_1,
                             q_net_friend_7_same_clubs_other_2,
                             q_net_friend_7_same_clubs_other_3,
                             q_net_friend_7_same_clubs_other_4))#,
                             #q_net_friend_7_same_clubs_other_text))

fr7[!is.na(fr7)] <- 1
fr7[is.na(fr7)] <- 0
fr7 <- as.data.frame(apply(fr7, 2, function(x) as.numeric(x)))
fr7 <- transform(fr7, q_net_friend_7_same_clubs_sumAll = rowSums(fr7, na.rm = TRUE))

# binarize it to 0s and NAs
fr7$q_net_friend_7_same_clubs_sumAll[fr7$q_net_friend_7_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_7_same_clubs_sumAll <- fr7$q_net_friend_7_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_7_same_clubs_activity_1,
                             q_net_friend_7_same_clubs_activity_2,
                             q_net_friend_7_same_clubs_activity_3,
                             q_net_friend_7_same_clubs_activity_4,
                             q_net_friend_7_same_clubs_activity_5,
                             q_net_friend_7_same_clubs_activity_6,
                             q_net_friend_7_same_clubs_activity_7,
                             q_net_friend_7_same_clubs_activity_8,
                             q_net_friend_7_same_clubs_activity_9,
                             q_net_friend_7_same_clubs_activity_10,
                             q_net_friend_7_same_clubs_activity_11,
                             q_net_friend_7_same_clubs_activity_12,
                             q_net_friend_7_same_clubs_activity_13,
                             q_net_friend_7_same_clubs_activity_14,
                             q_net_friend_7_same_clubs_activity_15,
                             q_net_friend_7_same_clubs_activity_16,
                             q_net_friend_7_same_clubs_activity_17,
                             q_net_friend_7_same_clubs_activity_18,
                             q_net_friend_7_same_clubs_activity_19,
                             q_net_friend_7_same_clubs_activity_20,
                             q_net_friend_7_same_clubs_activity_21,
                             q_net_friend_7_same_clubs_activity_22,
                             q_net_friend_7_same_clubs_activity_23,
                             q_net_friend_7_same_clubs_activity_24,
                             q_net_friend_7_same_clubs_activity_25,
                             q_net_friend_7_same_clubs_activity_26,
                             q_net_friend_7_same_clubs_activity_27,
                             q_net_friend_7_same_clubs_activity_28,
                             q_net_friend_7_same_clubs_activity_29,
                             q_net_friend_7_same_clubs_activity_30,
                             q_net_friend_7_same_clubs_activity_31,
                             q_net_friend_7_same_clubs_activity_32,
                             q_net_friend_7_same_clubs_activity_33,
                             q_net_friend_7_same_clubs_activity_34,
                             q_net_friend_7_same_clubs_activity_35,
                             q_net_friend_7_same_clubs_activity_36,
                             q_net_friend_7_same_clubs_activity_37,
                             q_net_friend_7_same_clubs_activity_38,
                             q_net_friend_7_same_clubs_activity_39,
                             q_net_friend_7_same_clubs_activity_40,
                             q_net_friend_7_same_clubs_activity_41,
                             q_net_friend_7_same_clubs_activity_42,
                             q_net_friend_7_same_clubs_activity_43,
                             q_net_friend_7_same_clubs_activity_44,
                             q_net_friend_7_same_clubs_other_1,
                             q_net_friend_7_same_clubs_other_2,
                             q_net_friend_7_same_clubs_other_3,
                             q_net_friend_7_same_clubs_other_4,
                             q_net_friend_7_same_clubs_other_text))

fr8 <- subset(df, select = c(q_net_friend_8_same_clubs_activity_1,
                             q_net_friend_8_same_clubs_activity_2,
                             q_net_friend_8_same_clubs_activity_3,
                             q_net_friend_8_same_clubs_activity_4,
                             q_net_friend_8_same_clubs_activity_5,
                             q_net_friend_8_same_clubs_activity_6,
                             q_net_friend_8_same_clubs_activity_7,
                             q_net_friend_8_same_clubs_activity_8,
                             q_net_friend_8_same_clubs_activity_9,
                             q_net_friend_8_same_clubs_activity_10,
                             q_net_friend_8_same_clubs_activity_11,
                             q_net_friend_8_same_clubs_activity_12,
                             q_net_friend_8_same_clubs_activity_13,
                             q_net_friend_8_same_clubs_activity_14,
                             q_net_friend_8_same_clubs_activity_15,
                             q_net_friend_8_same_clubs_activity_16,
                             q_net_friend_8_same_clubs_activity_17,
                             q_net_friend_8_same_clubs_activity_18,
                             q_net_friend_8_same_clubs_activity_19,
                             q_net_friend_8_same_clubs_activity_20,
                             q_net_friend_8_same_clubs_activity_21,
                             q_net_friend_8_same_clubs_activity_22,
                             q_net_friend_8_same_clubs_activity_23,
                             q_net_friend_8_same_clubs_activity_24,
                             q_net_friend_8_same_clubs_activity_25,
                             q_net_friend_8_same_clubs_activity_26,
                             q_net_friend_8_same_clubs_activity_27,
                             q_net_friend_8_same_clubs_activity_28,
                             q_net_friend_8_same_clubs_activity_29,
                             q_net_friend_8_same_clubs_activity_30,
                             q_net_friend_8_same_clubs_activity_31,
                             q_net_friend_8_same_clubs_activity_32,
                             q_net_friend_8_same_clubs_activity_33,
                             q_net_friend_8_same_clubs_activity_34,
                             q_net_friend_8_same_clubs_activity_35,
                             q_net_friend_8_same_clubs_activity_36,
                             q_net_friend_8_same_clubs_activity_37,
                             q_net_friend_8_same_clubs_activity_38,
                             q_net_friend_8_same_clubs_activity_39,
                             q_net_friend_8_same_clubs_activity_40,
                             q_net_friend_8_same_clubs_activity_41,
                             q_net_friend_8_same_clubs_activity_42,
                             q_net_friend_8_same_clubs_activity_43,
                             q_net_friend_8_same_clubs_activity_44,
                             q_net_friend_8_same_clubs_other_1,
                             q_net_friend_8_same_clubs_other_2,
                             q_net_friend_8_same_clubs_other_3,
                             q_net_friend_8_same_clubs_other_4))#,
                             ##q_net_friend_8_same_clubs_other_text))

fr8[!is.na(fr8)] <- 1
fr8[is.na(fr8)] <- 0
fr8 <- as.data.frame(apply(fr8, 2, function(x) as.numeric(x)))
fr8 <- transform(fr8, q_net_friend_8_same_clubs_sumAll = rowSums(fr8, na.rm = TRUE))

# binarize it to 0s and NAs
fr8$q_net_friend_8_same_clubs_sumAll[fr8$q_net_friend_8_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_8_same_clubs_sumAll <- fr8$q_net_friend_8_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_8_same_clubs_activity_1,
                             q_net_friend_8_same_clubs_activity_2,
                             q_net_friend_8_same_clubs_activity_3,
                             q_net_friend_8_same_clubs_activity_4,
                             q_net_friend_8_same_clubs_activity_5,
                             q_net_friend_8_same_clubs_activity_6,
                             q_net_friend_8_same_clubs_activity_7,
                             q_net_friend_8_same_clubs_activity_8,
                             q_net_friend_8_same_clubs_activity_9,
                             q_net_friend_8_same_clubs_activity_10,
                             q_net_friend_8_same_clubs_activity_11,
                             q_net_friend_8_same_clubs_activity_12,
                             q_net_friend_8_same_clubs_activity_13,
                             q_net_friend_8_same_clubs_activity_14,
                             q_net_friend_8_same_clubs_activity_15,
                             q_net_friend_8_same_clubs_activity_16,
                             q_net_friend_8_same_clubs_activity_17,
                             q_net_friend_8_same_clubs_activity_18,
                             q_net_friend_8_same_clubs_activity_19,
                             q_net_friend_8_same_clubs_activity_20,
                             q_net_friend_8_same_clubs_activity_21,
                             q_net_friend_8_same_clubs_activity_22,
                             q_net_friend_8_same_clubs_activity_23,
                             q_net_friend_8_same_clubs_activity_24,
                             q_net_friend_8_same_clubs_activity_25,
                             q_net_friend_8_same_clubs_activity_26,
                             q_net_friend_8_same_clubs_activity_27,
                             q_net_friend_8_same_clubs_activity_28,
                             q_net_friend_8_same_clubs_activity_29,
                             q_net_friend_8_same_clubs_activity_30,
                             q_net_friend_8_same_clubs_activity_31,
                             q_net_friend_8_same_clubs_activity_32,
                             q_net_friend_8_same_clubs_activity_33,
                             q_net_friend_8_same_clubs_activity_34,
                             q_net_friend_8_same_clubs_activity_35,
                             q_net_friend_8_same_clubs_activity_36,
                             q_net_friend_8_same_clubs_activity_37,
                             q_net_friend_8_same_clubs_activity_38,
                             q_net_friend_8_same_clubs_activity_39,
                             q_net_friend_8_same_clubs_activity_40,
                             q_net_friend_8_same_clubs_activity_41,
                             q_net_friend_8_same_clubs_activity_42,
                             q_net_friend_8_same_clubs_activity_43,
                             q_net_friend_8_same_clubs_activity_44,
                             q_net_friend_8_same_clubs_other_1,
                             q_net_friend_8_same_clubs_other_2,
                             q_net_friend_8_same_clubs_other_3,
                             q_net_friend_8_same_clubs_other_4,
                             q_net_friend_8_same_clubs_other_text))


fr9 <- subset(df, select = c(q_net_friend_9_same_clubs_activity_1,
                             q_net_friend_9_same_clubs_activity_2,
                             q_net_friend_9_same_clubs_activity_3,
                             q_net_friend_9_same_clubs_activity_4,
                             q_net_friend_9_same_clubs_activity_5,
                             q_net_friend_9_same_clubs_activity_6,
                             q_net_friend_9_same_clubs_activity_7,
                             q_net_friend_9_same_clubs_activity_8,
                             q_net_friend_9_same_clubs_activity_9,
                             q_net_friend_9_same_clubs_activity_10,
                             q_net_friend_9_same_clubs_activity_11,
                             q_net_friend_9_same_clubs_activity_12,
                             q_net_friend_9_same_clubs_activity_13,
                             q_net_friend_9_same_clubs_activity_14,
                             q_net_friend_9_same_clubs_activity_15,
                             q_net_friend_9_same_clubs_activity_16,
                             q_net_friend_9_same_clubs_activity_17,
                             q_net_friend_9_same_clubs_activity_18,
                             q_net_friend_9_same_clubs_activity_19,
                             q_net_friend_9_same_clubs_activity_20,
                             q_net_friend_9_same_clubs_activity_21,
                             q_net_friend_9_same_clubs_activity_22,
                             q_net_friend_9_same_clubs_activity_23,
                             q_net_friend_9_same_clubs_activity_24,
                             q_net_friend_9_same_clubs_activity_25,
                             q_net_friend_9_same_clubs_activity_26,
                             q_net_friend_9_same_clubs_activity_27,
                             q_net_friend_9_same_clubs_activity_28,
                             q_net_friend_9_same_clubs_activity_29,
                             q_net_friend_9_same_clubs_activity_30,
                             q_net_friend_9_same_clubs_activity_31,
                             q_net_friend_9_same_clubs_activity_32,
                             q_net_friend_9_same_clubs_activity_33,
                             q_net_friend_9_same_clubs_activity_34,
                             q_net_friend_9_same_clubs_activity_35,
                             q_net_friend_9_same_clubs_activity_36,
                             q_net_friend_9_same_clubs_activity_37,
                             q_net_friend_9_same_clubs_activity_38,
                             q_net_friend_9_same_clubs_activity_39,
                             q_net_friend_9_same_clubs_activity_40,
                             q_net_friend_9_same_clubs_activity_41,
                             q_net_friend_9_same_clubs_activity_42,
                             q_net_friend_9_same_clubs_activity_43,
                             q_net_friend_9_same_clubs_activity_44,
                             q_net_friend_9_same_clubs_other_1,
                             q_net_friend_9_same_clubs_other_2,
                             q_net_friend_9_same_clubs_other_3,
                             q_net_friend_9_same_clubs_other_4))#,
                             #q_net_friend_9_same_clubs_other_text))

fr9[!is.na(fr9)] <- 1
fr9[is.na(fr9)] <- 0
fr9 <- as.data.frame(apply(fr9, 2, function(x) as.numeric(x)))
fr9 <- transform(fr9, q_net_friend_9_same_clubs_sumAll = rowSums(fr9, na.rm = TRUE))

# binarize it to 0s and NAs
fr9$q_net_friend_9_same_clubs_sumAll[fr9$q_net_friend_9_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_9_same_clubs_sumAll <- fr9$q_net_friend_9_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_9_same_clubs_activity_1,
                             q_net_friend_9_same_clubs_activity_2,
                             q_net_friend_9_same_clubs_activity_3,
                             q_net_friend_9_same_clubs_activity_4,
                             q_net_friend_9_same_clubs_activity_5,
                             q_net_friend_9_same_clubs_activity_6,
                             q_net_friend_9_same_clubs_activity_7,
                             q_net_friend_9_same_clubs_activity_8,
                             q_net_friend_9_same_clubs_activity_9,
                             q_net_friend_9_same_clubs_activity_10,
                             q_net_friend_9_same_clubs_activity_11,
                             q_net_friend_9_same_clubs_activity_12,
                             q_net_friend_9_same_clubs_activity_13,
                             q_net_friend_9_same_clubs_activity_14,
                             q_net_friend_9_same_clubs_activity_15,
                             q_net_friend_9_same_clubs_activity_16,
                             q_net_friend_9_same_clubs_activity_17,
                             q_net_friend_9_same_clubs_activity_18,
                             q_net_friend_9_same_clubs_activity_19,
                             q_net_friend_9_same_clubs_activity_20,
                             q_net_friend_9_same_clubs_activity_21,
                             q_net_friend_9_same_clubs_activity_22,
                             q_net_friend_9_same_clubs_activity_23,
                             q_net_friend_9_same_clubs_activity_24,
                             q_net_friend_9_same_clubs_activity_25,
                             q_net_friend_9_same_clubs_activity_26,
                             q_net_friend_9_same_clubs_activity_27,
                             q_net_friend_9_same_clubs_activity_28,
                             q_net_friend_9_same_clubs_activity_29,
                             q_net_friend_9_same_clubs_activity_30,
                             q_net_friend_9_same_clubs_activity_31,
                             q_net_friend_9_same_clubs_activity_32,
                             q_net_friend_9_same_clubs_activity_33,
                             q_net_friend_9_same_clubs_activity_34,
                             q_net_friend_9_same_clubs_activity_35,
                             q_net_friend_9_same_clubs_activity_36,
                             q_net_friend_9_same_clubs_activity_37,
                             q_net_friend_9_same_clubs_activity_38,
                             q_net_friend_9_same_clubs_activity_39,
                             q_net_friend_9_same_clubs_activity_40,
                             q_net_friend_9_same_clubs_activity_41,
                             q_net_friend_9_same_clubs_activity_42,
                             q_net_friend_9_same_clubs_activity_43,
                             q_net_friend_9_same_clubs_activity_44,
                             q_net_friend_9_same_clubs_other_1,
                             q_net_friend_9_same_clubs_other_2,
                             q_net_friend_9_same_clubs_other_3,
                             q_net_friend_9_same_clubs_other_4,
                             q_net_friend_9_same_clubs_other_text))


fr10 <- subset(df, select = c(q_net_friend_10_same_clubs_activity_1,
                              q_net_friend_10_same_clubs_activity_2,
                              q_net_friend_10_same_clubs_activity_3,
                              q_net_friend_10_same_clubs_activity_4,
                              q_net_friend_10_same_clubs_activity_5,
                              q_net_friend_10_same_clubs_activity_6,
                              q_net_friend_10_same_clubs_activity_7,
                              q_net_friend_10_same_clubs_activity_8,
                              q_net_friend_10_same_clubs_activity_9,
                              q_net_friend_10_same_clubs_activity_10,
                              q_net_friend_10_same_clubs_activity_11,
                              q_net_friend_10_same_clubs_activity_12,
                              q_net_friend_10_same_clubs_activity_13,
                              q_net_friend_10_same_clubs_activity_14,
                              q_net_friend_10_same_clubs_activity_15,
                              q_net_friend_10_same_clubs_activity_16,
                              q_net_friend_10_same_clubs_activity_17,
                              q_net_friend_10_same_clubs_activity_18,
                              q_net_friend_10_same_clubs_activity_19,
                              q_net_friend_10_same_clubs_activity_20,
                              q_net_friend_10_same_clubs_activity_21,
                              q_net_friend_10_same_clubs_activity_22,
                              q_net_friend_10_same_clubs_activity_23,
                              q_net_friend_10_same_clubs_activity_24,
                              q_net_friend_10_same_clubs_activity_25,
                              q_net_friend_10_same_clubs_activity_26,
                              q_net_friend_10_same_clubs_activity_27,
                              q_net_friend_10_same_clubs_activity_28,
                              q_net_friend_10_same_clubs_activity_29,
                              q_net_friend_10_same_clubs_activity_30,
                              q_net_friend_10_same_clubs_activity_31,
                              q_net_friend_10_same_clubs_activity_32,
                              q_net_friend_10_same_clubs_activity_33,
                              q_net_friend_10_same_clubs_activity_34,
                              q_net_friend_10_same_clubs_activity_35,
                              q_net_friend_10_same_clubs_activity_36,
                              q_net_friend_10_same_clubs_activity_37,
                              q_net_friend_10_same_clubs_activity_38,
                              q_net_friend_10_same_clubs_activity_39,
                              q_net_friend_10_same_clubs_activity_40,
                              q_net_friend_10_same_clubs_activity_41,
                              q_net_friend_10_same_clubs_activity_42,
                              q_net_friend_10_same_clubs_activity_43,
                              q_net_friend_10_same_clubs_activity_44,
                              q_net_friend_10_same_clubs_other_1,
                              q_net_friend_10_same_clubs_other_2,
                              q_net_friend_10_same_clubs_other_3,
                              q_net_friend_10_same_clubs_other_4))#,
                              #q_net_friend_10_same_clubs_other_text))

fr10[!is.na(fr10)] <- 1
fr10[is.na(fr10)] <- 0
fr10 <- as.data.frame(apply(fr10, 2, function(x) as.numeric(x)))
fr10 <- transform(fr10, q_net_friend_10_same_clubs_sumAll = rowSums(fr10, na.rm = TRUE))

# binarize it to 0s and NAs
fr10$q_net_friend_10_same_clubs_sumAll[fr10$q_net_friend_10_same_clubs_sumAll == 0] <- NA

# add the variable to df dataset
df$q_net_friend_10_same_clubs_sumAll <- fr10$q_net_friend_10_same_clubs_sumAll

# and now exclude the variables on which the new one is based

df <- subset(df, select = -c(q_net_friend_10_same_clubs_activity_1,
                             q_net_friend_10_same_clubs_activity_2,
                             q_net_friend_10_same_clubs_activity_3,
                             q_net_friend_10_same_clubs_activity_4,
                             q_net_friend_10_same_clubs_activity_5,
                             q_net_friend_10_same_clubs_activity_6,
                             q_net_friend_10_same_clubs_activity_7,
                             q_net_friend_10_same_clubs_activity_8,
                             q_net_friend_10_same_clubs_activity_9,
                             q_net_friend_10_same_clubs_activity_10,
                             q_net_friend_10_same_clubs_activity_11,
                             q_net_friend_10_same_clubs_activity_12,
                             q_net_friend_10_same_clubs_activity_13,
                             q_net_friend_10_same_clubs_activity_14,
                             q_net_friend_10_same_clubs_activity_15,
                             q_net_friend_10_same_clubs_activity_16,
                             q_net_friend_10_same_clubs_activity_17,
                             q_net_friend_10_same_clubs_activity_18,
                             q_net_friend_10_same_clubs_activity_19,
                             q_net_friend_10_same_clubs_activity_20,
                             q_net_friend_10_same_clubs_activity_21,
                             q_net_friend_10_same_clubs_activity_22,
                             q_net_friend_10_same_clubs_activity_23,
                             q_net_friend_10_same_clubs_activity_24,
                             q_net_friend_10_same_clubs_activity_25,
                             q_net_friend_10_same_clubs_activity_26,
                             q_net_friend_10_same_clubs_activity_27,
                             q_net_friend_10_same_clubs_activity_28,
                             q_net_friend_10_same_clubs_activity_29,
                             q_net_friend_10_same_clubs_activity_30,
                             q_net_friend_10_same_clubs_activity_31,
                             q_net_friend_10_same_clubs_activity_32,
                             q_net_friend_10_same_clubs_activity_33,
                             q_net_friend_10_same_clubs_activity_34,
                             q_net_friend_10_same_clubs_activity_35,
                             q_net_friend_10_same_clubs_activity_36,
                             q_net_friend_10_same_clubs_activity_37,
                             q_net_friend_10_same_clubs_activity_38,
                             q_net_friend_10_same_clubs_activity_39,
                             q_net_friend_10_same_clubs_activity_40,
                             q_net_friend_10_same_clubs_activity_41,
                             q_net_friend_10_same_clubs_activity_42,
                             q_net_friend_10_same_clubs_activity_43,
                             q_net_friend_10_same_clubs_activity_44,
                             q_net_friend_10_same_clubs_other_1,
                             q_net_friend_10_same_clubs_other_2,
                             q_net_friend_10_same_clubs_other_3,
                             q_net_friend_10_same_clubs_other_4,
                             q_net_friend_10_same_clubs_other_text))

# 460 variables less due to this last cutting
dim(df) # 575 - 597

# GANGS
colnames(df[,548:560])
df$q_gangs_text[df$q_gangs == 2] <- 0 # means they don't have a clique

df$q_gangs_sporty[df$q_gangs == 2] <- 0
df$q_gangs_popular[df$q_gangs == 2] <- 0
df$q_gangs_powerfull[df$q_gangs == 2] <- 0
df$q_gangs_doing_well[df$q_gangs == 2] <- 0
df$q_gangs_stylish[df$q_gangs == 2] <- 0
df$q_gangs_laugh[df$q_gangs == 2] <- 0
df$q_gangs_trouble[df$q_gangs == 2] <- 0
df$q_gangs_ordinary[df$q_gangs == 2] <- 0
df$q_gangs_respected[df$q_gangs == 2] <- 0
df$q_gangs_person_1_hidden_id[df$q_gangs == 2] <- 0
df$q_gangs_person_2_hidden_id[df$q_gangs == 2] <- 0

df$q_other_gangs_text[df$q_other_gangs == 2] <- 0 
df$q_other_gangs_sporty[df$q_other_gangs == 2] <- 0
df$q_other_gangs_popular[df$q_other_gangs == 2] <- 0
df$q_other_gangs_powerfull[df$q_other_gangs == 2] <- 0
df$q_other_gangs_doing_well[df$q_other_gangs == 2] <- 0
df$q_other_gangs_stylish[df$q_other_gangs == 2] <- 0
df$q_other_gangs_laugh[df$q_other_gangs == 2] <- 0
df$q_other_gangs_trouble[df$q_other_gangs == 2] <- 0
df$q_other_gangs_ordinary[df$q_other_gangs == 2] <- 0
df$q_other_gangs_respected[df$q_other_gangs == 2] <- 0
df$q_other_gangs_person_1_hidden_id[df$q_other_gangs == 2] <- 0
df$q_other_gangs_person_2_hidden_id[df$q_other_gangs == 2] <- 0

# stop here and save 07/07/2021 ------ 29/06

write.csv(df, file = "File_conf.csv", #dec = ',' ,sep = ";",
          row.names = FALSE) 



