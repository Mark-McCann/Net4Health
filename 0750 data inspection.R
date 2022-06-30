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

# Explore different sets of variables

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

# OTHER NETWORK CONDITIONAL QUESTIONS
#sum(is.na(df$q_net_friend_1_hidden_id))
#sum(is.na(df$q_net_friend_1_school_year))

### 999 we have the data, but not in this column since they were not asked 

#library(stringr)

List1 <- list()
for (i in 1:10) {
  name0 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_hidden_id", sep = "")))
  name1 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_school_year", sep = "")))
  name2 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_gender", sep = "")))
  name3 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_older_younger", sep = "")))
  
  name1[is.na(name1) & !is.na(name0)] <- 999
  name2[is.na(name2) & !is.na(name0)] <- 999
  name3[is.na(name3) & !is.na(name0)] <- 999
  
  li <- cbind(name1, name2, name3)
  List1[[length(List1) + 1]] = li
  
  print(table(name2))
}

s <- matrix(unlist(List1), ncol = 30, byrow = FALSE)
dfs <- as.data.frame(s)

head(dfs)
# new dataset - it will add ".1" later
colnames(dfs) <- c("q_net_friend_1_school_year",
                   "q_net_friend_1_gender",
                   "q_net_friend_1_older_younger",
                   "q_net_friend_2_school_year",
                   "q_net_friend_2_gender",
                   "q_net_friend_2_older_younger",
                   "q_net_friend_3_school_year",
                   "q_net_friend_3_gender",
                   "q_net_friend_3_older_younger",
                   "q_net_friend_4_school_year",
                   "q_net_friend_4_gender",
                   "q_net_friend_4_older_younger",
                   "q_net_friend_5_school_year",
                   "q_net_friend_5_gender",
                   "q_net_friend_5_older_younger",
                   "q_net_friend_6_school_year",
                   "q_net_friend_6_gender",
                   "q_net_friend_6_older_younger",
                   "q_net_friend_7_school_year",
                   "q_net_friend_7_gender",
                   "q_net_friend_7_older_younger",
                   "q_net_friend_8_school_year",
                   "q_net_friend_8_gender",
                   "q_net_friend_8_older_younger",
                   "q_net_friend_9_school_year",
                   "q_net_friend_9_gender",
                   "q_net_friend_9_older_younger",
                   "q_net_friend_10_school_year",
                   "q_net_friend_10_gender",
                   "q_net_friend_10_older_younger")

#999 means info not necessary  ---> does not add up completely ? maybe it does
df$q_net_friend_emotional_support_inward_1_older_younger[is.na(df$q_net_friend_emotional_support_inward_1_older_younger) &
                                                           !is.na(df$q_net_friend_emotional_support_outward_1_hidden_id)] <- 999
df$q_net_friend_emotional_support_inward_2_older_younger[is.na(df$q_net_friend_emotional_support_inward_2_older_younger) &
                                                           !is.na(df$q_net_friend_emotional_support_outward_2_hidden_id)] <- 999
df$q_net_friend_emotional_support_inward_3_older_younger[is.na(df$q_net_friend_emotional_support_inward_3_older_younger) &
                                                           !is.na(df$q_net_friend_emotional_support_outward_3_hidden_id)] <- 999
# same clubs 1 - yes; 2 - no ???
#df$q_net_friend_1_same_clubs_yes_no
#df$q_net_friend_1_same_clubs_sumAll

# 0 means in zero clubs together  ---> does not add up completely

List2 = list()
for (i in 1:10) {
  name1 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_same_clubs_sumAll", sep = "")))
  name2 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_same_clubs_yes_no", sep = "")))
  
  name1[name2 == 2] <- 0
  
  li <- c(name2, name1)
  List2[[length(List2) + 1]] = li
  
  print(table(name1))
  #print(table(name3))
}
s <- matrix(unlist(List2), ncol = 20, byrow = FALSE)
dfs1 <- as.data.frame(s)

# new dataset
colnames(dfs1) <- c("q_net_friend_1_same_clubs_yes_no",
                    "q_net_friend_1_same_clubs_sumAll",
                    "q_net_friend_2_same_clubs_yes_no",
                    "q_net_friend_2_same_clubs_sumAll",
                    "q_net_friend_3_same_clubs_yes_no",
                    "q_net_friend_3_same_clubs_sumAll",
                    "q_net_friend_4_same_clubs_yes_no",
                    "q_net_friend_4_same_clubs_sumAll",
                    "q_net_friend_5_same_clubs_yes_no",
                    "q_net_friend_5_same_clubs_sumAll",
                    "q_net_friend_6_same_clubs_yes_no",
                    "q_net_friend_6_same_clubs_sumAll",
                    "q_net_friend_7_same_clubs_yes_no",
                    "q_net_friend_7_same_clubs_sumAll",
                    "q_net_friend_8_same_clubs_yes_no",
                    "q_net_friend_8_same_clubs_sumAll",
                    "q_net_friend_9_same_clubs_yes_no",
                    "q_net_friend_9_same_clubs_sumAll",
                    "q_net_friend_10_same_clubs_yes_no",
                    "q_net_friend_10_same_clubs_sumAll")



# emot. support outward --> 0 - never talked about it
# not adding up completely, but ok
List3 <- list()
for (i in 1:3) {
  name1 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_feeling", sep = "")))
  name2 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_support", sep = "")))
  name3 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_emotions", sep = "")))
  
  name4 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_makes_jokes", sep = "")))
  
  name5 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_changes_subject", sep = "")))
  
  name6 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_text_1", sep = "")))
  
  name7 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_1", sep = "")))
  
  name8 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_text_2", sep = "")))
  
  name9 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_2", sep = "")))
  
  name10 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_text_3", sep = "")))
  
  name11 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_mood_3", sep = "")))
  
  name2[name1 == 1] <- 0
  name3[name1 == 1] <- 0
  name4[name1 == 1] <- 0
  name5[name1 == 1] <- 0
  name6[name1 == 1] <- 0
  name7[name1 == 1] <- 0
  name8[name1 == 1] <- 0
  name9[name1 == 1] <- 0
  name10[name1 == 1] <- 0
  name11[name1 == 1] <- 0
  
  print(table(name2))
  print(table(name3))
  print(table(name10))
  print(table(name11))
  
  li <- cbind(name1, name2, name3, name4, name5, name6, name7, name8, name9, 
              name10, name11)
  List3[[length(List3) + 1]] = li
  #print(table(name3))
}
s <- matrix(unlist(List3), ncol = 33, byrow = FALSE)
dfs2 <- as.data.frame(s)

# new dataset
colnames(dfs2) <- c("q_net_friend_1_feeling",
                    "q_net_friend_1_support",
                    "q_net_friend_1_emotions",
                    "q_net_friend_1_makes_jokes",
                    "q_net_friend_1_changes_subject",
                    "q_net_friend_1_mood_text_1",
                    "q_net_friend_1_mood_1",
                    "q_net_friend_1_mood_text_2",
                    "q_net_friend_1_mood_2",
                    "q_net_friend_1_mood_text_3",
                    "q_net_friend_1_mood_3",
                    "q_net_friend_2_feeling",
                    "q_net_friend_2_support",
                    "q_net_friend_2_emotions",
                    "q_net_friend_2_makes_jokes",
                    "q_net_friend_2_changes_subject",
                    "q_net_friend_2_mood_text_1",
                    "q_net_friend_2_mood_1",
                    "q_net_friend_2_mood_text_2",
                    "q_net_friend_2_mood_2",
                    "q_net_friend_2_mood_text_3",
                    "q_net_friend_2_mood_3",
                    "q_net_friend_3_feeling",
                    "q_net_friend_3_support",
                    "q_net_friend_3_emotions",
                    "q_net_friend_3_makes_jokes",
                    "q_net_friend_3_changes_subject",
                    "q_net_friend_3_mood_text_1",
                    "q_net_friend_3_mood_1",
                    "q_net_friend_3_mood_text_2",
                    "q_net_friend_3_mood_2",
                    "q_net_friend_3_mood_text_3",
                    "q_net_friend_3_mood_3")

# emo. support inward - --> 0 - never talked about it

#df$q_net_friend_1_you_respond_feeling

List4 <- list()
for (i in 1:3) {
  name1 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_feeling", sep = "")))
  name2 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_support", sep = "")))
  name3 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_emotions", sep = "")))
  name4 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_makes_jokes", sep = "")))
  
  name5 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_changes_subject", sep = "")))
  
  name6 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_text_1", sep = "")))
  
  name7 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_1", sep = "")))
  
  name8 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_text_2", sep = "")))
  
  name9 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_2", sep = "")))
  
  name10 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_text_3", sep = "")))
  
  name11 <- eval(parse(text = paste("df$q_net_friend_",toString(i),"_you_respond_mood_3", sep = "")))
  
  name2[name1 == 1] <- 0
  name3[name1 == 1] <- 0
  name4[name1 == 1] <- 0
  name5[name1 == 1] <- 0
  name6[name1 == 1] <- 0
  name7[name1 == 1] <- 0
  name8[name1 == 1] <- 0
  name9[name1 == 1] <- 0
  name10[name1 == 1] <- 0
  name11[name1 == 1] <- 0
  print(table(name2))
  print(table(name3))
  print(table(name4))
  print(table(name7))
  print(table(name8))
  print(table(name10))
  print(table(name11))
  #print(table(name3))
  li <- cbind(name1, name2, name3, name4, name5, name6, name7, name8, name9, 
              name10, name11)
  List4[[length(List4) + 1]] = li
  #print(table(name3))
}
s <- matrix(unlist(List4), ncol = 33, byrow = FALSE)
dfs3 <- as.data.frame(s)

# new dataset
colnames(dfs3) <- c("q_net_friend_1_you_respond_feeling",
                    "q_net_friend_1_you_respond_support",
                    "q_net_friend_1_you_respond_emotions",
                    "q_net_friend_1_you_respond_makes_jokes",
                    "q_net_friend_1_you_respond_changes_subject",
                    "q_net_friend_1_you_respond_mood_text_1",
                    "q_net_friend_1_you_respond_mood_1",
                    "q_net_friend_1_you_respond_mood_text_2",
                    "q_net_friend_1_you_respond_mood_2",
                    "q_net_friend_1_you_respond_mood_text_3",
                    "q_net_friend_1_you_respond_mood_3",
                    "q_net_friend_2_you_respond_feeling",
                    "q_net_friend_2_you_respond_support",
                    "q_net_friend_2_you_respond_emotions",
                    "q_net_friend_2_you_respond_makes_jokes",
                    "q_net_friend_2_you_respond_changes_subject",
                    "q_net_friend_2_you_respond_mood_text_1",
                    "q_net_friend_2_you_respond_mood_1",
                    "q_net_friend_2_you_respond_mood_text_2",
                    "q_net_friend_2_you_respond_mood_2",
                    "q_net_friend_2_you_respond_mood_text_3",
                    "q_net_friend_2_you_respond_mood_3",
                    "q_net_friend_3_you_respond_feeling",
                    "q_net_friend_3_you_respond_support",
                    "q_net_friend_3_you_respond_emotions",
                    "q_net_friend_3_you_respond_makes_jokes",
                    "q_net_friend_3_you_respond_changes_subject",
                    "q_net_friend_3_you_respond_mood_text_1",
                    "q_net_friend_3_you_respond_mood_1",
                    "q_net_friend_3_you_respond_mood_text_2",
                    "q_net_friend_3_you_respond_mood_2",
                    "q_net_friend_3_you_respond_mood_text_3",
                    "q_net_friend_3_you_respond_mood_3")

newdataset <- as.data.frame(cbind(dfs, dfs1, dfs2, dfs3))
dim(newdataset) # 116 corrected variables
dim(dfs3)

dim(df) # 597

# i should not exclude bullying and some other variables before saving it first
# the exclude and save again

write.csv(df, file = "Dataset trimmed for analysis1_w_bully_270221.csv")
write.table(df, file = "Dataset trimmed for analysis1_1_w_bully_270221.csv", dec = ',' ,
            sep = ";",row.names = FALSE)

head(newdataset)
dim(df)# 597
# add 116 corrected variables to df
newdf <- as.data.frame(cbind(df, newdataset))
dim(newdf) # 713
# exc bully
newdf = subset(newdf , select = -c(q_50_a,
                                   q_50_b,
                                   q_50_c,
                                   q_50_d,
                                   q_50_e,
                                   q_50_f,
                                   q_50_g,
                                   q_50_h,
                                   q_50_i,
                                   q_50_j,
                                   q_51_a,
                                   q_51_b,
                                   q_51_c,
                                   q_51_d,
                                   q_51_e,
                                   q_51_f,
                                   q_51_g,
                                   q_51_h,
                                   q_51_i,
                                   q_51_j,
                                   q_51_k,
                                   q_51_l))

dim(newdf)#[1]  302 691


# save - this is for descriptive NA
write.csv(newdf, file = "Dataset trimmed_with_corr_var_no_bully_270221.csv")
write.table(newdf, file = "Dataset trimmed_with_corr_var_no_bully_270221_1.csv", dec = ',' ,
            sep = ";",row.names = FALSE) # this is to be able to inspect file later

# NOTE: THIS DATASET HAS NO BULLYING VARIABLES and has corrected variables


# inspect with EDA, add order info, after ordering it in the original order excel

library(SmartEDA)
# Overview of the data - Type = 1

##### delete non-corrected columns by name
newdf <- subset(newdf, select = -c(q_net_friend_1_school_year,
                                   q_net_friend_1_gender,
                                   q_net_friend_1_older_younger,
                                   q_net_friend_2_school_year,
                                   q_net_friend_2_gender,
                                   q_net_friend_2_older_younger,
                                   q_net_friend_3_school_year,
                                   q_net_friend_3_gender,
                                   q_net_friend_3_older_younger,
                                   q_net_friend_4_school_year,
                                   q_net_friend_4_gender,
                                   q_net_friend_4_older_younger,
                                   q_net_friend_5_school_year,
                                   q_net_friend_5_gender,
                                   q_net_friend_5_older_younger,
                                   q_net_friend_6_school_year,
                                   q_net_friend_6_gender,
                                   q_net_friend_6_older_younger,
                                   q_net_friend_7_school_year,
                                   q_net_friend_7_gender,
                                   q_net_friend_7_older_younger,
                                   q_net_friend_8_school_year,
                                   q_net_friend_8_gender,
                                   q_net_friend_8_older_younger,
                                   q_net_friend_9_school_year,
                                   q_net_friend_9_gender,
                                   q_net_friend_9_older_younger,
                                   q_net_friend_10_school_year,
                                   q_net_friend_10_gender,
                                   q_net_friend_10_older_younger,
                                   q_net_friend_1_same_clubs_yes_no,
                                   q_net_friend_1_same_clubs_sumAll,
                                   q_net_friend_2_same_clubs_yes_no,
                                   q_net_friend_2_same_clubs_sumAll,
                                   q_net_friend_3_same_clubs_yes_no,
                                   q_net_friend_3_same_clubs_sumAll,
                                   q_net_friend_4_same_clubs_yes_no,
                                   q_net_friend_4_same_clubs_sumAll,
                                   q_net_friend_5_same_clubs_yes_no,
                                   q_net_friend_5_same_clubs_sumAll,
                                   q_net_friend_6_same_clubs_yes_no,
                                   q_net_friend_6_same_clubs_sumAll,
                                   q_net_friend_7_same_clubs_yes_no,
                                   q_net_friend_7_same_clubs_sumAll,
                                   q_net_friend_8_same_clubs_yes_no,
                                   q_net_friend_8_same_clubs_sumAll,
                                   q_net_friend_9_same_clubs_yes_no,
                                   q_net_friend_9_same_clubs_sumAll,
                                   q_net_friend_10_same_clubs_yes_no,
                                   q_net_friend_10_same_clubs_sumAll,
                                   q_net_friend_1_feeling,
                                   q_net_friend_1_support,
                                   q_net_friend_1_emotions,
                                   q_net_friend_1_makes_jokes,
                                   q_net_friend_1_changes_subject,
                                   q_net_friend_1_mood_text_1,
                                   q_net_friend_1_mood_1,
                                   q_net_friend_1_mood_text_2,
                                   q_net_friend_1_mood_2,
                                   q_net_friend_1_mood_text_3,
                                   q_net_friend_1_mood_3,
                                   q_net_friend_2_feeling,
                                   q_net_friend_2_support,
                                   q_net_friend_2_emotions,
                                   q_net_friend_2_makes_jokes,
                                   q_net_friend_2_changes_subject,
                                   q_net_friend_2_mood_text_1,
                                   q_net_friend_2_mood_1,
                                   q_net_friend_2_mood_text_2,
                                   q_net_friend_2_mood_2,
                                   q_net_friend_2_mood_text_3,
                                   q_net_friend_2_mood_3,
                                   q_net_friend_3_feeling,
                                   q_net_friend_3_support,
                                   q_net_friend_3_emotions,
                                   q_net_friend_3_makes_jokes,
                                   q_net_friend_3_changes_subject,
                                   q_net_friend_3_mood_text_1,
                                   q_net_friend_3_mood_1,
                                   q_net_friend_3_mood_text_2,
                                   q_net_friend_3_mood_2,
                                   q_net_friend_3_mood_text_3,
                                   q_net_friend_3_mood_3,
                                   q_net_friend_1_you_respond_feeling,
                                   q_net_friend_1_you_respond_support,
                                   q_net_friend_1_you_respond_emotions,
                                   q_net_friend_1_you_respond_makes_jokes,
                                   q_net_friend_1_you_respond_changes_subject,
                                   q_net_friend_1_you_respond_mood_text_1,
                                   q_net_friend_1_you_respond_mood_1,
                                   q_net_friend_1_you_respond_mood_text_2,
                                   q_net_friend_1_you_respond_mood_2,
                                   q_net_friend_1_you_respond_mood_text_3,
                                   q_net_friend_1_you_respond_mood_3,
                                   q_net_friend_2_you_respond_feeling,
                                   q_net_friend_2_you_respond_support,
                                   q_net_friend_2_you_respond_emotions,
                                   q_net_friend_2_you_respond_makes_jokes,
                                   q_net_friend_2_you_respond_changes_subject,
                                   q_net_friend_2_you_respond_mood_text_1,
                                   q_net_friend_2_you_respond_mood_1,
                                   q_net_friend_2_you_respond_mood_text_2,
                                   q_net_friend_2_you_respond_mood_2,
                                   q_net_friend_2_you_respond_mood_text_3,
                                   q_net_friend_2_you_respond_mood_3,
                                   q_net_friend_3_you_respond_feeling,
                                   q_net_friend_3_you_respond_support,
                                   q_net_friend_3_you_respond_emotions,
                                   q_net_friend_3_you_respond_makes_jokes,
                                   q_net_friend_3_you_respond_changes_subject,
                                   q_net_friend_3_you_respond_mood_text_1,
                                   q_net_friend_3_you_respond_mood_1,
                                   q_net_friend_3_you_respond_mood_text_2,
                                   q_net_friend_3_you_respond_mood_2,
                                   q_net_friend_3_you_respond_mood_text_3,
                                   q_net_friend_3_you_respond_mood_3))
dim(newdf)# 575


# the fact: some alters exist but are not named with ids 
# if person did not give name - they did not go further questions

newdf$q_net_friend_1_hidden_id[is.na(newdf$q_net_friend_1_hidden_id) &  
                                 newdf$q_net_friend_1_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_1_hidden_id[is.na(newdf$q_net_friend_1_hidden_id) &  
                                 newdf$q_net_friend_1_school_year.1 == 1 ] <- "from the school year"



newdf$q_net_friend_2_hidden_id[is.na(newdf$q_net_friend_2_hidden_id) &  
                                 newdf$q_net_friend_2_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_3_hidden_id[is.na(newdf$q_net_friend_3_hidden_id) &  
                                 newdf$q_net_friend_3_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_3_hidden_id[is.na(newdf$q_net_friend_3_hidden_id) &  
                                 newdf$q_net_friend_3_school_year.1 == 1 ] <- "from the school year"


newdf$q_net_friend_4_hidden_id[is.na(newdf$q_net_friend_4_hidden_id) &  
                                 newdf$q_net_friend_4_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_4_hidden_id[is.na(newdf$q_net_friend_4_hidden_id) &  
                                 newdf$q_net_friend_4_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_5_hidden_id[is.na(newdf$q_net_friend_5_hidden_id) &  
                                 newdf$q_net_friend_5_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_5_hidden_id[is.na(newdf$q_net_friend_5_hidden_id) &  
                                 newdf$q_net_friend_5_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_6_hidden_id[is.na(newdf$q_net_friend_6_hidden_id) &  
                                 newdf$q_net_friend_6_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_6_hidden_id[is.na(newdf$q_net_friend_6_hidden_id) &  
                                 newdf$q_net_friend_6_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_7_hidden_id[is.na(newdf$q_net_friend_7_hidden_id) &  
                                 newdf$q_net_friend_7_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_7_hidden_id[is.na(newdf$q_net_friend_7_hidden_id) &  
                                 newdf$q_net_friend_7_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_8_hidden_id[is.na(newdf$q_net_friend_8_hidden_id) &  
                                 newdf$q_net_friend_8_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_8_hidden_id[is.na(newdf$q_net_friend_8_hidden_id) &  
                                 newdf$q_net_friend_8_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_9_hidden_id[is.na(newdf$q_net_friend_9_hidden_id) &  
                                 newdf$q_net_friend_9_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_9_hidden_id[is.na(newdf$q_net_friend_9_hidden_id) &  
                                 newdf$q_net_friend_9_school_year.1 == 1 ] <- "from the school year"

newdf$q_net_friend_10_hidden_id[is.na(newdf$q_net_friend_10_hidden_id) &  
                                  newdf$q_net_friend_10_school_year.1 == 2 ] <- "not from the school year"

newdf$q_net_friend_10_hidden_id[is.na(newdf$q_net_friend_10_hidden_id) &  
                                  newdf$q_net_friend_10_school_year.1 == 1 ] <- "from the school year"



newdf$q_net_friend_1_hidden_id[is.na(newdf$q_net_friend_1_hidden_id) &  
                                 is.na(newdf$q_net_friend_1_school_year.1) &
                                 !is.na(newdf$q_net_friend_1_how_close)] <- "someone"

newdf$q_net_friend_2_hidden_id[is.na(newdf$q_net_friend_2_hidden_id) &  
                                 is.na(newdf$q_net_friend_2_school_year.1) &
                                 !is.na(newdf$q_net_friend_2_how_close)] <- "someone"

newdf$q_net_friend_3_hidden_id[is.na(newdf$q_net_friend_3_hidden_id) &  
                                 is.na(newdf$q_net_friend_3_school_year.1) &
                                 !is.na(newdf$q_net_friend_3_how_close)] <- "someone"

newdf$q_net_friend_4_hidden_id[is.na(newdf$q_net_friend_4_hidden_id) &  
                                 is.na(newdf$q_net_friend_4_school_year.1) &
                                 !is.na(newdf$q_net_friend_4_how_close)] <- "someone"

newdf$q_net_friend_5_hidden_id[is.na(newdf$q_net_friend_5_hidden_id) &  
                                 is.na(newdf$q_net_friend_5_school_year.1) &
                                 !is.na(newdf$q_net_friend_5_how_close)] <- "someone"

newdf$q_net_friend_6_hidden_id[is.na(newdf$q_net_friend_6_hidden_id) &  
                                 is.na(newdf$q_net_friend_6_school_year.1) &
                                 !is.na(newdf$q_net_friend_6_how_close)] <- "someone"

newdf$q_net_friend_7_hidden_id[is.na(newdf$q_net_friend_7_hidden_id) &  
                                 is.na(newdf$q_net_friend_7_school_year.1) &
                                 !is.na(newdf$q_net_friend_7_how_close)] <- "someone"

newdf$q_net_friend_8_hidden_id[is.na(newdf$q_net_friend_8_hidden_id) &  
                                 is.na(newdf$q_net_friend_8_school_year.1) &
                                 !is.na(newdf$q_net_friend_8_how_close)] <- "someone"

newdf$q_net_friend_9_hidden_id[is.na(newdf$q_net_friend_9_hidden_id) &  
                                 is.na(newdf$q_net_friend_9_school_year.1) &
                                 !is.na(newdf$q_net_friend_9_how_close)] <- "someone"

newdf$q_net_friend_10_hidden_id[is.na(newdf$q_net_friend_10_hidden_id) &  
                                  is.na(newdf$q_net_friend_10_school_year.1) &
                                  !is.na(newdf$q_net_friend_10_how_close)] <- "someone"


newdf$q_net_friend_emotional_support_outward_1_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_1_hidden_id) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_1_talk) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_1_person) ] <- "no name"


newdf$q_net_friend_emotional_support_outward_2_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_2_hidden_id) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_2_talk) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_2_person) ] <- "no name"

newdf$q_net_friend_emotional_support_outward_3_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_3_hidden_id) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_3_talk) &
                                                           !is.na(newdf$q_net_friend_emotional_support_outward_3_person) ] <- "no name"


newdf$q_net_friend_emotional_support_outward_1_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_1_hidden_id) &
                                                           !is.na(newdf$q_net_friend_1_feeling.1)] <- "someone"

newdf$q_net_friend_emotional_support_outward_2_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_2_hidden_id) &
                                                           !is.na(newdf$q_net_friend_2_feeling.1)] <- "someone"

newdf$q_net_friend_emotional_support_outward_3_hidden_id[is.na(newdf$q_net_friend_emotional_support_outward_3_hidden_id) &
                                                           !is.na(newdf$q_net_friend_3_feeling.1)] <- "someone"





newdf$q_net_friend_emotional_support_inward_1_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_1_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_1_school_year == 1 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_1_gender)] <- "from the school year"
newdf$q_net_friend_emotional_support_inward_1_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_1_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_1_school_year == 2 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_1_gender)] <- "not from the school year"

newdf$q_net_friend_emotional_support_inward_2_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_2_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_2_school_year == 1 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_2_gender)] <- "from the school year"
newdf$q_net_friend_emotional_support_inward_2_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_2_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_2_school_year == 2 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_2_gender)] <- "not from the school year"


newdf$q_net_friend_emotional_support_inward_3_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_3_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_3_school_year == 1 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_3_gender)] <- "from the school year"
newdf$q_net_friend_emotional_support_inward_3_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_3_hidden_id) &
                                                          newdf$q_net_friend_emotional_support_inward_3_school_year == 2 &
                                                          !is.na(newdf$q_net_friend_emotional_support_inward_3_gender)] <- "not from the school year"


newdf$q_net_friend_emotional_support_inward_1_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_1_hidden_id) &
                                                          !is.na(newdf$q_net_friend_1_you_respond_feeling.1)] <- 'someone'

newdf$q_net_friend_emotional_support_inward_2_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_2_hidden_id) &
                                                          !is.na(newdf$q_net_friend_2_you_respond_feeling.1)] <- 'someone'

newdf$q_net_friend_emotional_support_inward_3_hidden_id[is.na(newdf$q_net_friend_emotional_support_inward_3_hidden_id) &
                                                          !is.na(newdf$q_net_friend_3_you_respond_feeling.1)] <- 'someone'

exp1 <- ExpData(newdf,type = 1) # without bullying and with some redundant
exp1

misdat <- map(newdf, ~sum(is.na(.)))
misdat1 <- ldply(misdat, data.frame)
write.table(misdat1, file = "miss_per_col_010321.csv", dec = ',' ,
            sep = ";",row.names = FALSE)

write.csv(newdf, file = "Dataset trimmed_010321.csv")
write.table(newdf, file = "Dataset trimmed_010321_1.csv", dec = ',' ,
            sep = ";",row.names = FALSE)

year2df <- newdf[newdf$respondent_school == 22,]
year4df <- newdf[newdf$respondent_school == 24,]

misdat <- map(year2df, ~sum(is.na(.)))
misdat1 <- ldply(misdat, data.frame)
write.table(misdat1, file = "Y2miss_per_col_010321.csv", dec = ',' ,
            sep = ";",row.names = FALSE)

misdat <- map(year4df, ~sum(is.na(.)))
misdat1 <- ldply(misdat, data.frame)
write.table(misdat1, file = "Y4miss_per_col_010321.csv", dec = ',' ,
            sep = ";",row.names = FALSE)



colnames(newdf)
# read in the order and miss proportion data
ord <- read.csv("Order_proportion_miss_010321.csv", 
                stringsAsFactors = FALSE, sep = ";", dec = ',')
colnames(ord)
dim(ord) # 560 variables
ord$cond[is.na(ord$cond)] <- 0

xx <- ord[,-2]

ord <- as.data.frame(apply(ord[,-2], 2, function(x) as.numeric(x)))
library(ggplot2)

#creating factors
ord$order_sections1 = cut(ord$order_sections1, 3, labels = c('Section 1', 'Section 2',
                                                             'Section 3')) 

boxplot(ord$Proportion_miss_all ~ ord$order_sections1,data = ord, 
        main = "Proportion of missing by section - whole sample", 
        xlab = "Section", ylab = "Proportion of missing")

boxplot(ord$Proportion_Y2 ~ ord$order_sections1,data = ord, 
        main = "Proportion of missing by section - year 2", 
        xlab = "Section", ylab = "Proportion of missing")


boxplot(ord$Proportion_Y4 ~ ord$order_sections1,data = ord, 
        main = "Proportion of missing by section - year 4", 
        xlab = "Section", ylab = "Proportion of missing")


p <- ggplot(ord, aes(y = Proportion_miss_all, x =order_sections1),
            main = "Proportion of missing by section - whole sample") + #, color=order_sections1)) +
  geom_boxplot()
p
# distributions

ggplot(ord, aes(x = ord$Proportion_miss_all, color = as.factor(ord$order_sections1))) +
  geom_density() 
# Basic scatter plot
ggplot(ord, aes(x = order_whole_survey, y = Proportion_miss_all, 
                color = order_sections1)) + geom_point()
# Change the point size, and shape
ggplot(ord, aes(x = order_whole_survey, y = Proportion_miss_all)) +
  geom_point(size = 2, shape = 18, color = 'blue') + geom_rug() +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed",
              color = "darkred")

p <- ggplot(ord, aes(x = order_whole_survey, y = Proportion_miss_all, 
                     color = ord$order_sections1, 
                     shape = ord$order_sections1)) +
  geom_point() + 
  geom_smooth(method = lm, se = TRUE, fullrange = FALSE, 
              linetype = "dashed", color = "darkred") +
  theme_classic()

# Use brewer color palettes
p + scale_color_brewer(palette = "Dark2")
# Use grey scale
p + scale_color_grey()


qplot(ord$Proportion_miss_all, 
      geom = "histogram",
      binwidth = 0.05,  
      main = "Histogram for proportion of missing per item", 
      xlab = "Proportion of missing",  
      fill = I("blue"), 
      col = I("black"), 
      alpha = I(.2)) +
  #   xlim = c(0.0,1)) 
  geom_vline(aes(xintercept = mean(ord$Proportion_miss_all, na.rm = TRUE)),col = 'red',size = 2)


# whole sample

cor.test(ord$Proportion_miss_all, ord$order_whole_survey,  method = "pearson")
cor(ord$Proportion_miss_all, ord$order_whole_survey,  method = "spearman", use = "complete.obs")
ord1 <- ord[ord$order_sections1 == 'Section 1',]
ord2 <- ord[ord$order_sections1 == 'Section 2',]
ord3 <- ord[ord$order_sections1 == 'Section 3',]
ord13 <- ord[ord$order_sections1 != 'Section 2',]
cor.test(ord1$Proportion_miss_all, ord1$order_whole_survey,  method = "pearson")
cor(ord1$Proportion_miss_all, ord1$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord2$Proportion_miss_all, ord2$order_whole_survey,  method = "pearson")
cor(ord2$Proportion_miss_all, ord2$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord3$Proportion_miss_all, ord3$order_whole_survey,  method = "pearson")
cor(ord3$Proportion_miss_all, ord3$order_whole_survey,  method = "spearman", use = "complete.obs")

ord13$section13ord <- seq.int(nrow(ord13))
cor.test(ord13$Proportion_miss_all, ord13$section13ord,  method = "pearson")
cor(ord13$Proportion_miss_all, ord13$section13ord,  method = "spearman", use = "complete.obs")

# Y2
cor.test(ord$Proportion_Y2, ord$order_whole_survey,  method = "pearson")
cor(ord$Proportion_Y2, ord$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord1$Proportion_Y2, ord1$order_whole_survey,  method = "pearson")
cor(ord1$Proportion_Y2, ord1$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord2$Proportion_Y2, ord2$order_whole_survey,  method = "pearson")
cor(ord2$Proportion_Y2, ord2$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord3$Proportion_Y2, ord3$order_whole_survey,  method = "pearson")
cor(ord3$Proportion_Y2, ord3$order_whole_survey,  method = "spearman", use = "complete.obs")


cor.test(ord13$Proportion_Y2, ord13$section13ord,  method = "pearson")
cor(ord13$Proportion_Y2, ord13$section13ord,  method = "spearman", use = "complete.obs")

# Y4

cor.test(ord$Proportion_Y4, ord$order_whole_survey,  method = "pearson")
cor(ord$Proportion_Y4, ord$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord1$Proportion_Y4, ord1$order_whole_survey,  method = "pearson")
cor(ord1$Proportion_Y4, ord1$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord2$Proportion_Y4, ord2$order_whole_survey,  method = "pearson")
cor(ord2$Proportion_Y4, ord2$order_whole_survey,  method = "spearman", use = "complete.obs")

cor.test(ord3$Proportion_Y4, ord3$order_whole_survey,  method = "pearson")
cor(ord3$Proportion_Y4, ord3$order_whole_survey,  method = "spearman", use = "complete.obs")


cor.test(ord13$Proportion_Y4, ord13$section13ord,  method = "pearson")
cor(ord13$Proportion_Y4, ord13$section13ord,  method = "spearman", use = "complete.obs")

# Add the regression line
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth(method=lm)
# Remove the confidence interval
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# Loess method
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth()
# exclude item 394
# make general analysis of missing (mean, distribution, etc. like in exp results)
# identify miss between 5 and 95%, and other groups




# remember your ideas about total missing per individual (and section)!


# Structure of the data - Type = 2
exp2 <- ExpData(df,type = 2)
write.table(exp2, file = "Datastructure of Dataset trimmed for analysis1_1 250221.csv", dec = ',' ,
            sep = ";",row.names = FALSE) 



# sna datset
snadf2 <- subset(newdf, select = c(respondent_id,
                                   respondent_school,
                                   q_6,
                                   q_net_friend_1_hidden_id,
                                   q_net_friend_1_school_year.1,
                                   q_net_friend_1_gender.1,
                                   q_net_friend_1_older_younger.1,
                                   q_net_friend_1_how_close,
                                   q_net_friend_1_how_long,
                                   q_net_friend_1_weekend,
                                   q_net_friend_1_same_clubs_yes_no.1,
                                   q_net_friend_1_same_clubs_sumAll.1,
                                   q_net_friend_2_hidden_id,
                                   q_net_friend_2_school_year.1,
                                   q_net_friend_2_gender.1,
                                   q_net_friend_2_older_younger.1,
                                   q_net_friend_2_how_close,
                                   q_net_friend_2_how_long,
                                   q_net_friend_2_weekend,
                                   q_net_friend_2_same_clubs_yes_no.1,
                                   q_net_friend_2_same_clubs_sumAll.1,
                                   q_net_friend_3_hidden_id,
                                   q_net_friend_3_school_year.1,
                                   q_net_friend_3_gender.1,
                                   q_net_friend_3_older_younger.1,
                                   q_net_friend_3_how_close,
                                   q_net_friend_3_how_long,
                                   q_net_friend_3_weekend,
                                   q_net_friend_3_same_clubs_yes_no.1,
                                   q_net_friend_3_same_clubs_sumAll.1,
                                   q_net_friend_4_hidden_id,
                                   q_net_friend_4_school_year.1,
                                   q_net_friend_4_gender.1,
                                   q_net_friend_4_older_younger.1,
                                   q_net_friend_4_how_close,
                                   q_net_friend_4_how_long,
                                   q_net_friend_4_weekend,
                                   q_net_friend_4_same_clubs_yes_no.1,
                                   q_net_friend_4_same_clubs_sumAll.1,
                                   q_net_friend_5_hidden_id,
                                   q_net_friend_5_school_year.1,
                                   q_net_friend_5_gender.1,
                                   q_net_friend_5_older_younger.1,
                                   q_net_friend_5_how_close,
                                   q_net_friend_5_how_long,
                                   q_net_friend_5_weekend,
                                   q_net_friend_5_same_clubs_yes_no.1,
                                   q_net_friend_5_same_clubs_sumAll.1,
                                   q_net_friend_6_hidden_id,
                                   q_net_friend_6_school_year.1,
                                   q_net_friend_6_gender.1,
                                   q_net_friend_6_older_younger.1,
                                   q_net_friend_6_how_close,
                                   q_net_friend_6_how_long,
                                   q_net_friend_6_weekend,
                                   q_net_friend_6_same_clubs_yes_no.1,
                                   q_net_friend_6_same_clubs_sumAll.1,
                                   q_net_friend_7_hidden_id,
                                   q_net_friend_7_school_year.1,
                                   q_net_friend_7_gender.1,
                                   q_net_friend_7_older_younger.1,
                                   q_net_friend_7_how_close,
                                   q_net_friend_7_how_long,
                                   q_net_friend_7_weekend,
                                   q_net_friend_7_same_clubs_yes_no.1,
                                   q_net_friend_7_same_clubs_sumAll.1,
                                   q_net_friend_8_hidden_id,
                                   q_net_friend_8_school_year.1,
                                   q_net_friend_8_gender.1,
                                   q_net_friend_8_older_younger.1,
                                   q_net_friend_8_how_close,
                                   q_net_friend_8_how_long,
                                   q_net_friend_8_weekend,
                                   q_net_friend_8_same_clubs_yes_no.1,
                                   q_net_friend_8_same_clubs_sumAll.1,
                                   q_net_friend_9_hidden_id,
                                   q_net_friend_9_school_year.1,
                                   q_net_friend_9_gender.1,
                                   q_net_friend_9_older_younger.1,
                                   q_net_friend_9_how_close,
                                   q_net_friend_9_how_long,
                                   q_net_friend_9_weekend,
                                   q_net_friend_9_same_clubs_yes_no.1,
                                   q_net_friend_9_same_clubs_sumAll.1,
                                   q_net_friend_10_hidden_id,
                                   q_net_friend_10_school_year.1,
                                   q_net_friend_10_gender.1,
                                   q_net_friend_10_older_younger.1,
                                   q_net_friend_10_how_close,
                                   q_net_friend_10_how_long,
                                   q_net_friend_10_weekend,
                                   q_net_friend_10_same_clubs_yes_no.1,
                                   q_net_friend_10_same_clubs_sumAll.1,
                                   q_net_friend_emotional_support_outward_1_hidden_id,
                                   q_net_friend_emotional_support_outward_1_talk,
                                   q_net_friend_emotional_support_outward_1_person,
                                   q_net_friend_1_feeling.1,
                                   q_net_friend_1_support.1,
                                   q_net_friend_1_emotions.1,
                                   q_net_friend_1_makes_jokes.1,
                                   q_net_friend_1_changes_subject.1,
                                   q_net_friend_1_mood_text_1.1,
                                   q_net_friend_1_mood_1.1,
                                   q_net_friend_1_mood_text_2.1,
                                   q_net_friend_1_mood_2.1,
                                   q_net_friend_1_mood_text_3.1,
                                   q_net_friend_1_mood_3.1,
                                   q_net_friend_emotional_support_outward_2_hidden_id,
                                   q_net_friend_emotional_support_outward_2_talk,
                                   q_net_friend_emotional_support_outward_2_person,
                                   q_net_friend_2_feeling.1,
                                   q_net_friend_2_support.1,
                                   q_net_friend_2_emotions.1,
                                   q_net_friend_2_makes_jokes.1,
                                   q_net_friend_2_changes_subject.1,
                                   q_net_friend_2_mood_text_1.1,
                                   q_net_friend_2_mood_1.1,
                                   q_net_friend_2_mood_text_2.1,
                                   q_net_friend_2_mood_2.1,
                                   q_net_friend_2_mood_text_3.1,
                                   q_net_friend_2_mood_3.1,
                                   q_net_friend_emotional_support_outward_3_hidden_id,
                                   q_net_friend_emotional_support_outward_3_talk,
                                   q_net_friend_emotional_support_outward_3_person,
                                   q_net_friend_3_feeling.1,
                                   q_net_friend_3_support.1,
                                   q_net_friend_3_emotions.1,
                                   q_net_friend_3_makes_jokes.1,
                                   q_net_friend_3_changes_subject.1,
                                   q_net_friend_3_mood_text_1.1,
                                   q_net_friend_3_mood_1.1,
                                   q_net_friend_3_mood_text_2.1,
                                   q_net_friend_3_mood_2.1,
                                   q_net_friend_3_mood_text_3.1,
                                   q_net_friend_3_mood_3.1,
                                   q_net_friend_emotional_support_inward_1_hidden_id,
                                   q_net_friend_emotional_support_inward_1_school_year,
                                   q_net_friend_emotional_support_inward_1_gender,
                                   q_net_friend_emotional_support_inward_1_older_younger,
                                   q_net_friend_1_you_respond_feeling.1,
                                   q_net_friend_1_you_respond_support.1,
                                   q_net_friend_1_you_respond_emotions.1,
                                   q_net_friend_1_you_respond_makes_jokes.1,
                                   q_net_friend_1_you_respond_changes_subject.1,
                                   q_net_friend_1_you_respond_mood_text_1.1,
                                   q_net_friend_1_you_respond_mood_1.1,
                                   q_net_friend_1_you_respond_mood_text_2.1,
                                   q_net_friend_1_you_respond_mood_2.1,
                                   q_net_friend_1_you_respond_mood_text_3.1,
                                   q_net_friend_1_you_respond_mood_3.1,
                                   q_net_friend_emotional_support_inward_2_hidden_id,
                                   q_net_friend_emotional_support_inward_2_school_year,
                                   q_net_friend_emotional_support_inward_2_gender,
                                   q_net_friend_emotional_support_inward_2_older_younger,
                                   q_net_friend_2_you_respond_feeling.1,
                                   q_net_friend_2_you_respond_support.1,
                                   q_net_friend_2_you_respond_emotions.1,
                                   q_net_friend_2_you_respond_makes_jokes.1,
                                   q_net_friend_2_you_respond_changes_subject.1,
                                   q_net_friend_2_you_respond_mood_text_1.1,
                                   q_net_friend_2_you_respond_mood_1.1,
                                   q_net_friend_2_you_respond_mood_text_2.1,
                                   q_net_friend_2_you_respond_mood_2.1,
                                   q_net_friend_2_you_respond_mood_text_3.1,
                                   q_net_friend_2_you_respond_mood_3.1,
                                   q_net_friend_emotional_support_inward_3_hidden_id,
                                   q_net_friend_emotional_support_inward_3_school_year,
                                   q_net_friend_emotional_support_inward_3_gender,
                                   q_net_friend_emotional_support_inward_3_older_younger,
                                   q_net_friend_3_you_respond_feeling.1,
                                   q_net_friend_3_you_respond_support.1,
                                   q_net_friend_3_you_respond_emotions.1,
                                   q_net_friend_3_you_respond_makes_jokes.1,
                                   q_net_friend_3_you_respond_changes_subject.1,
                                   q_net_friend_3_you_respond_mood_text_1.1,
                                   q_net_friend_3_you_respond_mood_1.1,
                                   q_net_friend_3_you_respond_mood_text_2.1,
                                   q_net_friend_3_you_respond_mood_2.1,
                                   q_net_friend_3_you_respond_mood_text_3.1,
                                   q_net_friend_3_you_respond_mood_3.1,
                                   q_net_friend_trust_1_hidden_id,
                                   q_net_friend_trust_2_hidden_id,
                                   q_net_friend_trust_3_hidden_id,
                                   q_net_friend_best_grades_pals_ladder_1_hidden_id,
                                   q_net_friend_best_grades_pals_ladder_2_hidden_id,
                                   q_net_friend_best_grades_pals_ladder_3_hidden_id,
                                   q_net_friend_respect_pals_ladder_1_hidden_id,
                                   q_net_friend_respect_pals_ladder_2_hidden_id,
                                   q_net_friend_respect_pals_ladder_3_hidden_id,
                                   q_net_friend_people_you_dont_like_1_hidden_id,
                                   q_net_friend_people_you_dont_like_1_reason,
                                   q_net_friend_people_you_dont_like_2_hidden_id,
                                   q_net_friend_people_you_dont_like_2_reason,
                                   q_net_friend_people_you_dont_like_3_hidden_id,
                                   q_net_friend_people_you_dont_like_3_reason,
                                   q_net_friend_pals_ladder_1_hidden_id,
                                   q_net_friend_pals_ladder_2_hidden_id,
                                   q_net_friend_pals_ladder_3_hidden_id,
                                   q_agree_with_the_most,
                                   q_activities_sumAll,
                                   id))



