#################
#               #
#      Name     #
#               #
#################

# 2000 atribute files

# Mark McCann developed this script
#     


#############
#  Purpose  #
#############

# Loads coded data and creates non-network files and variables


##############
#            #
#    Notes   #
#            #
##############

# 

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################


#########################
#                       #
#    Load packages      #
#                       #
#########################

library("plyr")
library("dplyr")
library("psych")


#########################
#                       #
#     Load functions    #
#                       #
#########################
create.attributes <- function(df = NULL){
  
  
  
  df$score1 <- ifelse(df$knwcdm_t == "False", "1", "0")
  df$score2 <- ifelse(df$knwsti_t == "False", "1", "0")
  df$score3 <- ifelse(df$knwdoc_t == "False", "1", "0")
  df$score4 <- ifelse(df$knwcum_t == "False", "1", "0")
  
  df$score1 <- as.numeric(df$score1)
  df$score2 <- as.numeric(df$score2)
  df$score3 <- as.numeric(df$score3)
  df$score4 <- as.numeric(df$score4)
  
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$know.var.utile <- as.data.frame(cbind(df$score1, df$score2, df$score3, df$score4))
  #table(df$know.var.utile)
  #alpha(df$know.var.utile, na.rm = T)$total[[1]] # no good
  
  df$know.var <- rowSums(cbind(df$score1, df$score2, df$score3, df$score4), na.rm=F)
  
  
  #table(df$know.var, useNA = "ifany")
  #hist(df$know.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~~~#
  # Attitude questions #
  #~~~~~~~~~~~~~~~~~~~~#
  df$Ok_asknude       <- df$opnude_t
  df$Porn_real        <- df$oporn_t
  df$Partner_happy    <- df$opagree_t
  df$Drunk_notOK      <- df$opdrunk_t 
  df$Risk             <- df$oprisk_t
  
  df$Ok_asknude <- ifelse(df$opnude_t == "Strongly disagree", "1", 
                          ifelse(df$opnude_t == "Disagree", "1", "0"))
  
  df$Porn_real <- ifelse(df$oporn_t == "Strongly disagree", "1", 
                         ifelse(df$oporn_t == "Disagree", "1", "0"))
  
  df$Partner_happy <- ifelse(df$opagree_t == "Strongly disagree", "1", 
                             ifelse(df$opagree_t == "Disagree", "1", "0"))
  
  df$Drunk_notOK <- ifelse(df$opdrunk_t == "Strongly disagree", "0", 
                           ifelse(df$opdrunk_t == "Disagree", "0", "1"))
  
  df$Risk <- ifelse(df$oprisk_t == "Strongly disagree", "1", 
                    ifelse(df$oprisk_t == "Disagree", "1", "0"))
  
  df$Ok_asknude <- as.numeric(df$Ok_asknude)
  df$Porn_real <- as.numeric(df$Porn_real)
  df$Partner_happy <- as.numeric(df$Partner_happy )
  df$Drunk_notOK <- as.numeric(df$Drunk_notOK)
  df$Risk <- as.numeric(df$Risk)
  
  #table(df$Ok_asknude)
  #table(df$Porn_real)
  #table(df$Partner_happy)
  #table(df$Drunk_notOK)
  #table(df$Risk)
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$att.score.utile <- as.data.frame(cbind(df$Ok_asknude, df$Porn_real, df$Partner_happy, df$Drunk_notOK, df$Risk))
  #table(df$att.score.utile)
  #alpha(df$att.score.utile, na.rm = T)$total[[1]] # no good
  
  
  df$att.var <- rowSums(cbind(df$Ok_asknude, df$Porn_real, df$Partner_happy, df$Drunk_notOK, df$Risk), na.rm=F)
  
  
  table(df$att.var, useNA = "ifany")
  #hist(df$att.var, col = "sky blue", main= "Histogram of attitude variable") # labels = TRUE)
  
  #summary (df$att.var)
  
  #~~~~~~~~~~~~~~~~~~~#
  #Confidence measures#
  #~~~~~~~~~~~~~~~~~~~#
  ###lower scorse = higher confidence - TO BE INVERTED
  df$Use_condom    <- 6 - df$conput 
  df$Get_condom   <- 6 - df$conget
  df$Refuse_unprot <- 6 - df$conref 
  
  #table(df$Use_condom)
  #table(df$Get_condom)
  #table(df$Refuse_unprot)
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$conf.var.utile <- as.data.frame(cbind(df$Use_condom, df$Get_condom, df$Refuse_unprot))
  table(df$conf.var.utile)
  alpha(df$conf.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # not good
  
  df$conf.var <- rowSums(cbind(df$Use_condom, df$Get_condom,  df$Refuse_unprot), na.rm=F)
  
  table(df$conf.var, useNA = "ifany")
  #hist(df$conf.var, col = "red")
  
  #0 = no confidence a t all,  1 = low, 2 = medium, 3 = high confidence
  
  #df$conf.var <- ifelse(df$conf.var == "0", "0", 
  #                      ifelse(df$conf.var == "1", "1", 
  #                             ifelse(df$conf.var == "2", "1", 
  #                                    ifelse(df$conf.var == "3", "1", 
  #                                           ifelse(df$conf.var == "4", "1",  
  #                                                  ifelse(df$conf.var == "5", "1",  
  #                                                         ifelse(df$conf.var == "6", "2",
  #                                                                ifelse(df$conf.var == "7", "2",
  #                                                                       ifelse(df$conf.var == "8", "2",
  #                                                                              ifelse(df$conf.var == "9", "2",
  #                                                                                     ifelse(df$conf.var == "10", "2",
  #                                                                                            ifelse(df$conf.var == "11", "3",
  #                                                                                                   ifelse(df$conf.var == "12", "3",
  #                                                                                                          ifelse(df$conf.var == "13", "3",
  #                                                                                                                 ifelse(df$conf.var == "14", "3",      
  #                                                                                                                        ifelse(df$conf.var == "15", "3", "NA"))))))))))))))))
  
  
  
  
  table(df$conf.var, useNA = "ifany")
  df$conf.var <- as.numeric(df$conf.var)
  
  #~~~~~~~~~~~~~~~~~~~~#
  # scale # scale.var
  #~~~~~~~~~~~~~~~~~~~~#
  #Low and high categories are dfd on scores that were at least one standard deviation below and above the mean 
  
  df$SWEMWBS
  mean(df$SWEMWBS, na.rm = TRUE)
  sd(df$SWEMWBS, na.rm = TRUE)
  #21.34827 - 4.016282
  #21.34827 + 4.016282
  
  
  df$scale.var <- ifelse(df$SWEMWBS >= 7.00 & df$SWEMWBS < 17.33, 1, 
                         ifelse(df$SWEMWBS >= 17.34 & df$SWEMWBS < 25.36, 2, 
                                ifelse(df$SWEMWBS >= 25.37 & df$SWEMWBS < 35, 3, NA)))    
  
  table(df$scale.var)                            
  
  
  #~~~~~~~~~~~~~~~~~~~~#
  # gathering STIs info online # info.var
  #~~~~~~~~~~~~~~~~~~~~#
  
  #df$webinfo <- df$webinfo_t 
  # Never "1" -> 0 / Once or twoce "2" -> 1 / 3 to 10 times "3" -> 2 / More than 10 times "4" -> 3
  
  #df$info<- ifelse(df$webinfo_t == "Never","0",
  #                   ifelse(df$webinfo_t == "Once or twice", "1", 
  #                          ifelse(df$webinfo_t == "3-10times", "2", 
  #                                 ifelse(df$webinfo_t == ">10 times", "3", NA))))
  
  #table(df$info, useNA = "ifany")
  
  #df$info.var <- as.numeric(df$info)                           
  #class(df$info.var)
  #table(df$info.var)
  #hist(df$info.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~#
  # Sexual activity  # 
  #~~~~~~~~~~~~~~~~~~#
  
  df$Oral        <- df$oralyes_t
  df$Intercourse <- df$intyes_t
  df$Kissing     <- df$kiss_t
  df$Genitals    <- df$gent_t
  df$Masturbate  <- df$mstbte_t
  
  # No & None = 0; [No experience]
  # Yes & Yes, last 6 months (yes in the last six months) = 1; [Experience]
  # Yes, more than 6 months (= yes more than 6 months ago) = 1; [Some experience]
  # missings coded as none
  
  df$Oral <- ifelse(df$Oral == "No", "0", "1")
  df$Oral<-as.numeric(df$Oral)
  
  df$Intercourse <- ifelse(df$Intercourse == "No", "0", "1" )
  df$Intercourse <-as.numeric(df$Intercourse)
  
  ###Massive missingness on the intercourse question. Recode all missings to No
  table(df$Intercourse, useNA = "always")
  df$Intercourse[is.na(df$Intercourse)] <- 0
  
  df$Kissing <- ifelse(df$Kissing == "Never", "0", "1")
  df$Kissing <-as.numeric(df$Kissing)
  
  df$Genitals <- ifelse(df$Genitals == "Never", "0", "1")
  df$Genitals<-as.numeric(df$Genitals)
  
  df$Masturbate <- ifelse(df$Masturbate == "Never", "0", "1")
  df$Masturbate <- as.numeric(df$Masturbate)
  
  table(df$Oral, useNA = "always")
  table(df$Genitals, useNA = "always")
  table(df$Kissing, useNA = "always")
  table(df$Intercourse, useNA = "always")
  table(df$Masturbate, useNA = "always")
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$sex.var.utile <- as.data.frame(cbind(df$Oral, df$Intercourse, df$Kissing, df$Genitals, df$Masturbate))
  table(df$sex.var.utile)
  #alpha(df$sex.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # yes - I can do it
  
  df$sex.var <- rowSums(cbind(df$Oral, df$Genitals,  df$Kissing, df$Intercourse, df$Masturbate), na.rm=F)
  
  table(df$sex.var, useNA = "ifany")
  #hist(df$sex.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~#
  # Three cat sex var#
  #~~~~~~~~~~~~~~~~~~#
  
  df$sex3.var <- ifelse(           df$Kissing %in%    0 & 
                                     df$Genitals     == 0 & 
                                     df$Masturbate   == 0 & 
                                     df$Oral         == 0 &
                                     df$Intercourse  == 0 
                                   , "None",  
                                   ifelse(      df$Oral         %in% 1 |
                                                  df$Intercourse  == 1 
                                                , "Experience",
                                                "Some experience") ) 
  
  ##Replace as missing anyone who was missing on all sex vars as well as intercourse
  
  df$sex3.var[is.na(df$Oral) &
                is.na(df$Genitals) &
                is.na(df$Kissing) &
                is.na(df$Masturbate &
                        is.na(df$intyes_t))] <- NA
  
  table(df$sex3.var, useNA = "always")
  
  df$sex3.var <- as.factor(df$sex3.var)
  
  #~~~~~~~~~~~~~~~~~~#
  # Talk var
  #~~~~~~~~~~~~~~~~~~#
  df$chabod   <- df$chabod_t
  df$charel   <- df$charel_t
  df$chaready <- df$chaready_t
  df$chasti   <- df$chasti_t
  df$chasxt   <- df$chasxt_t
  # No  = 0; 
  # Yes = 1; 
  # missings coded as none
  
  df$chabod <- ifelse(df$chabod == "No", "0", "1")
  df$chabod<-as.numeric(df$chabod)
  
  df$charel <- ifelse(df$charel == "No", "0", "1" )
  df$charel<-as.numeric(df$charel)
  
  df$chaready <- ifelse(df$chaready == "No", "0", "1")
  df$chaready <-as.numeric(df$chaready)
  
  df$chasti <- ifelse(df$chasti == "No", "0", "1")
  df$chasti<-as.numeric(df$chasti)
  
  df$chasxt <- ifelse(df$chasxt == "No", "0", "1")
  df$chasxt<-as.numeric(df$chasxt)
  
  table(df$chabod, useNA = "always")
  table(df$charel, useNA = "always")
  table(df$chaready, useNA = "always")
  table(df$chasti, useNA = "always")
  table(df$chasxt, useNA = "always")
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$talk.var.utile <- as.data.frame(cbind(df$chabod, df$charel, df$chaready, 
                                           df$chasti, df$chasxt))
  table(df$talk.var.utile)
  alpha(df$talk.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # yes - I can do it
  
  df$talk.var <- rowSums(cbind(df$chabod, df$charel, df$chaready, 
                               df$chasti, df$chasxt), na.rm=F)
  
  #table(df$talk.var, useNA = "ifany")
  #hist(df$talk.var, col = "red")
  
  
  
  ##################### ##################### ##################### #####################
  ##################### ##################### ##################### #####################
  
  # # # # # # Create Attribute File # # # 
  
  df.attribute.file <- df[,c("id", "gender", "scale.var", "talk.var",
                             "sex3.var", "conf.var", "know.var", "att.var","respondent_school")]
  
  
  return(df.attribute.file)               
  
        }       
                        

create.attributes.with.raw <- function(df = NULL){
  
  
                                                                                                                  
  df$score1 <- ifelse(df$knwcdm_t == "False", "1", "0")
  df$score2 <- ifelse(df$knwsti_t == "False", "1", "0")
  df$score3 <- ifelse(df$knwdoc_t == "False", "1", "0")
  df$score4 <- ifelse(df$knwcum_t == "False", "1", "0")
  
  df$score1 <- as.numeric(df$score1)
  df$score2 <- as.numeric(df$score2)
  df$score3 <- as.numeric(df$score3)
  df$score4 <- as.numeric(df$score4)
  
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$know.var.utile <- as.data.frame(cbind(df$score1, df$score2, df$score3, df$score4))
  #table(df$know.var.utile)
  #alpha(df$know.var.utile, na.rm = T)$total[[1]] # no good
  
  df$know.var <- rowSums(cbind(df$score1, df$score2, df$score3, df$score4), na.rm=F)
  
  
  #table(df$know.var, useNA = "ifany")
  #hist(df$know.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~~~#
  # Attitude questions #
  #~~~~~~~~~~~~~~~~~~~~#
  df$Ok_asknude       <- df$opnude_t
  df$Porn_real        <- df$oporn_t
  df$Partner_happy    <- df$opagree_t
  df$Drunk_notOK      <- df$opdrunk_t 
  df$Risk             <- df$oprisk_t
  
  df$Ok_asknude <- ifelse(df$opnude_t == "Strongly disagree", "1", 
                          ifelse(df$opnude_t == "Disagree", "1", "0"))
  
  df$Porn_real <- ifelse(df$oporn_t == "Strongly disagree", "1", 
                         ifelse(df$oporn_t == "Disagree", "1", "0"))
  
  df$Partner_happy <- ifelse(df$opagree_t == "Strongly disagree", "1", 
                             ifelse(df$opagree_t == "Disagree", "1", "0"))
  
  df$Drunk_notOK <- ifelse(df$opdrunk_t == "Strongly disagree", "0", 
                           ifelse(df$opdrunk_t == "Disagree", "0", "1"))
  
  df$Risk <- ifelse(df$oprisk_t == "Strongly disagree", "1", 
                    ifelse(df$oprisk_t == "Disagree", "1", "0"))
  
  df$Ok_asknude <- as.numeric(df$Ok_asknude)
  df$Porn_real <- as.numeric(df$Porn_real)
  df$Partner_happy <- as.numeric(df$Partner_happy )
  df$Drunk_notOK <- as.numeric(df$Drunk_notOK)
  df$Risk <- as.numeric(df$Risk)
  
  #table(df$Ok_asknude)
  #table(df$Porn_real)
  #table(df$Partner_happy)
  #table(df$Drunk_notOK)
  #table(df$Risk)
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$att.score.utile <- as.data.frame(cbind(df$Ok_asknude, df$Porn_real, df$Partner_happy, df$Drunk_notOK, df$Risk))
  #table(df$att.score.utile)
  #alpha(df$att.score.utile, na.rm = T)$total[[1]] # no good
  
  
  df$att.var <- rowSums(cbind(df$Ok_asknude, df$Porn_real, df$Partner_happy, df$Drunk_notOK, df$Risk), na.rm=F)
  
  
  table(df$att.var, useNA = "ifany")
  #hist(df$att.var, col = "sky blue", main= "Histogram of attitude variable") # labels = TRUE)
  
  #summary (df$att.var)
  
  #~~~~~~~~~~~~~~~~~~~#
  #Confidence measures#
  #~~~~~~~~~~~~~~~~~~~#
  ###lower scorse = higher confidence - TO BE INVERTED
  df$Use_condom    <- 6 - df$conput 
  df$Get_condom   <- 6 - df$conget
  df$Refuse_unprot <- 6 - df$conref 
  
  #table(df$Use_condom)
  #table(df$Get_condom)
  #table(df$Refuse_unprot)
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$conf.var.utile <- as.data.frame(cbind(df$Use_condom, df$Get_condom, df$Refuse_unprot))
  table(df$conf.var.utile)
  alpha(df$conf.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # not good
  
  df$conf.var <- rowSums(cbind(df$Use_condom, df$Get_condom,  df$Refuse_unprot), na.rm=F)
  
  table(df$conf.var, useNA = "ifany")
  #hist(df$conf.var, col = "red")
  
  #0 = no confidence a t all,  1 = low, 2 = medium, 3 = high confidence
  
  #df$conf.var <- ifelse(df$conf.var == "0", "0", 
  #                      ifelse(df$conf.var == "1", "1", 
  #                             ifelse(df$conf.var == "2", "1", 
  #                                    ifelse(df$conf.var == "3", "1", 
  #                                           ifelse(df$conf.var == "4", "1",  
  #                                                  ifelse(df$conf.var == "5", "1",  
  #                                                         ifelse(df$conf.var == "6", "2",
  #                                                                ifelse(df$conf.var == "7", "2",
  #                                                                       ifelse(df$conf.var == "8", "2",
  #                                                                              ifelse(df$conf.var == "9", "2",
  #                                                                                     ifelse(df$conf.var == "10", "2",
  #                                                                                            ifelse(df$conf.var == "11", "3",
  #                                                                                                   ifelse(df$conf.var == "12", "3",
  #                                                                                                          ifelse(df$conf.var == "13", "3",
  #                                                                                                                 ifelse(df$conf.var == "14", "3",      
  #                                                                                                                        ifelse(df$conf.var == "15", "3", "NA"))))))))))))))))
  
  
  
  
  table(df$conf.var, useNA = "ifany")
  df$conf.var <- as.numeric(df$conf.var)
  
  #~~~~~~~~~~~~~~~~~~~~#
  # scale # scale.var
  #~~~~~~~~~~~~~~~~~~~~#
  #Low and high categories are dfd on scores that were at least one standard deviation below and above the mean 
  
  df$SWEMWBS
  mean(df$SWEMWBS, na.rm = TRUE)
  sd(df$SWEMWBS, na.rm = TRUE)
  #21.34827 - 4.016282
  #21.34827 + 4.016282
  
  
  df$scale.var <- ifelse(df$SWEMWBS >= 7.00 & df$SWEMWBS < 17.33, 1, 
                         ifelse(df$SWEMWBS >= 17.34 & df$SWEMWBS < 25.36, 2, 
                                ifelse(df$SWEMWBS >= 25.37 & df$SWEMWBS < 35, 3, NA)))    
  
  table(df$scale.var)                            
  
  
  #~~~~~~~~~~~~~~~~~~~~#
  # gathering STIs info online # info.var
  #~~~~~~~~~~~~~~~~~~~~#
  
  #df$webinfo <- df$webinfo_t 
  # Never "1" -> 0 / Once or twoce "2" -> 1 / 3 to 10 times "3" -> 2 / More than 10 times "4" -> 3
  
  #df$info<- ifelse(df$webinfo_t == "Never","0",
  #                   ifelse(df$webinfo_t == "Once or twice", "1", 
  #                          ifelse(df$webinfo_t == "3-10times", "2", 
  #                                 ifelse(df$webinfo_t == ">10 times", "3", NA))))
  
  #table(df$info, useNA = "ifany")
  
  #df$info.var <- as.numeric(df$info)                           
  #class(df$info.var)
  #table(df$info.var)
  #hist(df$info.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~#
  # Sexual activity  # 
  #~~~~~~~~~~~~~~~~~~#
  
  df$Oral        <- df$oralyes_t
  df$Intercourse <- df$intyes_t
  df$Kissing     <- df$kiss_t
  df$Genitals    <- df$gent_t
  df$Masturbate  <- df$mstbte_t
  
  # No & None = 0; [No experience]
  # Yes & Yes, last 6 months (yes in the last six months) = 1; [Experience]
  # Yes, more than 6 months (= yes more than 6 months ago) = 1; [Some experience]
  # missings coded as none
  
  df$Oral <- ifelse(df$Oral == "No", "0", "1")
  df$Oral<-as.numeric(df$Oral)
  
  df$Intercourse <- ifelse(df$Intercourse == "No", "0", "1" )
  df$Intercourse <-as.numeric(df$Intercourse)
  
  ###Massive missingness on the intercourse question. Recode all missings to No
  table(df$Intercourse, useNA = "always")
  df$Intercourse[is.na(df$Intercourse)] <- 0
  
  df$Kissing <- ifelse(df$Kissing == "Never", "0", "1")
  df$Kissing <-as.numeric(df$Kissing)
  
  df$Genitals <- ifelse(df$Genitals == "Never", "0", "1")
  df$Genitals<-as.numeric(df$Genitals)
  
  df$Masturbate <- ifelse(df$Masturbate == "Never", "0", "1")
  df$Masturbate <- as.numeric(df$Masturbate)
  
  table(df$Oral, useNA = "always")
  table(df$Genitals, useNA = "always")
  table(df$Kissing, useNA = "always")
  table(df$Intercourse, useNA = "always")
  table(df$Masturbate, useNA = "always")
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$sex.var.utile <- as.data.frame(cbind(df$Oral, df$Intercourse, df$Kissing, df$Genitals, df$Masturbate))
  table(df$sex.var.utile)
  #alpha(df$sex.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # yes - I can do it
  
  df$sex.var <- rowSums(cbind(df$Oral, df$Genitals,  df$Kissing, df$Intercourse, df$Masturbate), na.rm=F)
  
  table(df$sex.var, useNA = "ifany")
  #hist(df$sex.var, col = "red")
  
  #~~~~~~~~~~~~~~~~~~#
  # Three cat sex var#
  #~~~~~~~~~~~~~~~~~~#
  
  df$sex3.var <- ifelse(           df$Kissing %in%    0 & 
                                     df$Genitals     == 0 & 
                                     df$Masturbate   == 0 & 
                                     df$Oral         == 0 &
                                     df$Intercourse  == 0 
                                   , "None",  
                                   ifelse(      df$Oral         %in% 1 |
                                                  df$Intercourse  == 1 
                                                , "Experience",
                                                "Some experience") ) 
  
  ##Replace as missing anyone who was missing on all sex vars as well as intercourse
  
  df$sex3.var[is.na(df$Oral) &
                is.na(df$Genitals) &
                is.na(df$Kissing) &
                is.na(df$Masturbate &
                        is.na(df$intyes_t))] <- NA
  
  table(df$sex3.var, useNA = "always")
  
  df$sex3.var <- as.factor(df$sex3.var)
  
  #~~~~~~~~~~~~~~~~~~#
  # Talk var
  #~~~~~~~~~~~~~~~~~~#
  df$chabod   <- df$chabod_t
  df$charel   <- df$charel_t
  df$chaready <- df$chaready_t
  df$chasti   <- df$chasti_t
  df$chasxt   <- df$chasxt_t
  # No  = 0; 
  # Yes = 1; 
  # missings coded as none
  
  df$chabod <- ifelse(df$chabod == "No", "0", "1")
  df$chabod<-as.numeric(df$chabod)
  
  df$charel <- ifelse(df$charel == "No", "0", "1" )
  df$charel<-as.numeric(df$charel)
  
  df$chaready <- ifelse(df$chaready == "No", "0", "1")
  df$chaready <-as.numeric(df$chaready)
  
  df$chasti <- ifelse(df$chasti == "No", "0", "1")
  df$chasti<-as.numeric(df$chasti)
  
  df$chasxt <- ifelse(df$chasxt == "No", "0", "1")
  df$chasxt<-as.numeric(df$chasxt)
  
  table(df$chabod, useNA = "always")
  table(df$charel, useNA = "always")
  table(df$chaready, useNA = "always")
  table(df$chasti, useNA = "always")
  table(df$chasxt, useNA = "always")
  
  #alpha() below 0.5 IS NOT GOOD -  0.8 or 0.7 is OK
  df$talk.var.utile <- as.data.frame(cbind(df$chabod, df$charel, df$chaready, 
                                           df$chasti, df$chasxt))
  table(df$talk.var.utile)
  alpha(df$talk.var.utile, na.rm = T, check.keys=TRUE)$total[[1]] # yes - I can do it
  
  df$talk.var <- rowSums(cbind(df$chabod, df$charel, df$chaready, 
                               df$chasti, df$chasxt), na.rm=F)
  
  #table(df$talk.var, useNA = "ifany")
  #hist(df$talk.var, col = "red")
  
  
  
  ##################### ##################### ##################### #####################
  ##################### ##################### ##################### #####################
  
  
  
  return(df)
  
}




#########################
#                       #
#  Main body of script  #
#                       #
#########################



load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/recoded_baseline.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/recoded_control.rdata")


baseline.attributes <- create.attributes(recoded.baseline)
control.attributes  <- create.attributes(recoded.control)

baseline.attributes.with.raw <- create.attributes.with.raw(recoded.baseline)
control.attributes.with.raw  <- create.attributes.with.raw(recoded.control)


summary(baseline.attributes)
summary(control.attributes)


save(baseline.attributes, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_attributes.rdata")
save(control.attributes, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_attributes.rdata")

save(baseline.attributes.with.raw, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_attributes_with_raw.rdata")
save(control.attributes.with.raw, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_attributes_with_raw.rdata")


##################### ##################### ##################### #####################
##################### ##################### ##################### #####################
################        Outside school friends variable 


#########Baseline

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_raw_net_qs.rdata")

baseline.sch.attr <- list()
for (i in 1:6){
  baseline.sch.attr[[i]] <- as.data.frame(raw.baseline.network.questions[[i]][,c(1, 10, 17, 24, 31, 38, 45)])
}

# Set missing values to NA
for (i in 1:6){
  is.na(baseline.sch.attr[[i]]) <- !baseline.sch.attr[[i]]
} 

baseline.sch.out <- list()
for (i in 1:6){
  baseline.sch.out[[i]] <- apply(baseline.sch.attr[[i]], 1, function(x) length(which(x==2)))
}


baseline.outside.school <- list()
for(i in 1:6){
  baseline.outside.school[[i]] <- data.frame(baseline.sch.attr[[i]]$respondent_id, baseline.sch.out[[i]])
  colnames(baseline.outside.school[[i]])<- c("respondent_id","outschfriends")
}

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_raw_net_qs.rdata")

control.sch.attr <- list()
for (i in 1:6){
  control.sch.attr[[i]] <- as.data.frame(raw.control.network.questions[[i]][,c(1, 10, 17, 24, 31, 38, 45)])
}

# Set missing values to NA
for (i in 1:6){
  is.na(control.sch.attr[[i]]) <- !control.sch.attr[[i]]
} 

control.sch.out <- list()
for (i in 1:6){
  control.sch.out[[i]] <- apply(control.sch.attr[[i]], 1, function(x) length(which(x==2)))
}


control.outside.school <- list()
for(i in 1:6){
  control.outside.school[[i]] <- data.frame(control.sch.attr[[i]]$respondent_id, control.sch.out[[i]])
  colnames(control.outside.school[[i]])<- c("respondent_id","outschfriends")
}


save(control.outside.school, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_outschool_friends.rdata")
save(baseline.outside.school, file = "//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_outschool_friends.rdata")



