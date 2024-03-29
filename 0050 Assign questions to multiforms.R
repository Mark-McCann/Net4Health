#################
#               #
#      Name     #
#               #
#################

#Mark McCann created this script

#This takes the question labels from the database to manual 
# multiform development.

#It also takes the manual multiform Set information and 
# outputs question labels within each multiform.

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(readxl)
library (dplyr)

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
rm(list=ls())
######################
#####Load in data
#######################


#####Getting the question labels from the PHRF datbase
#Load data
df <- read.csv("C:/Users/mmc78h/OneDrive - University of Glasgow/Peers and networks/N4h multiform/net4qs.csv"
            ,sep = ""   )

colnames(df)

setwd("C:/Users/mmc78h/OneDrive - University of Glasgow/Peers and networks/N4h multiform/")

write.csv(colnames(df) ,"net4 varnames.csv")

####These labels were used to update the multiform information


######Creating the Multiform form by label info


#Load data

df <- read_excel("C:/Users/mmc78h/Downloads/Multiform information.xlsx",
                 sheet = 'Question sets')

##Delete rows that total up numbers and leave just the quetion numbers
head(df)
tail(df)
dim(df)

df <- df[c(1,6:328),]

#Create sets

SetX <- df$`New Set X`[!is.na(df$`New Set X`)]
SetA <- df$`New Set A`[!is.na(df$`New Set A`)]
SetB <- df$`New Set B`[!is.na(df$`New Set B`)]
SetC <- df$`New Set C`[!is.na(df$`New Set C`)]
SetD <- df$`New Set D`[!is.na(df$`New Set D`)]
SetE <- df$`New Set E`[!is.na(df$`New Set E`)]
SetF <- df$`New Set F`[!is.na(df$`New Set F`)]




#Create forms

#1	X	A	B				
Form1 <- c(SetX,SetA, SetB)

#2	X	A		C			
Form2 <- c(SetX,SetA, SetC)

#3	X	A			D		
Form3 <- c(SetX,SetA, SetD)
#4	X	A				E	
Form4 <- c(SetX,SetA, SetE)
#5	X	A					F
Form5 <- c(SetX,SetA, SetF)
#6	X		B	C			
Form6 <- c(SetX,SetB, SetC)
#7	X		B		D		
Form7 <- c(SetX,SetB, SetD)
#8	X		B			E	
Form8 <- c(SetX,SetB, SetE)
#9	X		B				F
Form9 <- c(SetX,SetB, SetF)
#10	X			C	D		
Form10 <- c(SetX,SetC, SetD)
#11	X			C		E	
Form11 <- c(SetX,SetC, SetE)
#12	X			C			F
Form12 <- c(SetX,SetC, SetF)
#13	X				D	E	
Form13 <- c(SetX,SetD, SetE)
#14	X				D		F
Form14 <- c(SetX,SetD, SetF)
#15	X					E	F
Form15 <- c(SetX,SetE, SetF)

#All Qs
Form0 <- c(SetX,SetA,
           SetB,
           SetC,
           SetD,
           SetE,
           SetF)

length(Form0)
length(Form15)

##Put all forms into a data frame.
formdf <- data.frame(Form0)
#Add NAs for the trailing end of the shorter Forms

formdf$FormA <- c(Form1, rep(NA, length(Form0) - length(Form1)))
formdf$FormB <- c(Form2, rep(NA, length(Form0) - length(Form2)))
formdf$FormC <- c(Form3, rep(NA, length(Form0) - length(Form3)))
formdf$FormD <- c(Form4, rep(NA, length(Form0) - length(Form4)))
formdf$FormE <- c(Form5, rep(NA, length(Form0) - length(Form5)))
formdf$FormF <- c(Form6, rep(NA, length(Form0) - length(Form6)))
formdf$FormG <- c(Form7, rep(NA, length(Form0) - length(Form7)))
formdf$FormH <- c(Form8, rep(NA, length(Form0) - length(Form8)))
formdf$FormI <- c(Form9, rep(NA, length(Form0) - length(Form9)))
formdf$FormJ <- c(Form10, rep(NA, length(Form0) - length(Form10)))
formdf$FormK <- c(Form11, rep(NA, length(Form0) - length(Form11)))
formdf$FormL <- c(Form12, rep(NA, length(Form0) - length(Form12)))
formdf$FormM <- c(Form13, rep(NA, length(Form0) - length(Form13)))
formdf$FormN <- c(Form14, rep(NA, length(Form0) - length(Form14)))
formdf$FormO <- c(Form15, rep(NA, length(Form0) - length(Form15)))

head(formdf)
tail(formdf)

write.csv(formdf,  "Question labels for multiforms.csv")



######Create list of excluded questions per form

#Create forms

#1	X	A	B				
notFormA <- c(SetC,SetD, SetE, SetF)

#2	X	A		C			
notFormB <- c(SetB, SetD, SetE, SetF)

#3	X	A			D		
notFormC <- c(SetB,SetC, SetE, SetF)
#4	X	A				E	
notFormD <- c(SetB,SetC, SetD, SetF)
#5	X	A					F
notFormE <- c(SetB, SetC,SetD, SetE)
#6	X		B	C			
notFormF <- c(SetA, SetD,SetE, SetF)
#7	X		B		D		
notFormG <- c(SetA, SetC,SetE, SetF)
#8	X		B			E	
notFormH <- c(SetA, SetC,SetD, SetF)
#9	X		B				F
notFormI <- c(SetA, SetC,SetD, SetE)
#10	X			C	D		
notFormJ <- c(SetA, SetB,SetE, SetF)
#11	X			C		E	
notFormK <- c(SetA, SetB,SetD, SetF)
#12	X			C			F
notFormL <- c(SetA, SetB,SetD, SetE)
#13	X				D	E	
notFormM <- c(SetA, SetB,SetC, SetF)
#14	X				D		F
notFormN <- c(SetA, SetB,SetC, SetE)
#15	X					E	F
notFormO <- c(SetA, SetB,SetC, SetD)


notformdf <- data.frame(Form0)
#Add NAs for the trailing end of the shorter Forms

notformdf$FormA <- c(notFormA, rep(NA, length(Form0) - length(notFormA)))
notformdf$FormB <- c(notFormB, rep(NA, length(Form0) - length(notFormB)))
notformdf$FormC <- c(notFormC, rep(NA, length(Form0) - length(notFormC)))
notformdf$FormD <- c(notFormD, rep(NA, length(Form0) - length(notFormD)))
notformdf$FormE <- c(notFormE, rep(NA, length(Form0) - length(notFormE)))
notformdf$FormF <- c(notFormF, rep(NA, length(Form0) - length(notFormF)))
notformdf$FormG <- c(notFormG, rep(NA, length(Form0) - length(notFormG)))
notformdf$FormH <- c(notFormH, rep(NA, length(Form0) - length(notFormH)))
notformdf$FormI <- c(notFormI, rep(NA, length(Form0) - length(notFormI)))
notformdf$FormJ <- c(notFormJ, rep(NA, length(Form0) - length(notFormJ)))
notformdf$FormK <- c(notFormK, rep(NA, length(Form0) - length(notFormK)))
notformdf$FormL <- c(notFormL, rep(NA, length(Form0) - length(notFormL)))
notformdf$FormM <- c(notFormM, rep(NA, length(Form0) - length(notFormM)))
notformdf$FormN <- c(notFormN, rep(NA, length(Form0) - length(notFormN)))
notformdf$FormO <- c(notFormO, rep(NA, length(Form0) - length(notFormO)))

notformdf$Form0 <- NULL

write.csv(notformdf,  "Excluded variables for multiforms.csv")
