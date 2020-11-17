rm(list=ls())
# Set your working dir as the current dir
#add your path and put Study1_ready_yujing_1a_short.sav and Study1_ready_yujing_1b_short.sav 
#and this file into one folder.
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis") 
dev = "png"
# Read data
library("haven")
my_data_original_1a <- read_sav("Study1_ready_yujing_1a_short.sav")
my_data_original_1b <- read_sav("Study1_ready_yujing_1b_short.sav")

# loading libraries used for analysis
library("papaja")
library("pwr")
library("MASS")
library("psych")
library("ggpubr")
library("lsmeans")
library("multcompView")
library("ggpubr")
library("sjstats")
library("car")
library("ggplot2")
library("fancycut")
library("numform")
library("ez")

## 1.Truth_age
# 1.1Extract columns
my_data_extracted_A_T1 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","TruthPosAge","TruthNegAge","TruthIn","TruthOut"))

# 1.2Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Ttest_AT<- t.test(my_data_extracted_A_T1$TruthPosAge, my_data_extracted_A_T1$TruthNegAge, paired=TRUE, alternative = "two.sided")
sum_Ttest_AT<- summary(Ttest_AT)

# 1.3Get descriptives
#Stack
library(reshape2)
my_data_age_T1 <- melt(my_data_extracted_A_T1, id.vars=1:4)
my_data_age_T1
#valence
my_data_age_T1$valence <- ifelse(my_data_age_T1$variable=="TruthPosAge", "1","2")
#describeBy
library(psych) 
Descriptives_AT_TTEST<-describeBy(my_data_age_T1, 
                                  group = my_data_age_T1$valence)
# 1.4Truth: Group membership x Valence Interaction 
# Extract columns
my_data_extracted_A_T2 <- subset(my_data_original_1b, select = c("ID", "agegroup_obj","Consistency","Format","TruthInPos","TruthOutPos", "TruthInNeg", "TruthOutNeg"))
#Stack
library(reshape2)
my_data_age_T <- melt(my_data_extracted_A_T2, id.vars=1:4)
my_data_age_T
##membership
my_data_age_T$membership <- ifelse(my_data_age_T$variable=="TruthInPos", "1","2")
my_data_age_T$membership[my_data_age_T$variable=="TruthOutPos"] <- 2
my_data_age_T$membership[my_data_age_T$variable=="TruthInNeg"] <- 1
my_data_age_T$membership[my_data_age_T$variable=="TruthOutNeg"] <- 2
#valence
my_data_age_T$valence <- ifelse(my_data_age_T$variable=="TruthInPos", "1","2")
my_data_age_T$valence[my_data_age_T$variable=="TruthOutPos"] <- 1
my_data_age_T$valence[my_data_age_T$variable=="TruthInNeg"] <- 2
my_data_age_T$valence[my_data_age_T$variable=="TruthOutNeg"] <- 2
# Factor
my_data_age_T$membership<- factor(my_data_age_T$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_age_T$valence<- factor(my_data_age_T$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_age_T$ID<- factor(my_data_age_T$ID)

# Truth: Group membership x Valence Interaction 
Interaction_AT  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                       data=my_data_age_T)
sum_Interaction_AT <- summary(Interaction_AT)

# 1.5Effect size
library(sjstats)
library(car)
omega_sq(Interaction_AT)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large


# 1.6Planned Comparisons of Interaction
library(emmeans)
#Contrasts 1:by membership
Simple.Effects.By.membership<-emmeans(Interaction_AT, ~valence|membership)
test(pairs(Simple.Effects.By.membership), joint = TRUE)
sum_Contrast1_AT<-summary(Contrast1_AT)
#Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Interaction_AT, ~membership|valence)
test(pairs(Simple.Effects.By.valence), joint = TRUE)
sum_Contrast2_AT<-summary(Contrast2_AT)

# 1.7 exploratory analysis: three way interaction
#consistency
Interaction_AT3_C <- aov(value~Consistency*valence*membership+                          
                           Error(ID / (Consistency*valence*membership)),
                         data=my_data_age_T)
sum_Interaction_AT3_C<-summary(Interaction_AT3_C)
#format
Interaction_AT3_F <- aov(value~Format*valence*membership+
                           Error(ID / (Format*valence*membership)),
                         data=my_data_age_T)
sum_Interaction_AT3_F<-summary(Interaction_AT3_F)

#simple effects-consistency
GT.sub_Con_1 <- subset(my_data_age_T, Consistency == 1)
GT.sub_Con_2 <- subset(my_data_age_T, Consistency == 2)
Interaction_AT3_Con_1 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_Con_1)
sum_Interaction_AT3_Con_1<-summary(Interaction_AT3_Con_1)
Interaction_AT3_Con_2 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_Con_2)
sum_Interaction_AT3_Con_2<-summary(Interaction_AT3_Con_2)
#simple effects-format
GT.sub_For_1 <- subset(my_data_age_T, Format == 1)
GT.sub_For_2 <- subset(my_data_age_T, Format == 2)
Interaction_AT3_For_1 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_For_1)
sum_Interaction_AT3_For_1<-summary(Interaction_AT3_For_1)
Interaction_AT3_For_2 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_For_2)
sum_Interaction_AT3_For_2<-summary(Interaction_AT3_For_2)

## 2.Gender_Truth
# 2.1Extract columns
my_data_extracted_G_T1 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","TruthPosGender","TruthNegGender"))
# Paired ttest
#Main effect of valence, prediction: positive > negative 
Ttest_GT<- t.test(my_data_extracted_G_T1$TruthPosGender, my_data_extracted_G_T1$TruthNegGender, paired=TRUE, alternative = "two.sided")
sum_Ttest_GT<- summary(Ttest_GT)

# 2.2Get descriptives
#Stack
library(reshape2)
my_data_gender_T1 <- melt(my_data_extracted_G_T1, id.vars=1:4)
my_data_gender_T1
#valence
my_data_gender_T1$valence <- ifelse(my_data_gender_T1$variable=="TruthPosGender", "1","2")
#describeBy
library(psych) 
Descriptives_GT_TTEST<-describeBy(my_data_gender_T1, 
                                  group = my_data_gender_T1$valence)

# 2.3Truth: Group membership x Valence repeated Interaction 
# Extract columns
my_data_extracted_G_T2 <- subset(my_data_original_1a, select = c("ID", "gender","Consistency","Format","TruthInPos","TruthOutPos", "TruthInNeg", "TruthOutNeg"))
#Stack
library(reshape2)
my_data_gender_T <- melt(my_data_extracted_G_T2, id.vars=1:4)
my_data_gender_T
##membership
my_data_gender_T$membership <- ifelse(my_data_gender_T$variable=="TruthInPos", "1","2")
my_data_gender_T$membership[my_data_gender_T$variable=="TruthOutPos"] <- 2
my_data_gender_T$membership[my_data_gender_T$variable=="TruthInNeg"] <- 1
my_data_gender_T$membership[my_data_gender_T$variable=="TruthOutNeg"] <- 2
#valence
my_data_gender_T$valence <- ifelse(my_data_gender_T$variable=="TruthInPos", "1","2")
my_data_gender_T$valence[my_data_gender_T$variable=="TruthOutPos"] <- 1
my_data_gender_T$valence[my_data_gender_T$variable=="TruthInNeg"] <- 2
my_data_gender_T$valence[my_data_gender_T$variable=="TruthOutNeg"] <- 2
# Factor
my_data_gender_T$membership<- factor(my_data_gender_T$membership,c(1,2),labels = c("Ingroup","Outgroup"))
my_data_gender_T$valence<- factor(my_data_gender_T$valence,c(1,2),labels = c("Postive","Negtive"))
my_data_gender_T$ID<- factor(my_data_gender_T$ID)
# Calculate Interaction 
Interaction_GT  <- aov(value ~ valence*membership+
                         Error(ID / (valence*membership)),
                       data=my_data_gender_T)
sum_Interaction_GT <- summary(Interaction_GT)

# 2.4Planned Comparisons of Interaction
library(emmeans)
#Contrasts 1:by membership
Simple.Effects.By.membership<-emmeans(Interaction_GT, ~valence|membership)
Contrast1_GT <- pairs(Simple.Effects.By.membership,adjust='none')
sum_Contrast1_GT<-summary(Contrast1_GT)
#Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Interaction_GT, ~membership|valence)
Contrast2_GT<-pairs(Simple.Effects.By.valence,adjust='none')
sum_Contrast2_GT<-summary(Contrast2_GT)

## 2.5exploraatory analyis: three way interaction
#consistency
Interaction_GT3_C  <- aov(value ~ Consistency*valence*membership+
                            Error(ID/(Consistency*valence*membership)),
                          data=my_data_gender_T)
sum_Interaction_GT3_C<-summary(Interaction_GT3_C)
#format
Interaction_GT3_F  <- aov(value ~ Format*valence*membership+
                            Error(ID/(Format*valence*membership)),
                          data=my_data_gender_T)
sum_Interaction_GT3_F<-summary(Interaction_GT3_F)

#simple effects under three way interaction
#consistency
GT.sub_Con_1 <- subset(my_data_gender_T, Consistency == 1)
GT.sub_Con_2 <- subset(my_data_gender_T, Consistency == 2)
Interaction_GT3_Con_1 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_Con_1)
sum_Interaction_GT3_Con_1<-summary(Interaction_GT3_Con_1)

Interaction_GT3_Con_2 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_Con_2)
sum_Interaction_GT3_Con_2<-summary(Interaction_GT3_Con_2)
##format
GT.sub_For_1 <- subset(my_data_gender_T, Format == 1)
GT.sub_For_2 <- subset(my_data_gender_T, Format == 2)
Interaction_GT3_For_1 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_For_1)
sum_Interaction_GT3_For_1<-summary(Interaction_GT3_For_1)
Interaction_GT3_For_2 <- aov(value~valence*membership+
                               Error(ID/(valence*membership)),
                             data=GT.sub_For_2)
sum_Interaction_GT3_For_2<-summary(Interaction_GT3_For_2)










