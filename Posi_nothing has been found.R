rm(list=ls())

# Set your working dir as the current dir
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis")

# Read data
library(haven)
my_data_original <- read_sav("Study1_ready_short.sav")
my_data_original <- na.omit(my_data_original)
my_data_original <- my_data_original[my_data_original$gender!=3, ]
summary(my_data_original)

#Posi_gender
# Extract columns
my_data_extracted_G_P <- subset(my_data_original, select = c("ID", "gender", "Consistency","Format",
                                                             "PosiMenPos", "PosiWomenPos", "PosiMenNeg", "PosiWomenNeg"))
summary(my_data_extracted_G_P)

# Stack columns
library(reshape2)
my_data_gender_P <- melt(my_data_extracted_G_P, id.vars=1:2)
my_data_gender_P

# Defeine factors
str(my_data_gender_P)
#gender
gender<- as.factor(my_data_gender_P$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_P$membership <- ifelse(my_data_gender_P$gender==1 & my_data_gender_P$variable=="PosiMenPos", "1","0")
my_data_gender_P$membership[my_data_gender_P$gender==1 & my_data_gender_P$variable=="PosiMenNeg"] <- 1
my_data_gender_P$membership[my_data_gender_P$gender==2 & my_data_gender_P$variable=="PosiWomenPos"] <- 1
my_data_gender_P$membership[my_data_gender_P$gender==2 & my_data_gender_P$variable=="PosiWomenNeg"] <- 1
my_data_gender_P$membership<- factor(my_data_gender_P$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_P$valence <- ifelse(my_data_gender_P$variable=="PosiMenPos", "1","0")
my_data_gender_P$valence[my_data_gender_P$variable=="PosiWomenPos"] <- 1
my_data_gender_P$valence<- factor(my_data_gender_P$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_P)

# Posi
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_P$value~my_data_gender_P$membership, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_P$value~my_data_gender_P$membership, 
        names=c("Posi_Pos","Posi_Neg"), 
        main="Main effect of membership on gender-related claims", 
        ylab="Posi rating", xlab="Membership",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~membership,
        data = my_data_gender_P)

# Posi: Group membership x Valence Interaction 
Gen_val_interaction_P <- lm(value~valence*membership,data=my_data_gender_P)
summary(Gen_val_interaction_P)
#visualize
interaction.plot(x.factor = my_data_gender_P$valence, trace.factor = my_data_gender_P$membership, 
                 response = my_data_gender_P$value, fun = mean, 
                 trace.label = "Valence",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Gen_val_interaction<-Anova(Gen_val_interaction1)
omega_sq(Gen_val_interaction)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#visualize
library(dplyr)
groups <- group_by(my_data_gender_P, valence, membership)
plot.data <- summarise(groups,
                       mean = mean(value, na.rm=TRUE),
                       sd = sd(value, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
library(ggplot2)
ggplot(plot.data, aes(x=valence, y=mean, fill = membership)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,7)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=position_dodge(.9)) +
  ggtitle("Mean rating by valence and membership")


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership<-emmeans(Gen_val_interaction1, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)

#???Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Gen_val_interaction1, ~membership|valence)
Simple.Effects.By.valence
pairs(Simple.Effects.By.valence,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.valence,Set1,adjust='none')
test(pairs(Simple.Effects.By.valence), joint = TRUE)


# Post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(Gender_valence_interaction,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)

# Report results
p_plcon_GT <- Gen_val_interaction[, 4] 
fstat_plcon_GT <- Gen_val_interaction[, 3] 
df_plcon_GT <- Gen_val_interaction[, 2]


############################################################
#Posi_Age
# Extract columns
my_data_extracted_A_P <- subset(my_data_original, select = c("ID", "agegroup_subj", "Consistency","Format",
                                                             "PosiYoungPos", "PosiOldPos", "PosiYoungNeg", "PosiOldNeg"))
summary(my_data_extracted_A_P)

# Stack columns
library(reshape2)
my_data_age_P <- melt(my_data_extracted_A_P, id.vars=1:2)
my_data_age_P

# Defeine factors
str(my_data_age_P)

#Age
age<- as.factor(my_data_age_P$agegroup_subj)
age<- factor(age,c(1,2,3),labels = c("Young","Middle","Old"))
#membership
##ingroup
my_data_age_P$membership <- ifelse(my_data_age_P$agegroup_subj==1 & my_data_age_P$variable=="PosiYoungPos", "1","2")
my_data_age_P$membership[my_data_age_P$agegroup_subj==1 & my_data_age_P$variable=="PosiYoungNeg"] <- 1
my_data_age_P$membership[my_data_age_P$agegroup_subj==3 & my_data_age_P$variable=="PosiOldPos"] <- 1
my_data_age_P$membership[my_data_age_P$agegroup_subj==3 & my_data_age_P$variable=="PosiOldNeg"] <- 1
##outgroup
my_data_age_P$membership[my_data_age_P$agegroup_subj==1 & my_data_age_P$variable=="PosiOldPos"] <- 0
my_data_age_P$membership[my_data_age_P$agegroup_subj==1 & my_data_age_P$variable=="PosiOldNeg"] <- 0
my_data_age_P$membership[my_data_age_P$agegroup_subj==3 & my_data_age_P$variable=="PosiYoungPos"] <- 0
my_data_age_P$membership[my_data_age_P$agegroup_subj==3 & my_data_age_P$variable=="PosiYoungNeg"] <- 0

my_data_age_P$membership<- factor(my_data_age_P$membership,c(1,0,2),labels = c("Ingroup","Outgroup","Middlegroup"))
#valence
my_data_age_P$valence <- ifelse(my_data_age_P$variable=="PosiOldPos", "1","0")
my_data_age_P$valence[my_data_age_P$variable=="PosiYoungPos"] <- 1
my_data_age_P$valence<- factor(my_data_age_P$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_age_P)

# Posi
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_age_P$value~my_data_age_P$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_age_P$value~my_data_age_P$valence, 
        names=c("Posi_Pos","Posi_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Posi rating", xlab="Valence",col=(c("gold","lightblue")))

#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_age_P)

# Posi: Group membership x Valence Interaction 
Age_val_interaction2 <- lm(value~membership*valence,data=my_data_age_P)
summary(Age_val_interaction2)
#visualize
interaction.plot(x.factor = my_data_age_P$membership, trace.factor = my_data_age_P$valence,
                 response = my_data_age_P$value, fun = mean, 
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Membership", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Age_val_interaction<-Anova(Age_val_interaction2)
omega_sq(Age_val_interaction)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#visualize
library(dplyr)
groups <- group_by(my_data_age_P,membership,valence)
plot.data <- summarise(groups,
                       mean = mean(value, na.rm=TRUE),
                       sd = sd(value, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
library(ggplot2)
ggplot(plot.data, aes(x=membership, y=mean, fill =valence)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,7)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, position=position_dodge(.9)) +
  ggtitle("Mean rating by valence and membership")


# Planned Comparisons of Interaction
#Simple Effects with 2-Levels
#Note: These can be reported as F-tests (as basically, we are doing one-way ANOVAs) or as t-values.
library(emmeans)
#???Contrasts 1:by membership
Simple.Effects.By.membership<-emmeans(Age_val_interaction2, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)
###membership = Middlegroup, not significant

#???Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Age_val_interaction2, ~membership|valence)
Simple.Effects.By.valence
pairs(Simple.Effects.By.valence,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.valence,Set1,adjust='none')
test(pairs(Simple.Effects.By.valence), joint = TRUE)


# Post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(Age_val_interaction2,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)


















