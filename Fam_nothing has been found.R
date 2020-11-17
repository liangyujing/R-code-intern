rm(list=ls())

# Set your working dir as the current dir
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis")

# Read data
library(haven)
my_data_original <- read_sav("Study1_ready_short.sav")
my_data_original <- na.omit(my_data_original)
my_data_original <- my_data_original[my_data_original$gender!=3, ]
summary(my_data_original)

#Fam_gender
# Extract columns
my_data_extracted_G_F <- subset(my_data_original, select = c("ID", "gender", 
                                                             "FamMenPos", "FamWomenPos", "FamMenNeg", "FamWomenNeg"))
summary(my_data_extracted_G_F)

# Stack columns
library(reshape2)
my_data_gender_F <- melt(my_data_extracted_G_F, id.vars=1:2)
my_data_gender_F

# Defeine factors
str(my_data_gender_F)
#gender
gender<- as.factor(my_data_gender_F$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_F$membership <- ifelse(my_data_gender_F$gender==1 & my_data_gender_F$variable=="FamMenPos", "1","0")
my_data_gender_F$membership[my_data_gender_F$gender==1 & my_data_gender_F$variable=="FamMenNeg"] <- 1
my_data_gender_F$membership[my_data_gender_F$gender==2 & my_data_gender_F$variable=="FamWomenPos"] <- 1
my_data_gender_F$membership[my_data_gender_F$gender==2 & my_data_gender_F$variable=="FamWomenNeg"] <- 1
my_data_gender_F$membership<- factor(my_data_gender_F$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_F$valence <- ifelse(my_data_gender_F$variable=="FamMenPos", "1","0")
my_data_gender_F$valence[my_data_gender_F$variable=="FamWomenPos"] <- 1
my_data_gender_F$valence<- factor(my_data_gender_F$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_F)

# Fam
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_F$value~my_data_gender_F$membership, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_F$value~my_data_gender_F$membership, 
        names=c("Fam_Pos","Fam_Neg"), 
        main="Main effect of membership on gender-related claims", 
        ylab="Fam rating", xlab="Membership",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~membership,
        data = my_data_gender_F)

# Fam: Group membership x Valence Interaction 
Gen_val_interaction_F <- lm(value~valence*membership,data=my_data_gender_F)
summary(Gen_val_interaction_F)
#visualize
interaction.plot(x.factor = my_data_gender_F$valence, trace.factor = my_data_gender_F$membership, 
                 response = my_data_gender_F$value, fun = mean, 
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
groups <- group_by(my_data_gender_F, valence, membership)
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
#Fam_Age
# Extract columns
my_data_extracted_A_F <- subset(my_data_original, select = c("ID", "agegroup_subj",
                                                             "FamYoungPos", "FamOldPos", "FamYoungNeg", "FamOldNeg"))
summary(my_data_extracted_A_F)

# Stack columns
library(reshape2)
my_data_age_F <- melt(my_data_extracted_A_F, id.vars=1:2)
my_data_age_F

# Defeine factors
str(my_data_age_F)

#Age
age<- as.factor(my_data_age_F$agegroup_subj)
age<- factor(age,c(1,2,3),labels = c("Young","Middle","Old"))
#membership
##ingroup
my_data_age_F$membership <- ifelse(my_data_age_F$agegroup_subj==1 & my_data_age_F$variable=="FamYoungPos", "1","2")
my_data_age_F$membership[my_data_age_F$agegroup_subj==1 & my_data_age_F$variable=="FamYoungNeg"] <- 1
my_data_age_F$membership[my_data_age_F$agegroup_subj==3 & my_data_age_F$variable=="FamOldPos"] <- 1
my_data_age_F$membership[my_data_age_F$agegroup_subj==3 & my_data_age_F$variable=="FamOldNeg"] <- 1
##outgroup
my_data_age_F$membership[my_data_age_F$agegroup_subj==1 & my_data_age_F$variable=="FamOldPos"] <- 0
my_data_age_F$membership[my_data_age_F$agegroup_subj==1 & my_data_age_F$variable=="FamOldNeg"] <- 0
my_data_age_F$membership[my_data_age_F$agegroup_subj==3 & my_data_age_F$variable=="FamYoungPos"] <- 0
my_data_age_F$membership[my_data_age_F$agegroup_subj==3 & my_data_age_F$variable=="FamYoungNeg"] <- 0

my_data_age_F$membership<- factor(my_data_age_F$membership,c(1,0,2),labels = c("Ingroup","Outgroup","Middlegroup"))
#valence
my_data_age_F$valence <- ifelse(my_data_age_F$variable=="FamOldPos", "1","0")
my_data_age_F$valence[my_data_age_F$variable=="FamYoungPos"] <- 1
my_data_age_F$valence<- factor(my_data_age_F$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_age_F)

# Truth
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_age_F$value~my_data_age_F$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_age_F$value~my_data_age_F$valence, 
        names=c("Fam_Pos","Fam_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Fam rating", xlab="Valence",col=(c("gold","lightblue")))

#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_age_F)

# Truth: Group membership x Valence Interaction 
Age_val_interaction2 <- lm(value~membership*valence,data=my_data_age_F)
summary(Age_val_interaction2)
#visualize
interaction.plot(x.factor = my_data_age_F$membership, trace.factor = my_data_age_F$valence,
                 response = my_data_age_F$value, fun = mean, 
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
groups <- group_by(my_data_age_F,membership,valence)
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


















