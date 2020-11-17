rm(list=ls())

# Set your working dir as the current dir
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis")

# Read data
library(haven)
my_data_original <- read_sav("Study1_ready_short.sav")
my_data_original <- na.omit(my_data_original)
my_data_original <- my_data_original[my_data_original$gender!=3, ]
summary(my_data_original)

#Ster_gender
# Extract columns
my_data_extracted_G_F <- subset(my_data_original, select = c("ID", "gender", "Consistency","Format",
                                                             "SterMenPos", "SterWomenPos", "SterMenNeg", "SterWomenNeg"))
summary(my_data_extracted_G_F)

# Stack columns
library(reshape2)
my_data_gender_S <- melt(my_data_extracted_G_F, id.vars=1:4)
my_data_gender_S

# Defeine factors
str(my_data_gender_S)
#gender
gender<- as.factor(my_data_gender_S$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_S$membership <- ifelse(my_data_gender_S$gender==1 & my_data_gender_S$variable=="SterMenPos", "1","0")
my_data_gender_S$membership[my_data_gender_S$gender==1 & my_data_gender_S$variable=="SterMenNeg"] <- 1
my_data_gender_S$membership[my_data_gender_S$gender==2 & my_data_gender_S$variable=="SterWomenPos"] <- 1
my_data_gender_S$membership[my_data_gender_S$gender==2 & my_data_gender_S$variable=="SterWomenNeg"] <- 1
my_data_gender_S$membership<- factor(my_data_gender_S$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_S$valence <- ifelse(my_data_gender_S$variable=="SterMenPos", "1","0")
my_data_gender_S$valence[my_data_gender_S$variable=="SterWomenPos"] <- 1
my_data_gender_S$valence<- factor(my_data_gender_S$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_S)

# Ster
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_S$value~my_data_gender_S$membership, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_S$value~my_data_gender_S$membership, 
        names=c("Ster_Pos","Ster_Neg"), 
        main="Main effect of membership on gender-related claims", 
        ylab="Ster rating", xlab="Membership",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~membership,
        data = my_data_gender_S)

# Ster: Group membership x Valence Interaction 
Interaction_gender_S1 <- lm(value~valence*membership,data=my_data_gender_S)
summary(Interaction_gender_S1)
#visualize
interaction.plot(x.factor = my_data_gender_S$valence, trace.factor = my_data_gender_S$membership, 
                 response = my_data_gender_S$value, fun = mean, 
                 trace.label = "Valence",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Interaction_gender_S<-Anova(Interaction_gender_S1)
omega_sq(Interaction_gender_S)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#visualize
library(dplyr)
groups <- group_by(my_data_gender_S, valence, membership)
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
Simple.Effects.By.membership<-emmeans(Interaction_gender_S1, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)

#???Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Interaction_gender_S1, ~membership|valence)
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
posthoc<-lsmeans(Interaction_gender_S1,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)

##3 way
Gen_val_interaction1_GS <- lm(value~consistency*valence*membership,data=my_data_gender_S)
summary(Gen_val_interaction1_GS)

#visualize
ggplot(data = my_data_gender_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(aes(group=membership))


############################################################
#Ster_Age
# Extract columns
my_data_extracted_A_F <- subset(my_data_original, select = c("ID", "agegroup_subj","Consistency", "Format",
                                                             "SterYoungPos", "SterOldPos", "SterYoungNeg", "SterOldNeg"))
summary(my_data_extracted_A_F)

# Stack columns
library(reshape2)
my_data_age_S <- melt(my_data_extracted_A_F, id.vars=1:4)
my_data_age_S

# Defeine factors
str(my_data_age_S)

#Age
age<- as.factor(my_data_age_S$agegroup_subj)
age<- factor(age,c(1,2,3),labels = c("Young","Middle","Old"))
#membership
##ingroup
my_data_age_S$membership <- ifelse(my_data_age_S$agegroup_subj==1 & my_data_age_S$variable=="SterYoungPos", "1","2")
my_data_age_S$membership[my_data_age_S$agegroup_subj==1 & my_data_age_S$variable=="SterYoungNeg"] <- 1
my_data_age_S$membership[my_data_age_S$agegroup_subj==3 & my_data_age_S$variable=="SterOldPos"] <- 1
my_data_age_S$membership[my_data_age_S$agegroup_subj==3 & my_data_age_S$variable=="SterOldNeg"] <- 1
##outgroup
my_data_age_S$membership[my_data_age_S$agegroup_subj==1 & my_data_age_S$variable=="SterOldPos"] <- 0
my_data_age_S$membership[my_data_age_S$agegroup_subj==1 & my_data_age_S$variable=="SterOldNeg"] <- 0
my_data_age_S$membership[my_data_age_S$agegroup_subj==3 & my_data_age_S$variable=="SterYoungPos"] <- 0
my_data_age_S$membership[my_data_age_S$agegroup_subj==3 & my_data_age_S$variable=="SterYoungNeg"] <- 0

my_data_age_S$membership<- factor(my_data_age_S$membership,c(1,0,2),labels = c("Ingroup","Outgroup","Middlegroup"))
#valence
my_data_age_S$valence <- ifelse(my_data_age_S$variable=="SterOldPos", "1","0")
my_data_age_S$valence[my_data_age_S$variable=="SterYoungPos"] <- 1
my_data_age_S$valence<- factor(my_data_age_S$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_age_S)

# Ster
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_age_S$value~my_data_age_S$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_age_S$value~my_data_age_S$valence, 
        names=c("Ster_Pos","Ster_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Ster rating", xlab="Valence",col=(c("gold","lightblue")))

#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_age_S)

# Ster: Group membership x Valence Interaction 
Interaction_age_S1 <- lm(value~membership*valence,data=my_data_age_S)
summary(Interaction_age_S1)
#visualize
interaction.plot(x.factor = my_data_age_S$membership, trace.factor = my_data_age_S$valence,
                 response = my_data_age_S$value, fun = mean, 
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Membership", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Interaction_age_S<-Anova(Interaction_age_S1)
omega_sq(Interaction_age_S)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#visualize
library(dplyr)
groups <- group_by(Interaction_age_S1,membership,valence)
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
Simple.Effects.By.membership<-emmeans(Interaction_age_S1, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)
###membership = Middlegroup, not significant

#???Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Interaction_age_S1, ~membership|valence)
Simple.Effects.By.valence
pairs(Simple.Effects.By.valence,adjust='none')

Set1 <- list(H1 = c(-1,0,1))
contrast(Simple.Effects.By.valence,Set1,adjust='none')
test(pairs(Simple.Effects.By.valence), joint = TRUE)


# Post-hoc test
#install.packages("lsmeans")
#install.packages("multcompView")
library("lsmeans")
library("multcompView")
posthoc<-lsmeans(Interaction_age_S1,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)

##3 way
Gen_val_interaction1_AS <- lm(value~consistency*valence*membership,data=my_data_age_S)
summary(Gen_val_interaction1_AS)

#visualize
ggplot(data = my_data_age_S, mapping = aes(x = valence, y = value, 
                                              color = membership)) + 
  facet_grid(.~ Consistency) +
  geom_jitter() + 
  geom_smooth(aes(group=membership))

















