rm(list=ls())

# Set your working dir as the current dir
setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/intern/analysis")

# Read data
library(haven)
my_data_original <- read_sav("Study1_ready_short.sav")
my_data_original <- na.omit(my_data_original)
my_data_original <- my_data_original[my_data_original$gender!=3, ]
summary(my_data_original)

#Truth_gender
# Extract columns
my_data_extracted_G_T <- subset(my_data_original, select = c("ID", "gender", 
                                                         "TruthMenPos", "TruthWomenPos", "TruthMenNeg", "TruthWomenNeg"))
summary(my_data_extracted_G_T)

# Stack columns
library(reshape2)
my_data_gender_T <- melt(my_data_extracted_G_T, id.vars=1:2)
my_data_gender_T

# Defeine factors
str(my_data_gender_T)
#gender
gender<- as.factor(my_data_gender_T$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_T$membership <- ifelse(my_data_gender_T$gender==1 & my_data_gender_T$variable=="TruthMenPos", "1","0")
my_data_gender_T$membership[my_data_gender_T$gender==1 & my_data_gender_T$variable=="TruthMenNeg"] <- 1
my_data_gender_T$membership[my_data_gender_T$gender==2 & my_data_gender_T$variable=="TruthWomenPos"] <- 1
my_data_gender_T$membership[my_data_gender_T$gender==2 & my_data_gender_T$variable=="TruthWomenNeg"] <- 1
my_data_gender_T$membership<- factor(my_data_gender_T$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_T$valence <- ifelse(my_data_gender_T$variable=="TruthMenPos", "1","0")
my_data_gender_T$valence[my_data_gender_T$variable=="TruthWomenPos"] <- 1
my_data_gender_T$valence<- factor(my_data_gender_T$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_T)

# Truth
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_T$value~my_data_gender_T$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_T$value~my_data_gender_T$valence, 
        names=c("Truth_Pos","Truth_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Truth rating", xlab="Valence",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_gender_T)

# Truth: Group membership x Valence Interaction 
Gen_val_interaction1 <- lm(value~valence*membership,data=my_data_gender_T)
summary(Gen_val_interaction1)
#visualize
interaction.plot(x.factor = my_data_gender_T$valence, trace.factor = my_data_gender_T$membership, 
                 response = my_data_gender_T$value, fun = mean, 
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
groups <- group_by(my_data_gender_T, valence, membership)
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

###################################################################
#Acceptability
# Extract columns
my_data_extracted_G_A <- subset(my_data_original, select = c("ID", "gender",
                                                         "AccMenPos", "AccWomenPos", "AccMenNeg", "AccWomenNeg"))
summary(my_data_extracted_G_A)

# Stack columns
library(reshape2)
my_data_gender_A <- melt(my_data_extracted_G_A, id.vars=1:2)
my_data_gender_A

# Defeine factors
str(my_data_gender_A)
#gender
gender<- as.factor(my_data_gender_A$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_A$membership <- ifelse(my_data_gender_A$gender==1 & my_data_gender_A$variable=="AccMenPos", "1","0")
my_data_gender_A$membership[my_data_gender_A$gender==1 & my_data_gender_A$variable=="AccMenNeg"] <- 1
my_data_gender_A$membership[my_data_gender_A$gender==2 & my_data_gender_A$variable=="AccWomenPos"] <- 1
my_data_gender_A$membership[my_data_gender_A$gender==2 & my_data_gender_A$variable=="AccWomenNeg"] <- 1
my_data_gender_A$membership<- factor(my_data_gender_A$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_A$valence <- ifelse(my_data_gender_A$variable=="AccMenPos", "1","0")
my_data_gender_A$valence[my_data_gender_A$variable=="AccWomenPos"] <- 1
my_data_gender_A$valence<- factor(my_data_gender_A$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_A)



# Acceptability_gender
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_A$value~my_data_gender_A$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_A$value~my_data_gender_A$valence, 
        names=c("Truth_Pos","Truth_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Truth rating", xlab="Valence",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_gender_A)

# Acceptability: Group membership x Valence Interaction 
Gen_val_interaction2 <- lm(value~valence*membership,data=my_data_gender_A)
summary(Gen_val_interaction2)
#visualize
interaction.plot(x.factor = my_data_gender_A$valence, trace.factor = my_data_gender_A$membership, 
                 response = my_data_gender_A$value, fun = mean, 
                 trace.label = "Membership",
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Gen_val_interaction_A<-Anova(Gen_val_interaction2)
omega_sq(Gen_val_interaction_A)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#???Contrat:by valence
Simple.Effects.By.valence<-emmeans(Gen_val_interaction2, ~membership|valence)
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
posthoc<-lsmeans(Gen_val_interaction2,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)


###################################################################
#Truth_age
# Extract columns
table(my_data_original$agegroup_subj)
table(my_data_original$agegroup_obj)
##no wrong identification
table(my_data_original$agegroup_subj==1 & my_data_original$agegroup_obj==3)
table(my_data_original$agegroup_subj==3 & my_data_original$agegroup_obj==1)


middle_subj=95/(59+95+29)  #[1] 0.5191257
middle_obj=78/(78+61+44)  #[1] 0.4262295
#25% or more of one or both age groups: so including them but distinguish between 3 rather than 2 age groups (younger, middle-aged, older)

my_data_extracted_A_T <- subset(my_data_original, select = c("ID", "agegroup_subj", 
                                                         "TruthOldPos", "TruthYoungPos", "TruthOldNeg", "TruthYoungNeg"))
summary(my_data_extracted_A_T)

# Stack columns
library(reshape2)
my_data_age_T <- melt(my_data_extracted_A_T, id.vars=1:2)
my_data_age_T

# Defeine factors
str(my_data_age_T)
#Age
age<- as.factor(my_data_age_T$agegroup_subj)
age<- factor(age,c(1,2,3),labels = c("Young","Middle","Old"))
#membership
##ingroup
my_data_age_T$membership <- ifelse(my_data_age_T$agegroup_subj==1 & my_data_age_T$variable=="TruthYoungPos", "1","2")
my_data_age_T$membership[my_data_age_T$agegroup_subj==1 & my_data_age_T$variable=="TruthYoungNeg"] <- 1
my_data_age_T$membership[my_data_age_T$agegroup_subj==3 & my_data_age_T$variable=="TruthOldPos"] <- 1
my_data_age_T$membership[my_data_age_T$agegroup_subj==3 & my_data_age_T$variable=="TruthOldNeg"] <- 1
##outgroup
my_data_age_T$membership[my_data_age_T$agegroup_subj==1 & my_data_age_T$variable=="TruthOldPos"] <- 0
my_data_age_T$membership[my_data_age_T$agegroup_subj==1 & my_data_age_T$variable=="TruthOldNeg"] <- 0
my_data_age_T$membership[my_data_age_T$agegroup_subj==3 & my_data_age_T$variable=="TruthYoungPos"] <- 0
my_data_age_T$membership[my_data_age_T$agegroup_subj==3 & my_data_age_T$variable=="TruthYoungNeg"] <- 0

my_data_age_T$membership<- factor(my_data_age_T$membership,c(1,0,2),labels = c("Ingroup","Outgroup","Middlegroup"))
#valence
my_data_age_T$valence <- ifelse(my_data_age_T$variable=="TruthOldPos", "1","0")
my_data_age_T$valence[my_data_age_T$variable=="TruthYoungPos"] <- 1
my_data_age_T$valence<- factor(my_data_age_T$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_age_T)

# Truth
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_age_T$value~my_data_age_T$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_age_T$value~my_data_age_T$valence, 
        names=c("Truth_Pos","Truth_Neg"), 
        main="Main effect of valence on age-related claims", 
        ylab="Truth rating", xlab="Valence",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_age_T)

# Truth: Group membership x Valence Interaction 
Age_val_interaction1 <- aov(value~membership*valence,data=my_data_age_T)
summary(Age_val_interaction1)
#visualize
interaction.plot(x.factor = my_data_age_T$membership, trace.factor = my_data_age_T$valence,
                 trace.lab = "Valence",
                 response = my_data_age_T$value, fun = mean, 
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Membership", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   


# Effect size
#install.packages("purrr")
library(purrr)
library(sjstats)
library(car)
Age_val_interaction<-Anova(Age_val_interaction1)
omega_sq(Age_val_interaction)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#visualize
library(dplyr)
groups <- group_by(my_data_age_T,membership,valence)
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
Simple.Effects.By.membership<-emmeans(Age_val_interaction1, ~valence|membership)
Simple.Effects.By.membership
pairs(Simple.Effects.By.membership,adjust='none')

Set1 <- list(H1 = c(-1,1))
contrast(Simple.Effects.By.membership,Set1,adjust='none')
test(pairs(Simple.Effects.By.membership), joint = TRUE)
###membership = Middlegroup, not significant

#???Contrast 2:by valence
Simple.Effects.By.valence<-emmeans(Age_val_interaction1, ~membership|valence)
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
posthoc<-lsmeans(Age_val_interaction1,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)


##################################
#Acceptability_Age
# Extract columns
my_data_extracted_A_A <- subset(my_data_original, select = c("ID", "agegroup_subj",
                                                           "AcceptYoungPos", "AcceptOldPos", "AcceptYoungNeg", "AcceptOldNeg"))
summary(my_data_extracted_A_A)

# Stack columns
library(reshape2)
my_data_age_A <- melt(my_data_extracted_A_A, id.vars=1:2)
my_data_age_A

# Defeine factors
str(my_data_age_A)

#Age
age<- as.factor(my_data_age_A$agegroup_subj)
age<- factor(age,c(1,2,3),labels = c("Young","Middle","Old"))
#membership
##ingroup
my_data_age_A$membership <- ifelse(my_data_age_A$agegroup_subj==1 & my_data_age_A$variable=="AcceptYoungPos", "1","2")
my_data_age_A$membership[my_data_age_A$agegroup_subj==1 & my_data_age_A$variable=="AcceptYoungNeg"] <- 1
my_data_age_A$membership[my_data_age_A$agegroup_subj==3 & my_data_age_A$variable=="AcceptOldPos"] <- 1
my_data_age_A$membership[my_data_age_A$agegroup_subj==3 & my_data_age_A$variable=="AcceptOldNeg"] <- 1
##outgroup
my_data_age_A$membership[my_data_age_A$agegroup_subj==1 & my_data_age_A$variable=="AcceptOldPos"] <- 0
my_data_age_A$membership[my_data_age_A$agegroup_subj==1 & my_data_age_A$variable=="AcceptOldNeg"] <- 0
my_data_age_A$membership[my_data_age_A$agegroup_subj==3 & my_data_age_A$variable=="AcceptYoungPos"] <- 0
my_data_age_A$membership[my_data_age_A$agegroup_subj==3 & my_data_age_A$variable=="AcceptYoungNeg"] <- 0

my_data_age_A$membership<- factor(my_data_age_A$membership,c(1,0,2),labels = c("Ingroup","Outgroup","Middlegroup"))
#valence
my_data_age_A$valence <- ifelse(my_data_age_A$variable=="AcceptOldPos", "1","0")
my_data_age_A$valence[my_data_age_A$variable=="AcceptYoungPos"] <- 1
my_data_age_A$valence<- factor(my_data_age_A$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_age_A)

# Truth
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_age_A$value~my_data_age_A$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_age_A$value~my_data_age_A$valence, 
        names=c("Truth_Pos","Truth_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Truth rating", xlab="Valence",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_age_A)

# Truth: Group membership x Valence Interaction 
Age_val_interaction2 <- lm(value~membership*valence,data=my_data_age_A)
summary(Age_val_interaction2)
#visualize
interaction.plot(x.factor = my_data_age_A$membership, trace.factor = my_data_age_A$valence,
                 response = my_data_age_A$value, fun = mean, 
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
groups <- group_by(my_data_age_A,membership,valence)
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










#gender
gender<- as.factor(my_data_gender_A$gender)
gender<- factor(gender,c(1,2),labels = c("Man","Woman"))
#membership
my_data_gender_A$membership <- ifelse(my_data_gender_A$gender==1 & my_data_gender_A$variable=="AccMenPos", "1","0")
my_data_gender_A$membership[my_data_gender_A$gender==1 & my_data_gender_A$variable=="AccMenNeg"] <- 1
my_data_gender_A$membership[my_data_gender_A$gender==2 & my_data_gender_A$variable=="AccWomenPos"] <- 1
my_data_gender_A$membership[my_data_gender_A$gender==2 & my_data_gender_A$variable=="AccWomenNeg"] <- 1
my_data_gender_A$membership<- factor(my_data_gender_A$membership,c(1,0),labels = c("Ingroup","Outgroup"))
#valence
my_data_gender_A$valence <- ifelse(my_data_gender_A$variable=="AccMenPos", "1","0")
my_data_gender_A$valence[my_data_gender_A$variable=="AccWomenPos"] <- 1
my_data_gender_A$valence<- factor(my_data_gender_A$valence,c(1,0),labels = c("Positive","Negative"))
summary(my_data_gender_A)



# Acceptability
#Main effect of valence, prediction: positive > negative 
#Independent 2-group t-test
Gender_valence_ttest<- t.test(my_data_gender_A$value~my_data_gender_A$valence, alternative = "two.sided")
summary(Gender_valence_ttest)
#visualize
boxplot(my_data_gender_A$value~my_data_gender_A$valence, 
        names=c("Truth_Pos","Truth_Neg"), 
        main="Main effect of valence on gender-related claims", 
        ylab="Truth rating", xlab="Valence",col=(c("gold","lightblue")))
#Cohen¡¯s d for two-sample t-test
library(lsr)
cohensD(value~valence,
        data = my_data_gender_A)

# Acceptability: Group membership x Valence Interaction 
Gen_val_interaction2 <- lm(value~valence*membership,data=my_data_gender_A)
summary(Gen_val_interaction2)
#visualize
interaction.plot(x.factor = my_data_gender_A$valence, trace.factor = my_data_gender_A$membership, 
                 response = my_data_gender_A$value, fun = mean, 
                 ylim = c(3,5),
                 legend = TRUE, 
                 xlab = "Valence", ylab="Rating",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))   
# Effect size
library(sjstats)
library(car)
Gen_val_interaction_A<-Anova(Gen_val_interaction2)
omega_sq(Gen_val_interaction_A)
##Field (2013) suggests the following interpretation heuristics:
##Omega Squared = 0 - 0.01: Very small
##Omega Squared = 0.01 - 0.06: Small
##Omega Squared = 0.06 - 0.14: Medium
##Omega Squared > 0.14: Large

#???Contrat:by valence
Simple.Effects.By.valence<-emmeans(Gen_val_interaction2, ~membership|valence)
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
posthoc<-lsmeans(Gen_val_interaction2,
                 pairwise~valence * membership,
                 adjust="tukey")
posthoc
#visualize
plot(posthoc)



