# SCRIPT WOLF DAILY MOVEMENTS IN HUMAN-DOMINATED LANDSCAPES OF NW-IBERIA
# Author: Iago Ferreiro Arias
# Date: 12-09-2019


# Install specific packages for data analysis
library(lme4)
library(effects)
library(MuMIn)
library(gtools)
library(ggplot2)
library(glmmTMB)
library(sjPlot)
library(boot)
library(performance)
library(bibtex)
library(sjmisc)
library(nortest)
library(dplyr)


# Citation R packages employed:
write.bib(citation("lme4"))
write.bib(citation("MuMIn"))
write.bib(citation("performance"))
write.bib(citation("boot"))


#Uploading databases:

# Raw database of all wolves: NA values are expected in some predictors (landscape factors etc.)
# Wolf_Distances database is a filtered database with the data that we will use for modelling since we were not able to
# adquire values for some predictors of some wolves.

# IMPORTING RAW DATASET: "Base de datos Lobo fusionada". 

# This dataset include raw data. For some predictors, NA values are expected since we could not estimate them
# due to logistic issues
raw_data <- read.csv("wolf_move_raw_data.csv", header = T, sep = ";")
Wolf_Distances <- read.csv("wolf_distances_database.csv", header = T, sep = ";")

str(raw_data) 
str(Wolf_Distances)


#CHECKING NORMALITY ASUMPTION IN DAILY DISTANCES DATA

# Distribution of daily distances in meters
ggplot(data=Wolf_Distances, aes(x=DIS_DIA)) + geom_histogram(fill="#FF4500", col = "#FF4500", alpha=0.6) + theme_classic() + xlab("Daily distances (m)") + ylab("Frecuency")
lillie.test(Wolf_Distances$DIS_DIA) # non-normal distribution

# Distribution of SQRT Transformation 
ggplot(data=Wolf_Distances, aes(x=SQRT_DIS)) + geom_histogram(fill="#FF4500", col = "#FF4500", alpha=0.6) + theme_classic() + xlab("SQRT of Daily distances (m)") + ylab("Frecuency")
lillie.test(Wolf_Distances$SQRT_DIS) # normal distribution

# Turn into factor class some  predictors:
raw_data$COD_C_EDAD <- as.factor(raw_data$COD_C_EDAD) # COD_AGE: 1= Adult  |  2= Subadult  |  3= Pup
raw_data$COD_SEX <- as.factor(raw_data$COD_SEX) # COD_SEX: 1= Male  | 2= Female 
raw_data$COD_STATUS <- as.factor(raw_data$COD_STATUS) # COD_STATUS: 1= Pack  |  2= Non-pack


# Transformation into factor class:

Wolf_Distances$COD_SEXO <- as.factor(Wolf_Distances$COD_SEXO)
Wolf_Distances$COD_EDAD <- as.factor(Wolf_Distances$COD_EDAD)
Wolf_Distances$COD_CRIA <- as.factor(Wolf_Distances$COD_CRIA)
Wolf_Distances$COD_STATUS <- as.factor(Wolf_Distances$COD_STATUS)
Wolf_Distances$ID_LOBO <- as.factor(Wolf_Distances$ID_LOBO)

str(as.factor(Wolf_Distances$SEXO))

# New variables: sex, age, status and breeding period
Wolf_Distances$Sex <- replace(as.character(Wolf_Distances$COD_SEXO),Wolf_Distances$COD_SEXO=="1","Male")
Wolf_Distances$Sex <- replace(as.character(Wolf_Distances$Sex),Wolf_Distances$COD_SEXO=="2","Female")
Wolf_Distances$Sex <-as.factor(Wolf_Distances$Sex)
str(Wolf_Distances$Sex)


Wolf_Distances$Age <- replace(as.character(Wolf_Distances$COD_EDAD), Wolf_Distances$COD_EDAD=="1", "Adult")
Wolf_Distances$Age <- replace(as.character(Wolf_Distances$Age), Wolf_Distances$COD_EDAD=="2", "Subadult")
Wolf_Distances$Age <- as.factor(Wolf_Distances$Age)
str(Wolf_Distances$Age)

Wolf_Distances$Status <- replace(as.character(Wolf_Distances$COD_STATUS), Wolf_Distances$COD_STATUS=="1", "Pack")
Wolf_Distances$Status <- replace(as.character(Wolf_Distances$Status), Wolf_Distances$COD_STATUS=="2", "NoPack")
Wolf_Distances$Status <- as.factor(Wolf_Distances$Status)
str(Wolf_Distances$Status)

Wolf_Distances$Breeding_period <- replace(as.character(Wolf_Distances$COD_CRIA), Wolf_Distances$COD_CRIA=="1", "May_Dec")
Wolf_Distances$Breeding_period <- replace(as.character(Wolf_Distances$Breeding_period), Wolf_Distances$COD_CRIA=="2", "Jan_April")
Wolf_Distances$Breeding_period <- as.factor(Wolf_Distances$Breeding_period)
str(Wolf_Distances$Breeding_period)

# NOTE: All wolf pups included in this study are male and belong to a pack, so it has no sense include them in the test
# and only check what are the mean values and its differences concerning the other age classes.

# We set "adult_data" as a subset from raw data but excluding wolf pups
adult_data <-subset(Base_de_datos_Lobo_fusionada, COD_C_EDAD < 3)


mean(adult_data$Dis_dia) # mean all wolves
sd(adult_data$Dis_dia) # sd all wolves
min(adult_data$Dis_dia) # min value of all wolves
max(adult_data$Dis_dia) # max value of all wolves

# Mean, sd and CI of ALL WOLVES grouped by sex:
aggregate(data=adult_data, Dis_dia ~ COD_SEX, FUN=mean)
aggregate(data=adult_data, Dis_dia ~ COD_SEX, FUN=sd)
tapply(adult_data$Dis_dia, adult_data$COD_SEX, t.test)

# Mean, sd and CI ALL WOLVES grouped by status:
aggregate(data=adult_data, Dis_dia ~ COD_STATUS, FUN=mean)
aggregate(data=adult_data, Dis_dia ~ COD_STATUS, FUN=sd)
tapply(adult_data$Dis_dia, adult_data$COD_STATUS, t.test)

# Mean, sd and CI of ONLY ADULT INDIVIDUALS grouped by sex:
aggregate(data=adult_wolves, Dis_dia ~ COD_SEX, FUN=mean)
aggregate(data=adult_wolves, Dis_dia ~ COD_SEX, FUN=sd)
tapply(adult_wolves$Dis_dia, adult_data$COD_SEX, t.test)

# Mean, sd and CI of ONLY ADULT INDIVIDUALS grouped by status:
aggregate(data=adult_wolves, Dis_dia ~ COD_STATUS, FUN=mean)
aggregate(data=adult_wolves, Dis_dia ~ COD_STATUS, FUN=sd)
tapply(adult_wolves$Dis_dia, adult_data$COD_STATUS, t.test)

# Mean, sd and CI of ONLY SUB-ADULT INDIVIDUALS grouped by sex:
aggregate(data=subadult_wolves, Dis_dia ~ COD_SEX, FUN=mean)
aggregate(data=subadult_wolves, Dis_dia ~ COD_SEX, FUN=sd)
tapply(subadult_wolves$Dis_dia, adult_data$COD_SEX, t.test)

# Mean, sd and CI of ONLY SUB-ADULT INDIVIDUALS grouped by status:
aggregate(data=subadult_wolves, Dis_dia ~ COD_STATUS, FUN=mean)
aggregate(data=subadult_wolves, Dis_dia ~ COD_STATUS, FUN=sd)
tapply(subadult_wolves$Dis_dia, adult_data$COD_STATUS, t.test)


# SUPPORTING INFORMATION: values per each wolf
tapply(Base_de_datos_Lobo_fusionada$Dis_dia, Base_de_datos_Lobo_fusionada$ID_LOBO, summary)  # Mean, Min and Max values
tapply(Base_de_datos_Lobo_fusionada$Dis_dia, Base_de_datos_Lobo_fusionada$ID_LOBO, sd) # SD values
tapply(Base_de_datos_Lobo_fusionada$Dis_dia, Base_de_datos_Lobo_fusionada$ID_LOBO, t.test) # 95% CI values


# TESTING DIFFERENCES OF DAILY DISTANCES GROUPING THEM BY AGE, SEX AND STATUS CLASSES:

# between males and females (all wolves considered)
t.test(SqrtDis_dia ~ COD_SEX, data=adult_data, conf.level= 0.99)

# between pack and non-pack members (all wolves considered)
t.test(SqrtDis_dia ~ COD_STATUS, data=adult_data, conf.level= 0.99)

# between adult and subadults wolves
t.test(SqrtDis_dia ~ COD_C_EDAD, data=adult_data, conf.level= 0.99)

# between subadult and pup wolves
young_data <-subset(raw_data, COD_C_EDAD != 1)
t.test(SqrtDis_dia ~ COD_C_EDAD, data=young_data, conf.level= 0.99)

# between adults and pups wolves
no_juvenile_data <- subset(raw_data, COD_C_EDAD != 2)
t.test(SqrtDis_dia ~ COD_C_EDAD, data=no_juvenile_data , conf.level= 0.99)

# between males and females within adult wolves?
adult_wolves <- subset(raw_data, COD_C_EDAD == 1) 
t.test(SqrtDis_dia ~ COD_SEX, data=adult_wolves , conf.level= 0.99)

# between pack and non-pack member within adult wolves?
t.test(SqrtDis_dia ~ COD_STATUS, data=adult_wolves , conf.level= 0.99)

# between males and females within sub-adult wolves?
subadult_wolves <- subset(raw_data, COD_C_EDAD == 2)
t.test(SqrtDis_dia ~ COD_SEX, data=subadult_wolves , conf.level= 0.99)

# between pack and non-pack members within sub-adult wolves?
t.test(SqrtDis_dia ~ COD_STATUS, data=subadult_wolves , conf.level= 0.99)

# EXPLORATORY DATA ANALYSIS: DAILY DISTANCES THROUGHOUT THE YEAR AND BREEDING PERIODS BOXPLOTS 


ggplot(adult_data, aes(y=Dis_dia_km, x= MONTH)) + labs(y="Daily distance (km)") + geom_boxplot()


Boxplot_Breeding <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period) + labs(y= "Sqrt(Daily Distances) in m" )
Boxplot_Breeding

Boxplot_Breeding_Sex <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period + Sex) + labs(y= "Sqrt(Daily Distances) in m" )
Boxplot_Breeding_Sex

Boxplot_Breeding_Age <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period + Age) + labs(y= "Sqrt(Daily Distances) in m" )
Boxplot_Breeding_Age

Boxplot_Breeding_Status <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period + Status) + labs(y= "Sqrt(Daily Distances) in m" ) + theme_classic()
Boxplot_Breeding_Status

Boxplot_Sex_Age <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Sex )) + geom_boxplot(notch = T) +
  facet_grid(.~Sex + Age) + labs(y= "Sqrt(Daily Distances) in m" ) + theme_classic()
Boxplot_Sex_Age

Boxplot_Breeding_Status_Sex <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period + Status + Sex) + labs(y= "Sqrt(Daily Distances) in m" ) 
Boxplot_Breeding_Status_Sex

Boxplot_Breeding_Status_Sex_Age <- ggplot(data=Wolf_Distances, aes(y=SQRT_DIS,fill=Breeding_period )) + geom_boxplot(notch = T) +
  facet_grid(.~Breeding_period + Status + Sex + Age) + labs(y= "Sqrt(Daily Distances) in m" ) 
Boxplot_Breeding_Status_Sex_Age

# IS THERE ANY DIFFERENCES IN DAILY DISTANCES IN FUNCTION OF THE PERIOD OF THE YEAR?
breeding_period <- subset(adult_data, COD_CRIA == 1)
non_breeding_period <- subset(adult_data, COD_CRIA == 2)

tapply(breeding_period$Dis_dia_km, breeding_period$STATUS, mean)
tapply(breeding_period$Dis_dia_km, breeding_period$STATUS, sd)
tapply(non_breeding_period$Dis_dia_km, non_breeding_period$STATUS, mean)
tapply(non_breeding_period$Dis_dia_km, non_breeding_period$STATUS, sd)

PACK_MEMBERS <- subset(adult_data, COD_STATUS == 1)
SOLITARY_WOLF <- subset(adult_data, COD_STATUS ==2)

t.test(SqrtDis_dia ~ COD_STATUS, data=breeding_period , conf.level= 0.99)
t.test(SqrtDis_dia ~ COD_STATUS, data=non_breeding_period , conf.level= 0.99)
t.test(SqrtDis_dia ~ COD_CRIA, data=PACK_MEMBERS , conf.level= 0.99)
t.test(SqrtDis_dia ~ COD_CRIA, data=SOLITARY_WOLF , conf.level= 0.99)







 
#----------------------------------- LINEAR MIXED-EFFECT MODELLING ----------------------------------------#

#NEW VARIABLES: Standardization of variables based on the individual's home range

Wolf_Distances$Road_density <- Wolf_Distances$CARRETERAS/Wolf_Distances$HOME_RANGE
Wolf_Distances$Settlement_density <- Wolf_Distances$NUCLEOS/Wolf_Distances$HOME_RANGE
Wolf_Distances$Refuge_perc <- Wolf_Distances$REFUGIO/Wolf_Distances$HOME_RANGE
Wolf_Distances$Prey_Availability <- Wolf_Distances$GANADO/Wolf_Distances$HOME_RANGE

 # Random effect: ID_WOLF (wolf identity)
 # Fixed effects: we will consider a set of different fixed effects that can be grouped into three categories:
 #               - Intrinsec factor such as sex or age
 #               - Behavioural factors such as breeding period and status
 #               - Landscape factors within individual home range such as road density, % of refuge....
 # Different combinations of sex and age, breeding period and satus and human activity are expected to have different
 # effects so we will include the interactions of those predictors in the set of models
 
 
# 1st Model: All variables and its interactions

Modelo1 <- lmer(SQRT_DIS ~ Sex * Age + Status + Breeding_period + Prey_Availability + Refuge_perc + Road_density*Settlement_density +
                  (1|ID_LOBO), data = Wolf_Distances)             

summary(Modelo1)
AIC(Modelo1)
plot(allEffects(Modelo1))

# 2nd model: WITHOUT PREY AVAILABILITY

Modelo2 <- lmer(SQRT_DIS ~ Sex * Age + Status + Breeding_period + Refuge_perc + Settlement_density * Road_density +
                  (1|ID_LOBO), data = Wolf_Distances)

AIC(Modelo1, Modelo2) # AIC value get lower when prey availability is not included
plot(allEffects(Modelo2))

# 3rd : equal to model 2 but without interaction of human activity (settlement density:road density)
Modelo3 <- lmer(SQRT_DIS ~ Sex * Age + Status + Breeding_period + Refuge_perc + Settlement_density  + Road_density +
                  (1|ID_LOBO), data = Wolf_Distances)

AIC(Modelo1, Modelo2, Modelo3)
# AIC value increases so human activity within home range individual could be relevant

# 4th Model: 

Modelo4 <- lmer(SQRT_DIS ~ Sex * Age + Status * Breeding_period + Prey_Availability + Refuge_perc + Settlement_density*Road_density +
                  (1|ID_LOBO), data = Wolf_Distances)            
AIC(Modelo1, Modelo2, Modelo3, Modelo4)
summary(Modelo4)
plot(allEffects(Modelo4))

# 5th model: interactions of factors + refuge
Modelo5 <- lmer(SQRT_DIS ~ Sex * Age + Status * Breeding_period + Refuge_perc + Settlement_density*Road_density +
                  (1|ID_LOBO), data = Wolf_Distances)   
summary(Modelo5)
plot(allEffects(Modelo5))


#Comparing AIC values: 
AIC(Modelo1, Modelo2, Modelo3, Modelo4, Modelo5)
delta_aic <- c(AIC(Modelo5)- AIC(Modelo5),AIC(Modelo4)- AIC(Modelo5), AIC(Modelo2)- AIC(Modelo5), AIC(Modelo1)- AIC(Modelo5),
               AIC(Modelo3)- AIC(Modelo5))
delta_aic
summary(Modelo5)
plot(Modelo5)
hist(residuals((Modelo5)))
qqnorm(residuals(Modelo5))

efectos<-allEffects(Modelo5)
plot(efectos$`Status:Breeding_period`)
plot(efectos$`Settlement_density:Road_density`)
plot(efectos)

# CALCULATE AKAIKE WEIGHTS:
AkaikeWeights <- Weights(AIC(Modelo1, Modelo2, Modelo3, Modelo4,Modelo5))
AkaikeWeights

# 5th model seems to be the best of them based on differences in AIC and Akaike weights values
# WE SET MODEL 5 AS THE BEST CANDIDATE MODEL

# PLOTTING FIXED AND RANDOM EFFECTS FROM BEST CANDIDATE MODEL
?plot_model
fixedeffects_plot <-plot_model(Modelo5, axis.title = "Estimates", title= "Square Root of Wolf Daily Distances (in meters)",
                               axis.labels= c("Settlement density : Road density", "Status (Pack) : Breeding period (May-Dec)",
                                              "Sex (Male) : Age (Subadult)", "Road density", "Settlement density", "Refuge",
                                              "Breeding period (May-Dec)","Status (Pack)","Age (Subadult)", "Sex (Male)"),
                               dot.size = 3, line.size = 1.)
fixedeffects_plot

randomeffects_plot <-plot_model(Modelo5, type ="re")

randomeffects_plot 

# BOOTSTRAPING FOR CONFIDENCE INTERVALS OF FIXED EFFECTS IN BEST CANDIDATE MODEL
# bootstraping candidate model: 5000 replicates
# we will considerate reliable effects those which have not zero within the confidence interval

bootstrap_modelo5 <- bootMer(Modelo5, FUN = fixef, nsim=5000, verbose=T)

# CONFIDENCE INTERVALS: INTERCEPT
boot.ci(bootstrap_modelo5, index = 1, type = "perc")

# CONFIDENCE INTERVALS: SEX
boot.ci(bootstrap_modelo5, index = 2, type = "perc")

# CONFIDENCE INTERVALS: AGE
boot.ci(bootstrap_modelo5, index = 3, type = "perc")

# CONFIDENCE INTERVALS: STATUS
boot.ci(bootstrap_modelo5, index = 4, type = "perc")

# CONFIDENCE INTERVALS: BREEDING PERIOD
boot.ci(bootstrap_modelo5, index = 5, type = "perc")

# CONFIDENCE INTERVALS: REFUGE
boot.ci(bootstrap_modelo5, index = 6, type = "perc")

# CONFIDENCE INTERVALS: HUMAN SETTLEMENT DENSITY
boot.ci(bootstrap_modelo5, index = 7, type = "perc")

# CONFIDENCE INTERVALS: ROAD DENSITY
boot.ci(bootstrap_modelo5, index = 8, type = "perc")

# CONFIDENCE INTERVALS: SEX:AGE INTERACTION
boot.ci(bootstrap_modelo5, index = 9, type = "perc")

# CONFIDENCE INTERVALS: BREEDING PERIOD:STATUS INTERACTION
boot.ci(bootstrap_modelo5, index = 10, type = "perc")

# CONFIDENCE INTERVALS: ROAD DENSITY:HUMAN SETTLEMENT DENSITY INTERACTION
boot.ci(bootstrap_modelo5, index = 11, type = "perc")




# ESTIMATING R2 FOR BEST CANDIDATE MODEL FOLLOWING NAKAGAWA & SCHIELZETH (2013)
# Conditional R2: variance explained by both fixed and random predictors
# Marginal R2: variance explained by fixed predictors

r2_nakagawa(Modelo5)

# Conditional R2 >>>>> Marginal R2
# It seems that individual behaviour of wolf (WOLF_ID) explain more daily movement variance than
# fixed effects considered in the models.


# CHECKING MODEL ASUMPTIONS: RESIDUAL ANALYSIS

Residuals <- residuals(Modelo5)
Fitted <- fitted(Modelo5)
par(mfrow=c(2,5))
plot(Residuals ~ Fitted, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Residuals ~ Wolf_Distances$Sex, xlab="Sex", ylab="Residuals", main = "SEX")
plot(Residuals ~ Wolf_Distances$Age, xlab="Age", ylab="Residuals", main = "AGE")
plot(Residuals ~ Wolf_Distances$Status, xlab="Status", ylab="Residuals", main = "STATUS")
plot(Residuals ~ Wolf_Distances$Breeding_period, xlab="Breeding period", ylab="Residuals", main = "BREEDING PERIOD")
plot(Residuals ~ Wolf_Distances$Refuge_perc, xlab="Refuge", ylab="Residuals", main = "REFUGE")
abline(h=0)
plot(Residuals ~ Wolf_Distances$Road_density, xlab="Road density", ylab="Residuals", main = "ROAD DENSITY")
abline(h=0)
plot(Residuals ~ Wolf_Distances$Settlement_density, xlab="Human settlement density", ylab="Residuals", main = "HUMAN SETTLEMENT")
abline(h=0)
hist(Residuals, main="Histogram of residuals", xlab="Residuals")
qqnorm(Residuals)
qqline(Residuals)

shapiro.test(Residuals)


