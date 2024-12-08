### Associations between Morning and Afternoon MVPA

#Libraries
library(readxl) #Read excel data
library(tidyverse) #Necessary for data wrangling, figures and diagnostic plots
library(broom) #Important for diagnostic plots
library(ppcor) #Partial correlation needs this package
library(GGally) #Not necessary, but creates cool graphs of correlation
library(naniar) #Replacing codes of missing data to NA
library(psych) # to use the function "with" to do the descriptive statistics
library(car) # Testing if the coefficients are different using linear hypothesis test
library(caret) #To produce non-linear regressions
library(lmtest) #Breusch-Pagan test
library(RVAideMemoire) # Necessary to do the shapiro test
library(psychometric) #provides confidence intervals for R2
library(ggpubr) #Create plots to check normaility
library(rstatix) #To use the function "add_significance"
library(quantreg) # Model quantile regression


#Set working directory

setwd("G:/Meu Drive/ARTIGOS PUBLICADOS E A SEREM PUBLICADOS/Artigos em andamento e parados/Artigo associações timing da AF idosos/Documentos e análises/Data/R/Data")

#Creating first object of the data

data1 <- readxl::read_excel(
  "Database-Patterns - Final - Clean Older adults.xlsx", sheet = 1)

################################################################################
################################################################################
################################################################################


# OLDER ADULTS DATA


OlderData <- data1 |>  mutate(TotalST = `8_MeanST`+`9_MeanST`+`10_MeanST`+`11_MeanST` +
                              `12_MeanST`+`13_MeanST`+`14_MeanST`+`15_MeanST`+`16_MeanST` +
                              `17_MeanST`+`18_MeanST`+`19_MeanST`,
                              MorningST = `8_MeanST`+`9_MeanST`+`10_MeanST`+`11_MeanST` +
                              `12_MeanST`,
                              AfternoonST = `13_MeanST`+`14_MeanST`+`15_MeanST`+`16_MeanST` +
                              `17_MeanST`+`18_MeanST`+`19_MeanST`,
                              TotalLPA = `8_MeanLPA`+`9_MeanLPA`+`10_MeanLPA`+`11_MeanLPA` +
                              `12_MeanLPA`+`13_MeanLPA`+`14_MeanLPA`+`15_MeanLPA`+`16_MeanLPA`+
                              `17_MeanLPA`+`18_MeanLPA`+`19_MeanLPA`,
                              MorningLPA = `8_MeanLPA`+`9_MeanLPA`+`10_MeanLPA`+`11_MeanLPA`+
                              `12_MeanLPA`,
                              AfternoonLPA = `13_MeanLPA`+`14_MeanLPA`+`15_MeanLPA`+`16_MeanLPA`+
                              `17_MeanLPA`+`18_MeanLPA`+`19_MeanLPA`) %>% 
  dplyr::select(ID, Age, Sex, Weight, Height, BMI,
                                     HandgripMean, ChairStand30s, ArmCurl,
                                     EightFtUpAndGo, SixMinWalk, TotalMorningMVPA,
                                     TotalAfternoonMVPA, TotalMVPA, TotalMorningRelativeMVPA,
                                     TotalAfternoonRelativeMVPA, TotalRelativeMVPA,
                                     TotalWEAR, TotalMorningWEAR, TotalAfternoonWEAR,
                TotalST, MorningST, AfternoonST, TotalLPA, MorningLPA, AfternoonLPA) %>% 
  replace_with_na(replace = list (HandgripMean = 999999,
                                  ChairStand30s = 999999,
                                  ArmCurl = 999999,
                                  EightFtUpAndGo = 999999,
                                  SixMinWalk = 999999,
                                  TotalMorningMVPA = 999999,
                                  TotalAfternoonMVPA = 999999,
                                  TotalMVPA = 999999,
                                  TotalMorningRelativeMVPA = 999999,
                                  TotalAfternoonRelativeMVPA = 999999,
                                  TotalRelativeMVPA = 999999,
                                  Weight = 999999,
                                  Height = 999999,
                                  BMI = 999999,
                                  TotalWEAR = 999999, 
                                  TotalMorningWEAR = 999999, 
                                  TotalAfternoonWEAR = 999999,
                                  TotalST = 999999,
                                  MorningST = 999999,
                                  AfternoonST = 999999,
                                  TotalLPA = 999999,
                                  MorningLPA = 999999,
                                  AfternoonLPA = 999999)) %>% 
  dplyr::mutate(RelMorningMVPA = TotalMorningMVPA/TotalMVPA*100,
                RelAfternoonMVPA = TotalAfternoonMVPA/TotalMVPA*100)

OlderData$Sex <- factor(OlderData$Sex, levels = c(0:1), labels = c("Female", "Male"))
OlderData$ID <- factor(OlderData$ID)


#Excluding outliers ####


OlderData <- OlderData %>% 
  mutate(EightFtUpAndGo = ifelse(EightFtUpAndGo > 40, NA, EightFtUpAndGo)) 


summary(OlderData)



#Checking for assumptions of normality and homogeneity of variances ####

#Variables with normality problems:Total MVPA, Total morning MVPA, 
#Total afternoon MVPA, and EightFtUpAndGo

ggqqplot(OlderData, x = "EightFtUpAndGo", facet.by = "Sex")


# Descriptive statistics ####


OlderData %>%
  group_by(Sex) %>% 
  summarise(Age = format(mean(Age), digits = 3),
            Weight = format(mean(Weight), digits = 3),
            Height = format(mean(Height), digits = 4),
            BMI = format(mean(BMI), digits = 3))

OlderData %>%
  group_by(Sex) %>% 
  summarise(Age_sd = format(sd(Age), digits = 3),
            Weight_sd = format(sd(Weight), digits = 3),
            Height_sd = format(sd(Height), digits = 2),
            BMI_sd = format(sd(BMI), digits = 2))


OlderData %>%
  group_by(Sex) %>% 
  summarise(TotalST = format(mean(TotalST, na.rm = T), digits = 4),
            TotalLPA = format(mean(TotalLPA, na.rm = T), digits = 4),
            TotalMVPA = format(median(TotalMVPA), digits = 3),
            MorningMVPA = format(median(TotalMorningMVPA), digits = 3),
            AfternoonMVPA = format(median(TotalAfternoonMVPA), digits = 3))

OlderData %>%
  group_by(Sex) %>% 
  summarise(TotalST = format(sd(TotalST, na.rm = T), digits = 3),
            TotalLPA = format(sd(TotalLPA, na.rm = T), digits = 3),
            TotalMVPA = format(IQR(TotalMVPA), digits = 3),
            MorningMVPA = format(IQR(TotalMorningMVPA), digits = 3),
            AfternoonMVPA = format(IQR(TotalAfternoonMVPA), digits = 3))



OlderData  |> 
  dplyr::group_by(Sex) |> 
  summarise(EightFtUpAndGo = format(median(EightFtUpAndGo, na.rm = T),digits = 4),
            ChairStand30s = format(mean(ChairStand30s, na.rm = T),digits = 4),
            ArmCurl = format(mean(ArmCurl, na.rm = T), digits = 4),
            HandgripMean = format(mean(HandgripMean, na.rm = T), digits = 4),
            SixMinWalk = format(mean(SixMinWalk, na.rm = T), digits = 4))


OlderData %>%
  summarise(EightFtUpAndGo = format(IQR(EightFtUpAndGo, na.rm = T),digits = 3),
            ChairStand30s = format(sd(ChairStand30s, na.rm = T),digits = 2),
            ArmCurl = format(sd(ArmCurl, na.rm = T), digits = 2),
            HandgripMean = format(sd(HandgripMean, na.rm = T), digits = 2),
            SixMinWalk = format(sd(SixMinWalk, na.rm = T), digits = 4))




#Testing differences between genders in all parameters ####

#Demographics
OlderData %>% t_test(Age ~ Sex) %>% add_significance()
OlderData %>% t_test(Weight ~ Sex) %>% add_significance()
OlderData %>% t_test(Height ~ Sex) %>% add_significance()
OlderData %>% t_test(BMI ~ Sex) %>% add_significance()

#Physical activity
OlderData %>% t_test(TotalST ~ Sex) %>% add_significance()
OlderData %>% t_test(TotalLPA ~ Sex) %>% add_significance()
wilcox.test(TotalMVPA ~ Sex, OlderData)
wilcox.test(TotalMorningMVPA ~ Sex, OlderData)
wilcox.test(TotalAfternoonMVPA ~ Sex, OlderData)

#Functional fitness
wilcox.test(EightFtUpAndGo ~ Sex, OlderData)
OlderData %>% t_test(ChairStand30s ~ Sex) %>% add_significance()
OlderData %>% t_test(ArmCurl ~ Sex) %>% add_significance()
OlderData %>% t_test(HandgripMean ~ Sex) %>% add_significance()
OlderData %>% t_test(SixMinWalk ~ Sex) %>% add_significance()

# Between day periods regarding MVPA
wilcox.test(OlderData$TotalMorningMVPA, OlderData$TotalAfternoonMVPA)


Male <- OlderData |>  
  filter(Sex == "Male")

Female <- OlderData |>  
  filter(Sex == "Female")


wilcox.test(Male$TotalMorningMVPA, Male$TotalAfternoonMVPA)
wilcox.test(Female$TotalMorningMVPA, Female$TotalAfternoonMVPA)


rm(Male, Female)


# Creating models and comparing slopes ####


# 8-ft-up-and-go

#Morning MVPA
EightTUGMorning1 <- rq(EightFtUpAndGo ~ TotalMorningMVPA, data = OlderData)
summary.rq(EightTUGMorning1, se = "iid")
summary.rq(EightTUGMorning1, se = "rank")

EightTUGMorning2 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(EightTUGMorning2, se = "iid")
summary.rq(EightTUGMorning2, se = "rank")

EightTUGMorning3 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, data = OlderData)
summary.rq(EightTUGMorning3, se = "iid")
summary.rq(EightTUGMorning3, se = "rank")


#Afternoon MVPA
EightTUGAfternoon1 <- rq(EightFtUpAndGo ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(EightTUGAfternoon1, se = "iid")
summary.rq(EightTUGAfternoon1, se = "rank")

EightTUGAfternoon2 <- rq(EightFtUpAndGo ~ TotalAfternoonMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(EightTUGAfternoon2, se = "iid")
summary.rq(EightTUGAfternoon2, se = "rank")

EightTUGAfternoon3 <- rq(EightFtUpAndGo ~ TotalAfternoonMVPA + Age + Sex + BMI + TotalMorningMVPA, data = OlderData)
summary.rq(EightTUGAfternoon3, se = "iid")
summary.rq(EightTUGAfternoon3, se = "rank")


#Total MVPA
EightTUGAfternoon1 <- rq(EightFtUpAndGo ~ TotalMVPA, data = OlderData)
summary.rq(EightTUGAfternoon1, se = "iid")
summary.rq(EightTUGAfternoon1, se = "rank")

EightTUGAfternoon2 <- rq(EightFtUpAndGo ~ TotalMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(EightTUGAfternoon2, se = "iid")
summary.rq(EightTUGAfternoon2, se = "rank")



# 30-s Chair Stand

#Morning MVPA
ChairStandMorning1 <- rq(ChairStand30s ~ TotalMorningMVPA, data = OlderData)
summary.rq(ChairStandMorning1, se = "iid")
summary.rq(ChairStandMorning1, se = "rank")

ChairStandMorning2 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ChairStandMorning2, se = "iid")
summary.rq(ChairStandMorning2, se = "rank")

ChairStandMorning3 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, data = OlderData)
summary.rq(ChairStandMorning3, se = "iid")
summary.rq(ChairStandMorning3, se = "rank")


#Afternoon MVPA
ChairStandAfternoon1 <- rq(ChairStand30s ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(ChairStandAfternoon1, se = "iid")
summary.rq(ChairStandAfternoon1, se = "rank")

ChairStandAfternoon2 <- rq(ChairStand30s ~ TotalAfternoonMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ChairStandAfternoon2, se = "iid")
summary.rq(ChairStandAfternoon2, se = "rank")

ChairStandAfternoon3 <- rq(ChairStand30s ~ TotalAfternoonMVPA + Age + Sex + BMI + TotalMorningMVPA, data = OlderData)
summary.rq(ChairStandAfternoon3, se = "iid")
summary.rq(ChairStandAfternoon3, se = "rank")


#Total MVPA
ChairStandAfternoon1 <- rq(ChairStand30s ~ TotalMVPA, data = OlderData)
summary.rq(ChairStandAfternoon1, se = "iid")
summary.rq(ChairStandAfternoon1, se = "rank")

ChairStandAfternoon2 <- rq(ChairStand30s ~ TotalMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ChairStandAfternoon2, se = "iid")
summary.rq(ChairStandAfternoon2, se = "rank")



# Arm curl

#Morning MVPA
ArmCurlMorning1 <- rq(ArmCurl ~ TotalMorningMVPA, data = OlderData)
summary.rq(ArmCurlMorning1, se = "iid")
summary.rq(ArmCurlMorning1, se = "rank")

ArmCurlMorning2 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ArmCurlMorning2, se = "iid")
summary.rq(ArmCurlMorning2, se = "rank")

ArmCurlMorning3 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, data = OlderData)
summary.rq(ArmCurlMorning3, se = "iid")
summary.rq(ArmCurlMorning3, se = "rank")

#Afternoon MVPA
ArmCurlAfternoon1 <- rq(ArmCurl ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(ArmCurlAfternoon1, se = "iid")
summary.rq(ArmCurlAfternoon1, se = "rank")

ArmCurlAfternoon2 <- rq(ArmCurl ~ TotalAfternoonMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ArmCurlAfternoon2, se = "iid")
summary.rq(ArmCurlAfternoon2, se = "rank")

ArmCurlAfternoon3 <- rq(ArmCurl ~ TotalAfternoonMVPA + Age + Sex + BMI + TotalMorningMVPA, data = OlderData)
summary.rq(ArmCurlAfternoon3, se = "iid")
summary.rq(ArmCurlAfternoon3, se = "rank")


#Total MVPA
ArmCurlAfternoon1 <- rq(ArmCurl ~ TotalMVPA, data = OlderData)
summary.rq(ArmCurlAfternoon1, se = "iid")
summary.rq(ArmCurlAfternoon1, se = "rank")

ArmCurlAfternoon2 <- rq(ArmCurl ~ TotalMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(ArmCurlAfternoon2, se = "iid")
summary.rq(ArmCurlAfternoon2, se = "rank")



#Handgrip strength

#Morning MVPA
HSmorning1 <- rq(HandgripMean ~ TotalMorningMVPA, data = OlderData)
summary.rq(HSmorning1, se = "iid")
summary.rq(HSmorning1, se = "rank")

HSmorning2 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(HSmorning2, se = "iid")
summary.rq(HSmorning2, se = "rank")

HSmorning3 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, data = OlderData)
summary.rq(HSmorning3, se = "iid")
summary.rq(HSmorning3, se = "rank")


#Afternoon MVPA
HSAfternoon1 <- rq(HandgripMean ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(HSAfternoon1, se = "iid")
summary.rq(HSAfternoon1, se = "rank")

HSAfternoon2 <- rq(HandgripMean ~ TotalAfternoonMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(HSAfternoon2, se = "iid")
summary.rq(HSAfternoon2, se = "rank")

HSAfternoon3 <- rq(HandgripMean ~ TotalAfternoonMVPA +  Age + Sex + BMI + TotalMorningMVPA, data = OlderData)
summary.rq(HSAfternoon3, se = "iid")
summary.rq(HSAfternoon3, se = "rank")


#Total MVPA
HSAfternoon1 <- rq(HandgripMean ~ TotalMVPA, data = OlderData)
summary.rq(HSAfternoon1, se = "iid")
summary.rq(HSAfternoon1, se = "rank")

HSAfternoon2 <- rq(HandgripMean ~ TotalMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(HSAfternoon2, se = "iid")
summary.rq(HSAfternoon2, se = "rank")



# Six min walk

#Morning MVPA
SixMinMorning1 <- rq(SixMinWalk ~ TotalMorningMVPA, data = OlderData)
summary.rq(SixMinMorning1, se = "iid")
summary.rq(SixMinMorning1, se = "rank")

SixMinMorning2 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(SixMinMorning2, se = "iid")
summary.rq(SixMinMorning2, se = "rank")

SixMinMorning3 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, data = OlderData)
summary.rq(SixMinMorning3, se = "iid")
summary.rq(SixMinMorning3, se = "rank")

#Afternoon MVPA
SixMinAfternoon1 <- rq(SixMinWalk ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(SixMinAfternoon1, se = "iid")
summary.rq(SixMinAfternoon1, se = "rank")

SixMinAfternoon2 <- rq(SixMinWalk ~ TotalAfternoonMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(SixMinAfternoon2, se = "iid")
summary.rq(SixMinAfternoon2, se = "rank")

SixMinAfternoon3 <- rq(SixMinWalk ~ TotalAfternoonMVPA + Age + Sex + BMI + TotalMorningMVPA, data = OlderData)
summary.rq(SixMinAfternoon3, se = "iid")
summary.rq(SixMinAfternoon3, se = "rank")


#Total MVPA
SixMinAfternoon1 <- rq(SixMinWalk ~ TotalMVPA, data = OlderData)
summary.rq(SixMinAfternoon1, se = "iid")
summary.rq(SixMinAfternoon1, se = "rank")

SixMinAfternoon2 <- rq(SixMinWalk ~ TotalMVPA + Age + Sex + BMI, data = OlderData)
summary.rq(SixMinAfternoon2, se = "iid")
summary.rq(SixMinAfternoon2, se = "rank")



# BMI

#Morning MVPA
BMIMorning1 <- rq(BMI ~ TotalMorningMVPA, data = OlderData)
summary.rq(BMIMorning1, se = "iid")
summary.rq(BMIMorning1, se = "rank")

BMIMorning2 <- rq(BMI ~ TotalMorningMVPA + Age + Sex, data = OlderData)
summary.rq(BMIMorning2, se = "iid")
summary.rq(BMIMorning2, se = "rank")

BMIMorning3 <- rq(BMI ~ TotalMorningMVPA + Age + Sex + TotalAfternoonMVPA, data = OlderData)
summary.rq(BMIMorning3, se = "iid")
summary.rq(BMIMorning3, se = "rank")


#Afternoon MVPA
BMIAfternoon1 <- rq(BMI ~ TotalAfternoonMVPA, data = OlderData)
summary.rq(BMIAfternoon1, se = "iid")
summary.rq(BMIAfternoon1, se = "rank")

BMIAfternoon2 <- rq(BMI ~ TotalAfternoonMVPA + Age + Sex, data = OlderData)
summary.rq(BMIAfternoon2, se = "iid")
summary.rq(BMIAfternoon2, se = "rank")

BMIAfternoon3 <- rq(BMI ~ TotalAfternoonMVPA + Age + Sex + TotalMorningMVPA, data = OlderData)
summary.rq(BMIAfternoon3, se = "iid")
summary.rq(BMIAfternoon3, se = "rank")


#Total MVPA
BMIAfternoon1 <- rq(BMI ~ TotalMVPA, data = OlderData)
summary.rq(BMIAfternoon1, se = "iid")
summary.rq(BMIAfternoon1, se = "rank")

BMIAfternoon2 <- rq(BMI ~ TotalMVPA + Age + Sex, data = OlderData)
summary.rq(BMIAfternoon2, se = "iid")
summary.rq(BMIAfternoon2, se = "rank")




# Internal checking ###

#Creating models below and above median morning MVPA

Below <- OlderData |> 
  dplyr::filter(TotalMorningMVPA <= median(TotalMorningMVPA, na.rm = T))

Above <- OlderData |> 
  dplyr::filter(TotalMorningMVPA > median(TotalMorningMVPA, na.rm = T))

summary(Below$TotalMorningMVPA)
summary(Above$TotalMorningMVPA)


# Creating models for morning MVPA ####

# Eight-ft-up-and-go

#Below
ModelM1 <- rq(EightFtUpAndGo ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(EightFtUpAndGo ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")



# 30s Chair Stand

#Below
ModelM1 <- rq(ChairStand30s ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(ChairStand30s ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")



# Arm Curl

#Below
ModelM1 <- rq(ArmCurl ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(ArmCurl ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")


# Handgrip Strength

#Below
ModelM1 <- rq(HandgripMean ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(HandgripMean ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")



# 6-min walk

#Below
ModelM1 <- rq(SixMinWalk ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(SixMinWalk ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")



# BMI

#Below
ModelM1 <- rq(BMI ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(ModelM1, se = "iid")
summary.rq(ModelM1, se = "rank")

ModelM2 <- rq(BMI ~ TotalMorningMVPA + Age + Sex, tau = 0.5, data = Below)
summary.rq(ModelM2, se = "iid")
summary.rq(ModelM2, se = "rank")

ModelM3 <- rq(BMI ~ TotalMorningMVPA + Age + Sex + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ModelM3, se = "iid")
summary.rq(ModelM3, se = "rank")


#Above
ModelA1 <- rq(BMI ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(ModelA1, se = "iid")
summary.rq(ModelA1, se = "rank")

ModelA2 <- rq(BMI ~ TotalMorningMVPA + Age + Sex, tau = 0.5, data = Above)
summary.rq(ModelA2, se = "iid")
summary.rq(ModelA2, se = "rank")

ModelA3 <- rq(BMI ~ TotalMorningMVPA + Age + Sex + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ModelA3, se = "iid")
summary.rq(ModelA3, se = "rank")




#Creating models below and above median afternoon MVPA

Below <- OlderData |>  
  dplyr::filter(TotalAfternoonMVPA <= median(TotalAfternoonMVPA, na.rm = T))

Above <- OlderData |> 
  dplyr::filter(TotalAfternoonMVPA > median(TotalAfternoonMVPA, na.rm = T))

summary(Below$TotalAfternoonMVPA)
summary(Above$TotalAfternoonMVPA)



# Eight-ft-up-and-go

#Below
VaieVemM1 <- rq(EightFtUpAndGo ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(EightFtUpAndGo ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(EightFtUpAndGo ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")




# 30s Chair Stand

#Below
VaieVemM1 <- rq(ChairStand30s ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(ChairStand30s ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(ChairStand30s ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")





# Arm Curl

#Below
VaieVemM1 <- rq(ArmCurl ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(ArmCurl ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(ArmCurl ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")



# Handgrip Strength

#Below
VaieVemM1 <- rq(HandgripMean ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(HandgripMean ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(HandgripMean ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")



# 6-min walk

#Below
VaieVemM1 <- rq(SixMinWalk ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(SixMinWalk ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(SixMinWalk ~ TotalMorningMVPA + Age + Sex + BMI + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")



# BMI

#Below
VaieVemM1 <- rq(BMI ~ TotalMorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1, se = "iid")
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(BMI ~ TotalMorningMVPA + Age + Sex, tau = 0.5, data = Below)
summary.rq(VaieVemM2, se = "iid")
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(BMI ~ TotalMorningMVPA + Age + Sex + TotalAfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3, se = "iid")
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(BMI ~ TotalMorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1, se = "iid")
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(BMI ~ TotalMorningMVPA + Age + Sex, tau = 0.5, data = Above)
summary.rq(VaieVemA2, se = "iid")
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(BMI ~ TotalMorningMVPA + Age + Sex + TotalAfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3, se = "iid")
summary.rq(VaieVemA3, se = "rank")

