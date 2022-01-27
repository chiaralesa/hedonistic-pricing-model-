setwd("~/Master WU/1. Jahr/Semester II/Econometrics/kaggle competition")

library(lmtest)
library(tseries)
library(olsrr)
library(caret)
library(mlbench)
library(Metrics)
library(corrplot)
library(tidyr)
library(ggplot2)
library(purrr)
library(dplyr)
library(BAS)



train <- read.csv("train.csv", 
                  header=TRUE, 
                  sep=",",
                  dec = ".",
                  stringsAsFactors=FALSE)



# Load your data to test
test <- read.csv2("test.csv", 
                  header=TRUE, 
                  sep=",",
                  dec = ".",
                  stringsAsFactors=FALSE)

head(train,15)

train$BsmtRating[is.na(train$BsmtRating)] <- "NoBsm"
train$GarageType[is.na(train$GarageType)] <- "NoGar"
train$GarageFinish[is.na(train$GarageFinish)] <- "NoG"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "NoBsmt"
train$GarageCond[is.na(train$GarageCond)] <- "Nogar"
train$GarageQual[is.na(train$GarageQual)] <- "NoGar"
train$GarageType[is.na(train$GarageType)]<- "NoGar"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "No"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$BsmtQual[is.na(train$BsmtQual)] <- "No"
train$BsmtCond[is.na(train$BsmtCond)] <- "No"
train$GarageFinish[is.na(train$GarageFinish)] <- "NoG"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train[,"year.old"]=as.integer(train[,"YrSold"] -train[,"YearBuilt"])
train[,"year.mod"]=as.integer(train[,"YrSold"] -train[,"YearRemodAdd"]
)





train$Gfin<- ifelse(train$GarageFinish == "Unf" | train$GarageFinish == "NoG", 0 , 1)
train$PPool <- ifelse(train$PoolArea != 0 , 1 , 0)
train[,"remod"]=1*(train[,"YearBuilt"] != train[,"YearRemodAdd"])
train[,"CentralAir"]<- ifelse(train$CentralAir =="Y", 1,0)
train[,"SaleConditionNormal"]<-ifelse(train$SaleCondition =="Normal", 1,0)


#create manually the dummies
train[,"HouseStyle1Story1.5Sto"] <- ifelse(train$HouseStyle == "1Story" | train$HouseStyle== "1.5Sto", 1, 0)
train[,"HouseStyle2Story2.5Sto"] <- ifelse(train$HouseStyle == "2Story" | train$HouseStyle== "2.5Sto", 1, 0)

train$ExterQualFaTA <- ifelse(train$ExterQual == "Fa" | train$ExterQual== "TA", 1, 0)
train$ExterQualGdEx <- ifelse(train$ExterQual == "Gd" | train$ExterQual== "Ex", 1, 0)

train$ExterCondFaTA <- ifelse(train$ExterCond == "Fa" | train$ExterCond== "TA", 1, 0)
train$ExterCondGdEx <- ifelse(train$ExterCond == "Gd" | train$ExterCond== "Ex", 1, 0)

train$BsmtQualFaTA <- ifelse(train$BsmtQual == "Fa" | train$BsmtQual== "TA", 1, 0)
train$BsmtQualGdEx <- ifelse(train$BsmtQual == "Gd" | train$BsmtQual== "Ex", 1, 0)

train$BsmtCondFaTA <- ifelse(train$BsmtCond == "Fa" | train$BsmtCond== "TA", 1, 0)
train$BsmtCondGdEx <- ifelse(train$BsmtCond == "Gd" | train$BsmtCond== "Ex", 1, 0)

train$KitchenQualFaTA <- ifelse(train$KitchenQual == "Fa" | train$KitchenQual== "TA", 1, 0)
train$KitchenQualGdEx <- ifelse(train$KitchenQual == "Gd" | train$KitchenQual== "Ex", 1, 0)

train$LotConfigCornerInside <- ifelse(train$LotConfig == "Inside" | train$LotConfig== "Corner", 1, 0)
train$LotConfigCulFR2 <- ifelse(train$LotConfig == "CulDSac" | train$LotConfig== "FR2", 1, 0)
train[train$LivAreaSF>2600,]$LivAreaSF = 2600
train[train$X1stFlrSF>2000,]$X1stFlrSF = 2000
# summary(train$LotArea)
# 
MM <- model.matrix( ~ LotConfig + BldgType + HouseStyle + OverallQual
                    + OverallCond + RoofStyle + ExterQual +ExterCond + Foundation +BsmtRating
                    +BsmtQual+ BsmtCond + BsmtExposure + BsmtRating + KitchenQual
                    +Functional + FireplaceQu + GarageType + GarageFinish + GarageQual
                    + GarageCond  - 1, data = train)

train <- cbind(train,MM)

train_new <- train[-c(52,4),]


train_new[which(train_new$YrSold < train_new$YearRemodAdd),]$YearRemodAdd <-
  train_new[which(train_new$YrSold < train_new$YearRemodAdd),]$YearBuilt <- 
  train_new[which(train_new$YrSold < train_new$YearRemodAdd),]$YrSold


train_new[,"yearssinceremod"] <- train_new$YrSold - train_new$YearRemodAdd
train_new[,"remodeledRecent"]<- ifelse(train_new$yearssinceremod < 1, 1, 0) 

train_new <- train_new %>% 
  mutate(FullBath = as.numeric(as.character(FullBath)),
         HalfBath = as.numeric(as.character(HalfBath)),
         TotalBathRooms =   FullBath + 0.5* HalfBath)
train_new <- train_new %>% mutate(TotalSF = BsmtSF + X1stFlrSF + X2ndFlrSF) 




train_new$OldHouse <- ifelse(train_new$YearBuilt <= 1949, 1, 0)
train_new$MiddleHouse <- ifelse(train_new$YearBuilt > 1949 & train_new$YearBuilt <= 1989, 1, 0)
train_new$ModernHouse <- ifelse(train_new$YearBuilt > 1989 & train_new$YearBuilt <= 2000, 1, 0)
train_new$NewHouse <- ifelse(train_new$YearBuilt > 2000 , 1, 0)

train_new$SmallLot <- ifelse(train_new$LotArea <= 8000 , 1, 0)
train_new$MedLot<- ifelse(train_new$LotArea > 8000 & train_new$LotArea <= 10000, 1, 0)
train_new$BigLot <- ifelse(train_new$LotArea > 10000, 1, 0)

train_new$SmallArea<- ifelse(train_new$LivAreaSF <= 1200 , 1, 0)
train_new$MedArea<- ifelse(train_new$LivAreaSF > 1200 & train_new$LivAreaSF <= 1700, 1, 0)
train_new$BigArea <- ifelse(train_new$LivAreaSF> 1700, 1, 0)

head(train_new$MoSold, 20)
train_new$Spring<- ifelse(train_new$MoSold == c(3,4,5)  , 1, 0)
train_new$Summer <- ifelse(train_new$MoSold == c(6,7,8), 1, 0)
train_new$Fall<- ifelse(train_new$MoSold == c(9,10,11),1 ,0)
train_new$Winter<- ifelse(train_new$MoSold == c(12,1,2), 1, 0)



reg <-lm(log(SalePrice)  ~ Zone + log(LotArea)+ TotalSF + OverallQual 
         + X2ndFlrSF+ log(LowQualSF+1)+  ExterCondTA + 
           + OverallCond + ExterQual + BsmtCond 
           + log(BsmtSF+1) + 
         + log(X1stFlrSF) + sqrt(LivAreaSF) + log(YearBuilt+1)
         + Kitchen+ Fireplaces + SaleConditionNormal+ SmallLot+ SmallArea+
           + BigArea+ BldgType+ Fall+ KitchenQual+  PPool*log(PoolArea+1)
         +  FoundationPConc + FunctionalMin+ yearssinceremod+
           +FunctionalTyp + NewHouse*GarageArea + year.old 
         ,
         data = train_new)



summary(reg)


#Calculate your predictions comparing the regression data (that you get from using the lm() function) with the test data
require(stats)


test$BsmtRating[is.na(test$BsmtRating)] <- "NoBsm"
train$GarageFinish[is.na(train$GarageFinish)] <- "NoG"
test[,"remod"]=1*(test[,"YearBuilt"] != test[,"YearRemodAdd"])
test[,"CentralAir"]<- ifelse(test$CentralAir =="Y", 1,0)
test$GarageType[is.na(test$GarageType)] <- "NoGar"


test$BsmtRating[is.na(test$BsmtRating)] <- "NoBsm"


test$GarageType[is.na(test$GarageType)] <- "NoGar"


test$GarageFinish[is.na(test$GarageFinish)] <- "NoG"

test$Gfin<- ifelse(test$GarageFinish == "Unf" | test$GarageFinish == "NoG", 0 , 1)


test$GarageCond[is.na(test$GarageCond)] <- "Nogar"

test$GarageQual[is.na(test$GarageQual)] <- "NoGar"



test$GarageCond[is.na(test$GarageCond)] <- "Nogar"
test$PPool <- ifelse(test$PoolArea != 0 , 1 , 0)

test$BsmtExposure[is.na(test$BsmtExposure)] <- "NoBsmt"
test$GarageQual[is.na(test$GarageQUal)] <- "NoGar"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "No"
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$BsmtQual[is.na(test$BsmtQual)] <- "No"
test$BsmtCond[is.na(test$BsmtCond)] <- "No"
test[,"SaleConditionNormal"]<-ifelse(test$SaleCondition =="Normal", 1,0)
#test$SaleCondition <-ifelse(test$SaleCondition !="Normal" , 0 , 1)
test[,"year.old"]=as.integer(test[,"YrSold"] -test[,"YearBuilt"])
test[,"year.mod"]=as.integer(test[,"YrSold"] -test[,"YearRemodAdd"])


test[,"HouseStyle1Story1.5Sto"] <- ifelse(test$HouseStyle == "1Story" | test$HouseStyle== "1.5Sto", 1, 0)
test[,"HouseStyle2Story2.5Sto"] <- ifelse(test$HouseStyle == "2Story" | test$HouseStyle== "2.5Sto", 1, 0)

test$ExterQualFaTA <- ifelse(test$ExterQual == "Fa" | test$ExterQual== "TA", 1, 0)
test$ExterQualGdEx <- ifelse(test$ExterQual == "Gd" | test$ExterQual== "Ex", 1, 0)

test$ExterCondFaTA <- ifelse(test$ExterCond == "Fa" | test$ExterCond== "TA", 1, 0)
test$ExterCondGdEx <- ifelse(test$ExterCond == "Gd" | test$ExterCond== "Ex", 1, 0)

test$BsmtQualFaTA <- ifelse(testBsmtQual == "Fa" |test$BsmtQual== "TA", 1, 0)
test$BsmtQualGdEx <- ifelse(test$BsmtQual == "Gd" | test$BsmtQual== "Ex", 1, 0)

test$BsmtCondFaTA <- ifelse(test$BsmtCond == "Fa" | test$BsmtCond== "TA", 1, 0)
test$BsmtCondGdEx <- ifelse(test$BsmtCond == "Gd" | test$BsmtCond== "Ex", 1, 0)

test$KitchenQualFaTA <- ifelse(test$KitchenQual == "Fa" | test$KitchenQual== "TA", 1, 0)
test$KitchenQualGdEx <- ifelse(test$KitchenQual == "Gd" | test$KitchenQual== "Ex", 1, 0)

test$LotConfigCornerInside <- ifelse(test$LotConfig == "Inside" | test$LotConfig== "Corner", 1, 0)
test$LotConfigCulFR2<- ifelse(test$LotConfig == "CulDSac" | test$LotConfig== "FR2", 1, 0)

test$OldHouse <- ifelse(test$YearBuilt <= 1949, 1, 0)
test$MiddleHouse <- ifelse(test$YearBuilt > 1949 & test$YearBuilt <= 1989, 1, 0)
test$ModernHouse <- ifelse(test$YearBuilt > 1989 & test$YearBuilt <= 2000, 1, 0)
test$NewHouse <- ifelse(test$YearBuilt > 2000 , 1, 0)


test$SmallLot <- ifelse(test$LotArea <= 8000 , 1, 0)
test$MedLot<- ifelse(test$LotArea > 8000 & test$LotArea <= 10000, 1, 0)
test$BigLot <- ifelse(test$LotArea > 10000, 1, 0)

test$SmallArea<- ifelse(test$LivAreaSF <= 1200 , 1, 0)
test$MedArea<- ifelse(test$LivAreaSF > 1200 & test$LivAreaSF <= 1700, 1, 0)
test$BigArea <- ifelse(test$LivAreaSF> 1700, 1, 0)

test$Spring <- ifelse(test$MoSold == 3|test$MoSold == 4 | test$MoSold == 5, 1, 0)
test$Summer <- ifelse(test$MoSold == 6|test$MoSold == 7 | test$MoSold == 8, 1, 0)
test$Fall <- ifelse(test$MoSold == 9|test$MoSold == 10 | test$MoSold == 11,1 ,0)
test$Winter<- ifelse(test$MoSold == 1|test$MoSold == 2 | test$MoSold == 12, 1, 0)


NN<- model.matrix( ~ Type + Zone + LotConfig + BldgType + HouseStyle + OverallQual
                   + OverallCond + RoofStyle + ExterQual +ExterCond + Foundation +BsmtRating
                   +BsmtQual+ BsmtCond + BsmtExposure + BsmtRating + KitchenQual 
                   +Functional + FireplaceQu + GarageType + GarageFinish + GarageQual 
                   + GarageCond +  - 1, data = test)

test <- cbind(test, NN)


colSums(is.na(test))

test[,"yearssinceremod"] <-test$YrSold - test$YearRemodAdd
test[,"remodeledRecent"]<- ifelse(test$yearssinceremod < 1, 1, 0) 

test <-test%>% 
  mutate(FullBath = as.numeric(as.character(FullBath)),
         HalfBath = as.numeric(as.character(HalfBath)),
         TotalBathRooms =   FullBath + 0.5* HalfBath)
test <- test %>% mutate(TotalSF = BsmtSF + X1stFlrSF + X2ndFlrSF) 
train_new <- train_new %>% mutate(TotalSF = BsmtSF + X1stFlrSF + X2ndFlrSF) 


prediction <- predict(reg,test)
exp(prediction)


head(test)

pred <- exp(prediction)

my_model = data.frame(test$Id, pred)
names(my_model) = c("Id","SalePrice")

write.table(my_model, file = "cl23.csv", sep = ",", dec=".", row.names=F)
