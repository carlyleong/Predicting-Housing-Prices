#### Assignment One ######
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(fastDummies)
library(reshape2)
library(car)

# reading in data
Train <- read.csv("Train.csv", header = TRUE, sep = ",",stringsAsFactors = TRUE) #Downloaded from Kaggle
Test <- read.csv("Test.csv", header = TRUE, sep = ",",  stringsAsFactors = TRUE)
complete_data <- bind_rows(Train, Test) # Binding both datasets together
backup_copy <- complete_data # Backup copy1
cleaned_data <- complete_data

# Deletion of Columns due to theory
cleaned_data <- subset(cleaned_data, 
                       select = -c(LotFrontage, Alley, Utilities, YearBuilt, 
                                   MoSold, Street, Condition2, RoofMatl, MasVnrArea,
                                   Heating, GarageYrBlt, PavedDrive, Exterior2nd, LandSlope)) # due to theory
####################################################################################################
#################### NA's, Grouping, Ordering, data manipulation ######################################
####################################################################################################

################################# MSZoning #################################
cleaned_data$MSZoning[cleaned_data$Id == 1916] <- "RM" #inputting the 4 NA vaules in the MSzoning column
cleaned_data$MSZoning[cleaned_data$Id == 2217] <- "C (all)"
cleaned_data$MSZoning[cleaned_data$Id == 2251] <- "RM"
cleaned_data$MSZoning[cleaned_data$Id == 2905] <- "RL"
cleaned_data <- subset(cleaned_data, select = -c(Id)) # deletion of Id column

################################# Neighborhood #################################
# reduce levels and group
# Ranked the average sale price of each Neighborhood 1-25 and grouped the most similar
# together, I ordered the levels from 1-12 with 1 being the most cheaper Neighborhood to
# 12 being the highest average sale price

levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="BrDale" | 
                                    levels(cleaned_data$Neighborhood)=="IDOTRR" |
                                    levels(cleaned_data$Neighborhood)=="MeadowV"] <-1 
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="Edwards" | 
                                    levels(cleaned_data$Neighborhood)=="BrkSide"] <-2
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="Sawyer" | 
                                    levels(cleaned_data$Neighborhood)=="OldTown"] <-3
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="SWISU" | 
                                    levels(cleaned_data$Neighborhood)=="Blueste"] <-4
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="NAmes" | 
                                    levels(cleaned_data$Neighborhood)=="NPkVill"] <-5
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="SawyerW" | 
                                    levels(cleaned_data$Neighborhood)=="Mitchel"] <-6
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="Gilbert" | 
                                    levels(cleaned_data$Neighborhood)=="NWAmes"] <-7
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="CollgCr" | 
                                    levels(cleaned_data$Neighborhood)=="Blmngtn"] <-8
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="ClearCr" | 
                                    levels(cleaned_data$Neighborhood)=="Crawfor"] <-9
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="Veenker" | 
                                    levels(cleaned_data$Neighborhood)=="Somerst"] <-10
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="StoneBr" | 
                                    levels(cleaned_data$Neighborhood)=="Timber"] <-11
levels(cleaned_data$Neighborhood)[levels(cleaned_data$Neighborhood)=="NridgHt" | 
                                    levels(cleaned_data$Neighborhood)=="NoRidge"] <-12

cleaned_data$Neighborhood <- ordered(cleaned_data$Neighborhood, 1:12) # ordering by grouped average sale price

################################# Condition one ##################################################################
# Grouped by similar features based on theory
# Did not order
levels(cleaned_data$Condition1)[levels(cleaned_data$Condition1)=="RRNn" | 
                                  levels(cleaned_data$Condition1)=="RRAn" |
                                  levels(cleaned_data$Condition1)=="RRNe" |
                                  levels(cleaned_data$Condition1)=="RRAe"] <- "Railroad"

levels(cleaned_data$Condition1)[levels(cleaned_data$Condition1)=="PosN" | 
                                  levels(cleaned_data$Condition1)=="PosA" ] <- "Positive"

levels(cleaned_data$Condition1)[levels(cleaned_data$Condition1)=="Artery" | 
                                  levels(cleaned_data$Condition1)=="Feedr" ] <- "Atery and Feedr" # did not order them

################################# BldgType ##################################################################
# Grouped by theory (External Research)
# Ordered by theory (External Research)
levels(cleaned_data$BldgType)[levels(cleaned_data$BldgType)=="Twnhs" | 
                                levels(cleaned_data$BldgType)=="TwnhsE" ] <- "Townhouse" #grouping and reducing levels

levels(cleaned_data$BldgType)[levels(cleaned_data$BldgType)=="2fmCon" | 
                                levels(cleaned_data$BldgType)=="Duplex" ] <- "Two Family" 

cleaned_data$BldgType<- ordered(cleaned_data$BldgType, c("Townhouse", "Two Family", "1Fam")) # ordering by grouped average sale price

################################# Housestyle ##################################################################
# Grouped by theory (External Research)
# Ordered by each level's (External Research)'s average sale price
levels(cleaned_data$HouseStyle)[ levels(cleaned_data$HouseStyle)=="1Story" |
                                   levels(cleaned_data$HouseStyle)=="1.5Fin" |
                                   levels(cleaned_data$HouseStyle)== "1.5Unf"] <- "1 and 1.5 story" #grouping and reducing levels/ 1 story will be its own

levels(cleaned_data$HouseStyle)[ levels(cleaned_data$HouseStyle)=="2Story" |
                                   levels(cleaned_data$HouseStyle)=="2.5Fin" |
                                   levels(cleaned_data$HouseStyle)== "2.5Unf"] <- "2 and 2.5 story"

levels(cleaned_data$HouseStyle)[ levels(cleaned_data$HouseStyle)=="SFoyer" |
                                   levels(cleaned_data$HouseStyle)== "SLvl"] <- "Other"

cleaned_data$HouseStyle<- ordered(cleaned_data$HouseStyle, c("Other", "1 and 1.5 story", "2 and 2.5 story")) # ordering by grouped average sale price

################################# Roof Style ##################################################################
# Grouped levels based on theory
# Did not order

levels(cleaned_data$RoofStyle)[ levels(cleaned_data$RoofStyle)=="Flat" |
                                  levels(cleaned_data$RoofStyle)== "Gambrel" |
                                  levels(cleaned_data$RoofStyle)== "Hip"|
                                  levels(cleaned_data$RoofStyle)== "Mansard" |
                                  levels(cleaned_data$RoofStyle)== "Shed"
] <- "Other"

### Exterior1st and Exterior2nd
# Grouped levels based on external research
# Did not order based off average sale price of each level
cleaned_data$Exterior1st[is.na(cleaned_data$Exterior1st) == TRUE]  <- "Wd Sdng" #inputting NA value

levels(cleaned_data$Exterior1st)[ levels(cleaned_data$Exterior1st)=="Stone" |
                                    levels(cleaned_data$Exterior1st)== "CemntBd" |
                                    levels(cleaned_data$Exterior1st)== "VinylSd"|
                                    levels(cleaned_data$Exterior1st)== "Plywood" |
                                    levels(cleaned_data$Exterior1st)== "HdBoard" |
                                    levels(cleaned_data$Exterior1st)== "Stucco"] <- "Upper Exterior" 

levels(cleaned_data$Exterior1st)[ levels(cleaned_data$Exterior1st)=="WdShing" |
                                    levels(cleaned_data$Exterior1st)== "Wd Sdng" |
                                    levels(cleaned_data$Exterior1st)== "MetalSd"|
                                    levels(cleaned_data$Exterior1st)== "AsbShng" |
                                    levels(cleaned_data$Exterior1st)== "CBlock" |
                                    levels(cleaned_data$Exterior1st)== "AsphShn" |
                                    levels(cleaned_data$Exterior1st)== "BrkComm"] <- "Lower Exterior" 

##################################### MasVnrType #################################
# Grouped levels based on external research
# Did not order based off average sale price of each level
cleaned_data$MasVnrType[is.na(cleaned_data$MasVnrType)  == TRUE] <- "None" # replaced NA with None

levels(cleaned_data$MasVnrType)[ levels(cleaned_data$MasVnrType)=="BrkCmn" |
                                   levels(cleaned_data$MasVnrType)== "BrkFace"] <- "Brick"
levels(cleaned_data$MasVnrType)[ levels(cleaned_data$MasVnrType)=="CBlock" |
                                   levels(cleaned_data$MasVnrType)== "Stone"] <- "Other" # grouping, have not ordered yet

################################# ExterQual and ExterCond ##################################################################

rating_exter <- c("Po","Fa", "TA", "Gd", "Ex") # Created the ordered rating
cleaned_data$ExterQual <- ordered(cleaned_data$ExterQual, rating_exter) # Applied the ordered rating
cleaned_data$ExterCond <- ordered(cleaned_data$ExterCond, rating_exter)
cleaned_data$ExterQual <- as.numeric(cleaned_data$ExterQual) # the values of the levels are now 1-5
cleaned_data$ExterCond <- as.numeric(cleaned_data$ExterCond)

################################# Foundation ##################################################################
# Grouped levels based on average sale price
# Ordered by average sale price
levels(cleaned_data$Foundation)[ levels(cleaned_data$Foundation)=="CBlock" |
                                   levels(cleaned_data$Foundation)== "PConc" |
                                   levels(cleaned_data$Foundation)== "Slab"] <- "Newer Foundation"
levels(cleaned_data$Foundation)[ levels(cleaned_data$Foundation)=="BrkTil" |
                                   levels(cleaned_data$Foundation)== "Stone" |
                                   levels(cleaned_data$Foundation)== "Wood" 
] <- "Older Foundation"  #grouping factors

cleaned_data$Foundation<- ordered(cleaned_data$Foundation, c("Older Foundation", "Newer Foundation")) # Ordering Factors

### BsmtQual, BsmtCond, BsmtExposure
rating <- c("NB", "Po", "Fa", "TA", "Gd", "Ex") # Creating ratings for each with NA's being the lowest
bsmt_finished_rating <- c("NB", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")

cleaned_data$BsmtQual<- ordered(cleaned_data$BsmtQual, levels = rating) # Ordered by the above ratings

cleaned_data$BsmtQual[(is.na(cleaned_data$BsmtQual) == TRUE)] <- "NB" # Inputting "NB" for "NA"

cleaned_data$BsmtQual <- as.numeric(cleaned_data$BsmtQual) #turning into numeric

cleaned_data <- subset(cleaned_data, select = -c(BsmtCond)) # Deleting unecessary feature

bsmt_exposure_rating <- c("No", "Mn", "Av", "Gd") # Created a rating with "No" (NA) as the lowest level
cleaned_data$BsmtExposure[is.na(cleaned_data$BsmtExposure) == TRUE] <- "No" # Inputting "No"as NA
cleaned_data$BsmtExposure<- ordered(cleaned_data$BsmtExposure, levels = bsmt_exposure_rating) # Ordered by the above rating

### BsmtFinType1, BsmtFinSF1, BsmtFinType2, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF #################################

bsmt_finished_rating <- c("NB", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ") # establishing factor levels before getting putting NA's in 
cleaned_data$BsmtFinType1<- ordered(cleaned_data$BsmtFinType1, bsmt_finished_rating) # ordering by rates
cleaned_data$BsmtFinType2<- ordered(cleaned_data$BsmtFinType2, bsmt_finished_rating) # 

cleaned_data$BsmtFinType1[(is.na(cleaned_data$BsmtFinType1) == TRUE)] <- "NB" #putting NA's as NB level
cleaned_data$BsmtFinType2[(is.na(cleaned_data$BsmtFinType2) == TRUE)] <- "NB"

cleaned_data$BsmtFinType1 <- as.numeric(cleaned_data$BsmtFinType1) #turning into numeric
cleaned_data$BsmtFinType2 <- as.numeric(cleaned_data$BsmtFinType2) #turning into numeric

cleaned_data <- cleaned_data %>% mutate(avg_bsmt_fintype = (BsmtFinType1 + BsmtFinType2)/2) # taking average of bsmt fintype

cleaned_data <- subset(cleaned_data, select = -c(BsmtFinType2)) # Deleting columns becasue average was takei

cleaned_data$BsmtFinSF1[(is.na(cleaned_data$BsmtFinSF1) == TRUE)] <- 0 # Getting rid of NA from numericals
cleaned_data$BsmtFinSF2[(is.na(cleaned_data$BsmtFinSF2) == TRUE)] <- 0
cleaned_data$BsmtUnfSF[is.na(cleaned_data$BsmtUnfSF) == TRUE] <- 0

################################# HeatingQC ##################################################################
heat_rating <- c("Po", "Fa", "TA", "Gd", "Ex")
cleaned_data$HeatingQC <- ordered(cleaned_data$HeatingQC , levels = heat_rating)
cleaned_data$HeatingQC <- as.numeric(cleaned_data$HeatingQC)

################################## CentralAir ##################################################################
cleaned_data$CentralAir <- ordered(cleaned_data$CentralAir, levels = c("N", "Y"))
cleaned_data$CentralAir <- as.numeric(cleaned_data$CentralAir)

################################### Electrical ####################################################################
electrical_rating <- c("Other","SBrkr")

levels(cleaned_data$Electrical)[ levels(cleaned_data$Electrical)=="FuseA" |
                                   levels(cleaned_data$Electrical)== "FuseF" |
                                   levels(cleaned_data$Electrical)== "FuseP" | # grouping and reducing levels
                                   levels(cleaned_data$Electrical)== "Mix"
] <- "Other"

cleaned_data$Electrical <- ordered(cleaned_data$Electrical, levels = electrical_rating)
cleaned_data$Electrical[is.na(cleaned_data$Electrical) == TRUE] <- "SBrkr" # getting rid of the NA's


############################ BsmtFullBath BsmtHalfBath FullBath HalfBath ##################################
cleaned_data$BsmtFullBath[is.na(cleaned_data$BsmtFullBath) == TRUE] <- 0 # inputting the N
cleaned_data$BsmtHalfBath[is.na(cleaned_data$BsmtHalfBath) == TRUE] <- 0


##################### BedroomAbvGr , KitchenAbvGr , KitchenQual , TotRmsAbvGrd ##################################

KitQual_rating <- c("Po", "Fa", "TA", "Gd", "Ex")
cleaned_data$KitchenQual <- ordered(cleaned_data$KitchenQual, levels = KitQual_rating) # factor kitchen quality ratings
cleaned_data$KitchenQual[is.na(cleaned_data$KitchenQual) == TRUE] <- "TA" #inputting NA
cleaned_data$KitchenQual <- as.numeric(cleaned_data$KitchenQual)

##################################### Functional #######################################################################

str(cleaned_data)
Functional_rating <- c("Other", "Typ") # Reducing levels

levels(cleaned_data$Functional)[ levels(cleaned_data$Functional)=="Min1" |
                                   levels(cleaned_data$Functional)== "Min2" |
                                   levels(cleaned_data$Functional)== "Mod" | 
                                   levels(cleaned_data$Functional)== "Maj1" |
                                   levels(cleaned_data$Functional)== "Sev" |
                                   levels(cleaned_data$Functional)== "Maj2" |
                                   levels(cleaned_data$Functional)== "Sal"] <- "Other"

cleaned_data$Functional <- ordered(cleaned_data$Functional, levels = Functional_rating) # factor Functional_rating quality ratings
cleaned_data$Functional[is.na(cleaned_data$Functional) == TRUE] <- "Typ"

################################### Fireplaces and FireplaceQu ####################################################################
fireplace_rating <- c("NF", "Po", "Fa", "TA", "Gd", "Ex") 
cleaned_data$FireplaceQu <- ordered(cleaned_data$FireplaceQu,  fireplace_rating)
cleaned_data$FireplaceQu[is.na(cleaned_data$FireplaceQu == TRUE)] <- "NF"

################GarageFinish GarageArea, GarageQual, GarageCond GarageCars ####################################################################

GarageFinish_rating <- c("Others", "Fin") # reducing the levels to finished and others

levels(cleaned_data$GarageFinish)[ levels(cleaned_data$GarageFinish)=="NG" |
                                     levels(cleaned_data$GarageFinish)== "Unf" |
                                     levels(cleaned_data$GarageFinish)== "RFn" ] <- "Others"

cleaned_data$GarageFinish <- ordered(cleaned_data$GarageFinish, levels = GarageFinish_rating)
cleaned_data$GarageFinish[is.na(cleaned_data$GarageFinish) == TRUE] <- "Others" # Replacing NA with NG

################################## GarageType #########################################################################

Garagetype_rating <- c("Other", "2Types", "Basment", "Attchd") # reducing the levels to basement/carport/NA

levels(cleaned_data$GarageType)[ levels(cleaned_data$GarageType)=="Basment" |
                                   levels(cleaned_data$GarageType)== "CarPort" ] <- "Other"

cleaned_data$GarageType <- ordered(cleaned_data$GarageType, levels = Garagetype_rating )
cleaned_data$GarageType[is.na(cleaned_data$GarageType) == TRUE] <- "Other" #putting NA's into other category


################################ Garage qual and cond ####################################################################
garage_analysis <- cleaned_data[1:1460,] %>% group_by(GarageType) %>% summarise(mean(SalePrice)) # Rate by average sale price

Garage_rating <- c("NG", "Po", "TA", "Gd", "Ex")
cleaned_data$GarageQual <- ordered(cleaned_data$GarageQual, levels = Garage_rating) # facotoring garage qual and cond
cleaned_data$GarageCond <- ordered(cleaned_data$GarageCond, levels = Garage_rating)

cleaned_data$GarageQual[is.na(cleaned_data$GarageQual) == TRUE] <- "NG" #replacing NA with NG
cleaned_data$GarageCond[is.na(cleaned_data$GarageCond) == TRUE] <- "NG"

###################################### Garage cars and area ########################################################################
cleaned_data$GarageCars[is.na(cleaned_data$GarageCars) == TRUE] <- 0
cleaned_data$GarageArea[is.na(cleaned_data$GarageArea) == TRUE] <- 0

cleaned_data$GarageQual <- as.numeric(cleaned_data$GarageQual) # changing type to numeric
cleaned_data$GarageCond <- as.numeric(cleaned_data$GarageCond)

################################### PoolArea , PoolQC ####################################################################
pool_rating <- c("NP", "Pool") # Turning pool into a factor (pool or no Pool)

levels(cleaned_data$PoolQC)[ levels(cleaned_data$PoolQC)=="Fa" |
                               levels(cleaned_data$PoolQC)== "TA" |
                               levels(cleaned_data$PoolQC)== "Gd" |
                               levels(cleaned_data$PoolQC)== "Ex" ] <- "Pool"

cleaned_data$PoolQC <- ordered(cleaned_data$PoolQC, levels = pool_rating)
cleaned_data$PoolQC[is.na(cleaned_data$PoolQC) == TRUE] <- "NP" #replacing NA with NP

cleaned_data <- subset(cleaned_data, select = -c(PoolArea))

################################### Fence ######################################################################################################

Fence_rating <- c("NF", "Fence") # Fence or no Fence
levels(cleaned_data$Fence)[ levels(cleaned_data$Fence)=="MnWw" |
                              levels(cleaned_data$Fence)== "GdWo"|
                              levels(cleaned_data$Fence)== "MnPrv" |
                              levels(cleaned_data$Fence)== "GdPrv" ] <- "Fence"


cleaned_data$Fence <- ordered(cleaned_data$Fence, levels = Fence_rating) # facotoring Fenc
cleaned_data$Fence[is.na(cleaned_data$Fence) == TRUE] <- "NF" #replacing NA with NFe

#################################### MiscFeature ######################################################################


Misc_rating <- c("None", "TenC", "Shed", "Othr", "Gar2", "Elev")
cleaned_data$MiscFeature <- factor(cleaned_data$MiscFeature, levels = Misc_rating)
cleaned_data$MiscFeature[is.na(cleaned_data$MiscFeature) == TRUE] <- "None"

levels(cleaned_data$MiscFeature)[ levels(cleaned_data$MiscFeature)=="TenC" |
                                    levels(cleaned_data$MiscFeature)== "Shed"|
                                    levels(cleaned_data$MiscFeature)== "Othr" |
                                    levels(cleaned_data$MiscFeature)== "Gar2" |
                                    levels(cleaned_data$MiscFeature)== "Elev" ] <- 2
levels(cleaned_data$MiscFeature)[ levels(cleaned_data$MiscFeature)=="None" ] <- 1
cleaned_data$MiscFeature <- ordered(cleaned_data$MiscFeature, levels = 1:2)

cleaned_data <- subset(cleaned_data, select = -c(MiscFeature))

#################################### MiscVal ########################################################################
ggplot(cleaned_data, aes(MiscVal, log(SalePrice))) + geom_jitter()

mean_MiscVal <- mean(cleaned_data$MiscVal, na.rm = TRUE)
cleaned_data$MiscVal[cleaned_data$MiscVal == 0] <- mean_MiscVal

for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$MiscVal[i] > 3000)
  {cleaned_data$MiscVal[i] <-  mean_MiscVal}
}

cleaned_data <- subset(cleaned_data, select = -c(MiscVal))
#################################### SaleType ######################################################################
levels(cleaned_data$SaleType)[ levels(cleaned_data$SaleType)=="COD" |
                                 levels(cleaned_data$SaleType)== "Con"|
                                 levels(cleaned_data$SaleType)== "ConLw" |
                                 levels(cleaned_data$SaleType)== "ConLI" |
                                 levels(cleaned_data$SaleType)== "ConLD" |
                                 levels(cleaned_data$SaleType)== "Oth"   ] <- "Other"
levels(cleaned_data$SaleType)[ levels(cleaned_data$SaleType)=="WD" |
                                 levels(cleaned_data$SaleType)== "CWD"|
                                 levels(cleaned_data$SaleType)== "VWD" ] <- "Warranty" # three levels, other, warrantly, new

cleaned_data$SaleType[is.na(cleaned_data$SaleType) == TRUE] <- "Warranty"

################################## SaleCondition ######################################################################

levels(cleaned_data$SaleCondition)[ levels(cleaned_data$SaleCondition)=="Abnorml" |
                                      levels(cleaned_data$SaleCondition)== "AdjLand"|
                                      levels(cleaned_data$SaleCondition)== "Alloca" |
                                      levels(cleaned_data$SaleCondition)== "Family" |
                                      levels(cleaned_data$SaleCondition)== "Partial" |
                                      levels(cleaned_data$SaleCondition)== "Oth"   ] <- "Other" #reduced to two levels normal and other

cleaned_data$SaleCondition<- ordered(cleaned_data$SaleCondition, c("Other", "Normal"))
cleaned_data$SaleCondition<- as.numeric(cleaned_data$SaleCondition)

#################################### Open porch ###################################################################################################

cleaned_data$OpenPorchSF[is.na(cleaned_data$OpenPorchSF == TRUE)] <- 
  mean(cleaned_data$OpenPorchSF, na.rm = TRUE)

#total bsmt NA
cleaned_data$TotalBsmtSF[is.na(cleaned_data$TotalBsmtSF) == TRUE] <- mean(cleaned_data$TotalBsmtSF,na.rm = TRUE)

################################### OLS Model before outliers #################################################################
cleaned_data_before <- cleaned_data


###################################################################################
#################### Removing Outliers  ##########################################
####################################################################################

#################### MSSubClass ############################################################

cleaned_data$MSSubClass <- factor(cleaned_data$MSSubClass)
summary(cleaned_data$MSSubClass)

levels(cleaned_data$MSSubClass)[ levels(cleaned_data$MSSubClass)== 40 |
                                 levels(cleaned_data$MSSubClass)== 45] <- 43
levels(cleaned_data$MSSubClass)[ levels(cleaned_data$MSSubClass)== 150 |
                                 levels(cleaned_data$MSSubClass)== 160] <- 155 


# LotArea
ggplot(cleaned_data, aes(LotArea, log(SalePrice))) + geom_jitter()


mean_lotarea <- mean(cleaned_data$LotArea, na.rm = TRUE)

for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$LotArea[i] > 30000)
  {cleaned_data$LotArea[i] <-  mean_lotarea}
}
################### BsmtUnfSF (maybe disregard) ########################################
ggplot(cleaned_data, aes(BsmtUnfSF, SalePrice)) + geom_jitter()
cleaned_data <- subset(cleaned_data, select = -c(BsmtUnfSF))

################### BsmtFinType2 ########################################
ggplot(cleaned_data, aes(BsmtFinType1,SalePrice)) + geom_jitter()


##################### TotalBsmtSF ############################################################

ggplot(cleaned_data, aes(TotalBsmtSF, SalePrice)) + geom_jitter()
mean_TotalBsmtSF<- mean(cleaned_data$TotalBsmtSF, na.rm = TRUE)
for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$TotalBsmtSF[i] > 4500)
  {cleaned_data$TotalBsmtSF[i] <-  mean_TotalBsmtSF}
}


##################### GrLivArea >4500 ############################################################
ggplot(cleaned_data, aes(GrLivArea, SalePrice)) + geom_jitter()

mean_GrLivArea <- mean(cleaned_data$GrLivArea, na.rm = TRUE)
for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$GrLivArea[i] > 4500)
  {cleaned_data$GrLivArea[i] <-  mean_GrLivArea}}

##################### GarageArea ############################################################

ggplot(cleaned_data, aes(GarageArea, log(SalePrice))) + geom_jitter()

mean_GarageArea <- mean(cleaned_data$GarageArea, na.rm = TRUE)
for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$GarageArea[i] > 1200)
  {cleaned_data$GarageArea[i] <-  mean_GarageArea}
}

##################### WoodDeckSF ############################################################
#ggplot(cleaned_data, aes(WoodDeckSF,log(SalePrice))) + geom_jitter()

cleaned_data <- subset(cleaned_data, select = -c(WoodDeckSF))

##################### OpenPorchSF >400 ############################################################
ggplot(cleaned_data, aes(OpenPorchSF, log(SalePrice))) + geom_jitter()
mean_OpenPorchSF <- mean(cleaned_data$OpenPorchSF, na.rm = TRUE)
for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$OpenPorchSF[i] > 400)
  {cleaned_data$OpenPorchSF[i] <-  mean_OpenPorchSF}}
#cleaned_data <- subset(cleaned_data, select = -c(OpenPorchSF))

##################### EnclosedPorch > 300 ############################################################
ggplot(cleaned_data, aes(EnclosedPorch,log(SalePrice))) + geom_jitter()

mean_EnclosedPorch <- mean(cleaned_data$EnclosedPorch, na.rm = TRUE)
cleaned_data$EnclosedPorch[cleaned_data$EnclosedPorch == 0] <- mean_EnclosedPorch

for (i in 1:nrow(cleaned_data)) {
  if (cleaned_data$EnclosedPorch[i] > 300)
  {cleaned_data$EnclosedPorch[i] <-  mean_EnclosedPorch}}

#cleaned_data <- subset(cleaned_data, select = -c(EnclosedPorch))
##################### x3SsnPorch ############################################################

#ggplot(cleaned_data, aes(X3SsnPorch,SalePrice)) + geom_jitter()

mean_x3SsnPorch <- mean(cleaned_data$X3SsnPorch)
cleaned_data$X3SsnPorch[cleaned_data$X3SsnPorch == 0] <- mean_x3SsnPorch

# ScreenPorch

#ggplot(cleaned_data, aes(ScreenPorch,SalePrice)) + geom_jitter()
cleaned_data <- subset(cleaned_data, select = -c(ScreenPorch))



####################################################################################
##################### Modeling for fully cleaned data ##############################
####################################################################################

##################### Final Cleaned_data Transformations ###########################

# Resplitting Test and Train
Train1 <- cleaned_data[1:1460,]
Test1 <- cleaned_data[1461:2919,]
Id <- Test[,1] # Extracting the Id's from the Test dataset to use later when predicting
Train1$SalePrice <- log(Train1$SalePrice) # Transforming Train SalePrice

###################################################################################
##################### OLS Model After Outliars ###################################
###################################################################################

##################### Validation model OLS  ##############################################

library(Metrics) #To calculate RMSLE
library(MASS)
set.seed(123) # To make results reproducible

smp_size <- floor(0.75 * nrow(Train1)) # Taking 75% of the Train Data to create an index
train_ind <- sample(seq_len(nrow(Train1)), size = smp_size) # Creating a random sample
pre_train <- Train1[train_ind, ] # Creating a validation model
pre_test <- Train1[-train_ind, ]

validation_model <- lm(SalePrice ~ ., pre_train) # Creating validation model
validation_pred <- predict(validation_model, newdata = pre_test) # Predicting SalePrice Values from the Train set
print(error_ols <- rmsle(validation_pred, pre_test$SalePrice)) #Calculating the error for the validation set
# do not need to unlog the SalePrice becasue all of the Saleprices are logged

###################################### OLS Model ########################################

Final_OLS <- lm(SalePrice ~ ., Train1) # Creating model based on the whole Train set
Final_pred_OLS <- predict(Final_OLS, newdata = Test1) # Predicting Test data set SalePrices
Final_pred_values_OLS <- data.frame(Final_pred_OLS)

Id <- Test[,1]
OLS_final_prediction1<- data.frame(Id) # Creating a data frame for the predicted values
OLS_final_prediction1$SalePrice <- exp(Final_pred_OLS) # Transforming predicted values back by exp()
write.csv(OLS_final_prediction1, "OLS_prediction_final.csv", row.names=FALSE) # Saving Data frame as CSV

