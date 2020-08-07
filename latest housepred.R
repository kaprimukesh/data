library(dplyr)
library(ggplot2)
library(corrplot)
library(ggrepel)
library(plyr)

train <- read.csv("train.csv")
test <- read.csv('test.csv')
train$Id <- NULL
test$Id <- NULL

sort(which(colSums(is.na(train))>0), decreasing = T)
sort(which(colSums(is.na(test))>0), decreasing = T)

ggplot(data = train, aes(x=SalePrice)) + geom_histogram()

##combining both test and train
test$SalePrice <- NA

dk <- rbind(train, test) 
dim(dk)
options(scipen = 999)

sort(which(colSums(is.na(dk))>0), decreasing = T)
ggplot(data = dk, aes(x=SalePrice)) + geom_histogram()
##sale prices are right skewed

num <- which(sapply(dk,is.numeric))
numname <- names(num)
numname

dj <- dj[,!(names(dj) %in% dropVars)]

numdk <- dk[,(names(dk) %in% numname)]
head(numdk,3)
str(numdk)
summary(numdk)

##Correlation

num_cor <- cor(numdk,  use="pairwise.complete.obs")
cor_sort <- as.matrix(sort(num_cor[,"SalePrice"], decreasing = T))

high_cor <- names(which(apply(cor_sort,1, function(x) abs(x)>0.5)))

high_num <- num_cor[high_cor,high_cor]

cor.high <- corrplot.mixed(high_num, tl.col="black" ,tl.pos = "lt")

##OverallQual
table(dk$OverallQual)

ggplot(data = dk[!is.na(dk$SalePrice),], 
       aes(x=factor(OverallQual), y = SalePrice)) + geom_boxplot()

subset(dk, SalePrice>200000 & factor(OverallQual) == 4)

dk <- dk[-458,]


##above Ground liv area
table(dk$GrLivArea)

ggplot(data = dk[!is.na(dk$SalePrice),], 
       aes( x = GrLivArea, y = SalePrice)) + geom_point(color="blue") + 
     geom_smooth(aes(group=1), method= "lm", se=F, color='Black')

subset(dk, SalePrice< 200000 & GrLivArea> 4000)#524, 1299

Nacol <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol], is.na)), decreasing = T)


##Droping Alley, Miscfeature, Poolqc
dk$PoolQC <- NULL
dk$MiscFeature <- NULL
dk$Alley <- NULL

dim(dk)

##Missing Values
##fence
table(dk$Fence)
dk$Fence <- as.character(dk$Fence)
dk$Fence[is.na(dk$Fence)] <- 'None'
table(dk$Fence)

dk$Fence <- as.factor(dk$Fence)


#fireplace
table(dk$Fireplaces)
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
dk$FireplaceQu <- as.character(dk$FireplaceQu)
dk$FireplaceQu[is.na(dk$FireplaceQu)] <- 'None'
dk$FireplaceQu <- as.integer(revalue(dk$FireplaceQu, Qualities))
table(dk$FireplaceQu)

table(dk$Fireplaces)


Nacol.1 <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol.1], is.na)), decreasing = T)

##Lot Variable
##LotFrontage: Linear feet of street connected to property
table(dk$LotFrontage)
str(dk$LotFrontage)

ggplot(data=dk[!is.na(dk$SalePrice),], aes(x=factor(Neighborhood), y =LotFrontage)
      ) + geom_bar(stat = 'summary')


for (i in 1:nrow(dk)){
  if(is.na(dk$LotFrontage[i])){
    dk$LotFrontage[i] <- as.integer(median(dk$LotFrontage[dk$Neighborhood==dk$Neighborhood[i]], na.rm=TRUE)) 
  }
}


##LotShape: General shape of property
##Reg  Regular 
##IR1  Slightly irregular
##IR2  Moderately Irregular
##IR3  Irregular

table(dk$LotShape)
dk$LotShape <- as.character(dk$LotShape)
dk$LotShape <- revalue(dk$LotShape, c('IR3'= 0, 'IR2'=1, 'IR1'=2, 'Reg'=3))


##LotConfig: Lot configuration
##   Inside   Inside lot
##Corner   Corner lot
##CulDSac  Cul-de-sac
##FR2  Frontage on 2 sides of property
##FR3  Frontage on 3 sides of property


dk$LotConfig <- as.factor(dk$LotConfig)
table(dk$LotConfig)

Nacol.1 <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol.1], is.na)), decreasing = T)

##Garage Variables 7 variables related to garages

dk$GarageYrBlt[is.na(dk$GarageYrBlt)] <- dk$YearBuilt[is.na(dk$GarageYrBlt)]

dk[!is.na(dk$GarageType) & is.na(dk$GarageQual), c('GarageFinish', 
                           'GarageQual' ,'GarageCond','GarageType', 'GarageArea')]

##these are test value

dk$GarageCond <- as.character(dk$GarageCond)
table(dk$GarageCond)
dk$GarageCond[2127] <- names(sort(-table(dk$GarageCond)))[1]
dk$GarageQual[2127] <- names(sort(-table(dk$GarageQual)))[1]
dk$GarageFinish[2127] <- names(sort(-table(dk$GarageFinish)))[1]  

dk$GarageCars[2577] <- 0
dk$GarageArea[2577] <- 0
dk$GarageType[2577] <- NA

table(dk$GarageCars)
which(is.na(dk$GarageCars))
dk$GarageCars[2576] <- 2

which(is.na(dk$GarageArea))
table(dk$GarageArea)
summary(dk$GarageArea)
dk$GarageArea[2576] <- 480

##GarageType
table(dk$GarageType)
dk$GarageType <- as.character(dk$GarageType)
dk$GarageType[is.na(dk$GarageType)] <- 'No Garage'

dk$GarageType <- as.factor(dk$GarageType)

##GarageFinish: Interior finish of the garage

dk$GarageFinish <- as.character(dk$GarageFinish)

dk$GarageFinish[is.na(dk$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
dk$GarageFinish<-as.integer(revalue(dk$GarageFinish, Finish))
table(dk$GarageFinish)

##GarageQual: Garage quality


dk$GarageQual <- as.character(dk$GarageQual)
dk$GarageQual[is.na(dk$GarageQual)] <- 'None'
dk$GarageQual<-as.integer(revalue(dk$GarageQual, Qualities))
table(dk$GarageQual)

##Garagecondition
dk$GarageCond <- as.character(dk$GarageCond)
dk$GarageCond[is.na(dk$GarageCond)] <- 'None'
dk$GarageCond<-as.integer(revalue(dk$GarageCond, Qualities))
table(dk$GarageCond)

##Garage qual and cond looks corelated

Nacol.1 <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol.1], is.na)), decreasing = T)

##basement variable 11

dk[!is.na(dk$BsmtFinType1) & (is.na(dk$BsmtCond)|is.na(dk$BsmtQual)
                              |is.na(dk$BsmtExposure)|is.na(dk$BsmtFinType2)),
   c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

#MOST Commom
dk$BsmtFinType2[333] <- names(sort(-table(dk$BsmtFinType2)))[1]
dk$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(dk$BsmtExposure)))[1]
dk$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(dk$BsmtCond)))[1]
dk$BsmtQual[c(2218, 2219)] <- names(sort(-table(dk$BsmtQual)))[1]


dk$BsmtQual <- as.character(dk$BsmtQual)
dk$BsmtQual[is.na(dk$BsmtQual)] <- 'None'
dk$BsmtQual<-as.integer(revalue(dk$BsmtQual, Qualities))
table(dk$BsmtQual)

dk$BsmtCond <- as.character(dk$BsmtCond)
dk$BsmtCond[is.na(dk$BsmtCond)] <- 'None'
dk$BsmtCond<-as.integer(revalue(dk$BsmtCond, Qualities))
table(dk$BsmtCond)

dk$BsmtExposure <- as.character(dk$BsmtExposure)
dk$BsmtExposure[is.na(dk$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
dk$BsmtExposure<-as.integer(revalue(dk$BsmtExposure, Exposure))
table(dk$BsmtExposure)

dk$BsmtFinType1 <- as.character(dk$BsmtFinType1)
dk$BsmtFinType1[is.na(dk$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
dk$BsmtFinType1<-as.integer(revalue(dk$BsmtFinType1, FinType))
table(dk$BsmtFinType1)

dk$BsmtFinType2 <- as.character(dk$BsmtFinType2)
dk$BsmtFinType2[is.na(dk$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
dk$BsmtFinType2<-as.integer(revalue(dk$BsmtFinType2, FinType))
table(dk$BsmtFinType2)

dk[(is.na(dk$BsmtFullBath)|is.na(dk$BsmtHalfBath)|
       is.na(dk$BsmtFinSF1)|is.na(dk$BsmtFinSF2)|is.na(dk$BsmtUnfSF)|is.na(dk$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
      'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

dk$BsmtFullBath[is.na(dk$BsmtFullBath)] <-0
table(dk$BsmtFullBath)

dk$BsmtHalfBath[is.na(dk$BsmtHalfBath)] <-0
table(dk$BsmtHalfBath)

table(dk$BsmtFinSF1)
dk$BsmtFinSF1[is.na(dk$BsmtFinSF1)] <-0

dk$BsmtFinSF2[is.na(dk$BsmtFinSF2)] <-0

dk$BsmtUnfSF[is.na(dk$BsmtUnfSF)] <-0

dk$TotalBsmtSF[is.na(dk$TotalBsmtSF)] <-0


###
Nacol.1 <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol.1], is.na)), decreasing = T)



#masonry 2 variable

length(which(is.na(dk$MasVnrType) & is.na(dk$MasVnrArea)))

dk[is.na(dk$MasVnrType) & !is.na(dk$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

dk$MasVnrType[2611] <- names(sort(-table(dk$MasVnrType)))[2] 

dk$MasVnrType <- as.character(dk$MasVnrType)
dk$MasVnrType[is.na(dk$MasVnrType)] <- 'None'

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
dk$MasVnrType <- as.integer(revalue(dk$MasVnrType, Masonry))
table(dk$MasVnrType)

dk$MasVnrArea[is.na(dk$MasVnrArea)] <-0

#Mszoning
table(dk$MSZoning)
dk$MSZoning[is.na(dk$MSZoning)] <- names(sort(-table(dk$MSZoning)))[1]
dk$MSZoning <- as.factor(dk$MSZoning)
table(dk$MSZoning)

#Kitchen
table(dk$KitchenQual)
dk$KitchenQual <- as.character(dk$KitchenQual)
dk$KitchenQual[is.na(dk$KitchenQual)] <- 'TA' 
dk$KitchenQual<-as.integer(revalue(dk$KitchenQual, Qualities))
table(dk$KitchenQual)

table(dk$KitchenAbvGr)

#utilities
table(dk$Utilities)
dk$Utilities <- NULL

#Home
## Typ  Typical Functionality
##Min1 Minor Deductions 1
#Min2 Minor Deductions 2
#Mod  Moderate Deductions
#Maj1 Major Deductions 1
#Maj2 Major Deductions 2
#Sev  Severely Damaged
#Sal  Salvage only
table(dk$Functional)
dk$Functional[is.na(dk$Functional)] <- names(sort(-table(dk$Functional)))[1]

dk$Functional <- as.integer(revalue(dk$Functional, c('Sal'=0, 'Sev'=1, 
                                                       'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(dk$Functional)

#Exterior
table(dk$Exterior1st)
dk$Exterior1st[is.na(dk$Exterior1st)] <- names(sort(-table(dk$Exterior1st)))[1]
dk$Exterior1st <- as.factor(dk$Exterior1st)
table(dk$Exterior1st)

dk$Exterior2nd[is.na(dk$Exterior2nd)] <- names(sort(-table(dk$Exterior2nd)))[1]
dk$Exterior2nd <- as.factor(dk$Exterior2nd)
table(dk$Exterior2nd)

dk$ExterQual <- as.character(dk$ExterQual)
dk$ExterQual<-as.integer(revalue(dk$ExterQual, Qualities))
table(dk$ExterQual)

dk$ExterCond <- as.character(dk$ExterCond)
dk$ExterCond<-as.integer(revalue(dk$ExterCond, Qualities))
table(dk$ExterCond)

dk$Electrical[is.na(dk$Electrical)] <- names(sort(-table(dk$Electrical)))[1]
dk$Electrical <- as.factor(dk$Electrical)
table(dk$Electrical)

#Salestype
dk$SaleType[is.na(dk$SaleType)] <- names(sort(-table(dk$SaleType)))[1]
dk$SaleType <- as.factor(dk$SaleType)
table(dk$SaleType)

dk$SaleCondition <- as.factor(dk$SaleCondition)
table(dk$SaleCondition)


############################  Categorical

names(dk[,sapply(dk,is.character)])

class(dk$Foundation)
table(dk$Foundation)

class(dk$Heating)

class(dk$HeatingQC)
table(dk$HeatingQC)
dk$HeatingQC<-as.integer(revalue(dk$HeatingQC, Qualities))

class(dk$CentralAir)
table(dk$CentralAir)

dk$CentralAir <- as.integer(revalue(dk$CentralAir, c('Y'=1, 'N'=0)))

dk$LandSlope<-as.integer(revalue(dk$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))

table(dk$Street)
dk$Street<-as.integer(revalue(dk$Street, c('Grvl'=0, 'Pave'=1)))

table(dk$PavedDrive)
dk$PavedDrive<-as.integer(revalue(dk$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))


Nacol.1 <- which(colSums(is.na(dk))>0)

sort(colSums(sapply(dk[Nacol.1], is.na)), decreasing = T)


#######Feature Engineering

##correlation

num.1 <- which(sapply(dk,is.numeric))
numname.1 <- names(num.1)
numname.1

dj.1 <- dj.1[,!(names(dj.1) %in% dropVars)]

numdk.1 <- dk[,(names(dk) %in% numname.1)]
head(numdk.1,3)

num_cor.1 <- cor(numdk.1,  use="pairwise.complete.obs")
cor_sort1 <- as.matrix(sort(num_cor.1[,"SalePrice"], decreasing = T))

high_cor1 <- names(which(apply(cor_sort1,1, function(x) abs(x)>0.5)))

high_num1 <- num_cor.1[high_cor1,high_cor1]

cor.high1 <- corrplot.mixed(high_num1, tl.col="black" ,tl.pos = "lt",
                           tl.cex = 0.7,cl.cex = .7, number.cex=.7)

#droping feature with high co relation
dk$GarageArea <- NULL
dk$TotalRmsAbvGrd <- NULL
dk$GarageYrBlt <- NULL
dk$TotalBsmtSF <- NULL

##YearBuilt: Original construction date
###YearRemodAdd: Remodel date



table(dk$LotShape)
dk$LotShape <- as.integer(dk$LotShape)

table(dk$FireplaceQu)

dk$TotBathrooms <- dk$FullBath + (dk$HalfBath*0.5) + dk$BsmtFullBath + (dk$BsmtHalfBath*0.5)
dk$FullBath <- NULL
dk$HalfBath <- NULL
dk$BsmtFullBath <- NULL
dk$BsmtHalfBath <- NULL

dim(dk)


which(sapply(dk,is.factor))
which(sapply(dk,is.numeric))

head(dk,2)

dk$Age <- as.numeric(dk$YrSold) - dk$YearBuilt

dk$YrSold <- NULL
dk$YearBuilt <- NULL
dk$YearRemodAdd <- NULL

head(dk,2)

dk$TotalPorchSF <- dk$OpenPorchSF + dk$EnclosedPorch + dk$X3SsnPorch + dk$ScreenPorch

dk$OpenPorchSF <- NULL
dk$EnclosedPorch <- NULL
dk$X3SsnPorch <- NULL
dk$ScreenPorch <- NULL

dim(dk)

which(sapply(dk,is.factor))
which(sapply(dk,is.numeric))

names(which(sapply(dk,is.factor)))

library(dplyr)

dkfactor <- select(dk, "MSZoning", "LandContour", "LotConfig","Neighborhood","Condition1",
                   "Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
                   "Exterior1st","Exterior2nd","Foundation","Heating","Electrical",
                   "GarageType","Fence","SaleType","SaleCondition") 

dknum <- select(dk, -c("MSZoning", "LandContour", "LotConfig","Neighborhood","Condition1",
                   "Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
                   "Exterior1st","Exterior2nd","Foundation","Heating","Electrical",
                   "GarageType","Fence","SaleType","SaleCondition")) 

head(dknum,5)
dknum$SalePrice <- NULL
head(dkfactor,5)


###one hot encoding
dkdummies <- as.data.frame(model.matrix(~.-1, dkfactor))
dim(dkdummies)

combined <- cbind(dknum, dkdummies)
dim(combined)
###salesprice Skewness
skew(dk$SalePrice)
dk$SalePrice <- log(dk$SalePrice) 

skew(dk$SalePrice)


train1 <- combined[!is.na(dk$SalePrice),]
test1 <- combined[is.na(dk$SalePrice),]







##### Lasso Regression model
library(caret)
library(gridExtra)
library(ggrepel)

set.seed(123)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=dk$SalePrice[!is.na(dk$SalePrice)], 
                   method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 

lasso_mod$bestTune

min(lasso_mod$results$RMSE)

LassoPred <- predict(lasso_mod, test1)

predictions_lasso <- exp(LassoPred)























