train <- read.csv('train.csv')
test <- read.csv('test.csv')


library(Amelia)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggrepel)
library(plyr)


#Structure

test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA
dfk <- rbind(train,test)
dim(dfk)

options(scipen = 999)

ggplot(data = dfk[!is.na(dfk$SalePrice),], aes(x= SalePrice)) + 
  geom_histogram(fill="pink", color='Black', binwidth = 10000) + 
  scale_x_continuous(breaks = seq(0,800000, by=100000))

summary(dfk$SalePrice)


##Correlations

numericVars <- which(sapply(dfk, is.numeric))
numericVarNames <- names(numericVars)
length(numericVars)

dfk_numVar <- dfk[, numericVars]

cor_numVar <- cor(dfk_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

CorHigh <- names(which(apply(cor_sorted,1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#Overdfk Quality

ggplot(data = dfk[!is.na(dfk$SalePrice),], 
       aes(x=factor(OverdfkQual), y = SalePrice)
) + geom_boxplot(col="darkblue") + labs(x="Quality") +
  scale_y_continuous(breaks = seq(0,800000, 100000))

#Above Grade (Ground) Living Area
ggplot(data=dfk[!is.na(dfk$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000)) +
  geom_text_repel(aes(label = ifelse(dfk$GrLivArea[!is.na(dfk$SalePrice)]>4500, rownames(dfk), '')))

dfk[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverdfkQual')]


#Missing data

NAcol <- which(colSums(is.na(dfk)) > 0)

sort(colSums(sapply(dfk[NAcol], is.na)), decreasing = TRUE)
str(dfk$GarageFinish)
length(NAcol)

#Poolqc
str(dfk$PoolQC)

dfk$PoolQC <- as.character(dfk$PoolQC)

dfk$PoolQC[is.na(dfk$PoolQC)] <- 'None'

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

dfk$PoolQC<-as.integer(revalue(dfk$PoolQC, Qualities))

table(dfk$PoolQC)


dfk[(dfk$PoolArea >0) & (dfk$PoolQC)==0,
    c('PoolQC','PoolArea','OverdfkQual')]

#based on overdfk quality

dfk$PoolQC[2421] <- 2
dfk$PoolQC[2504] <- 3
dfk$PoolQC[2600] <- 2

#misc
table(dfk$MiscFeature)
summary(dfk$MiscFeature)
any(is.na(dfk$MiscFeature))

dfk$MiscFeature <- as.character(dfk$MiscFeature)
dfk$MiscFeature[is.na(dfk$MiscFeature)] <- 'None'

dfk$MiscFeature <- as.factor(dfk$MiscFeature)

#dfkey
table(dfk$dfkey)
summary(dfk$dfkey)

dfk$dfkey <- as.character(dfk$dfkey)
dfk$dfkey[is.na(dfk$dfkey)] <- 'None'
dfk$dfkey <- as.factor(dfk$dfkey)

ggplot(dfk[!is.na(dfk$SalePrice),], aes(x=dfkey, y=SalePrice)) +
  geom_bar(fill='blue',  stat="summary")

#Fence

dfk$Fence <- as.character(dfk$Fence)
dfk$Fence[is.na(dfk$Fence)] <- 'None'
table(dfk$Fence)

dfk$Fence <- as.factor(dfk$Fence)

#fireplace
dfk$FireplaceQu <- as.character(dfk$FireplaceQu)
dfk$FireplaceQu[is.na(dfk$FireplaceQu)] <- 'None'
dfk$FireplaceQu<-as.integer(revalue(dfk$FireplaceQu, Qualities))
table(dfk$FireplaceQu)

table(dfk$Fireplaces)

#LOT

summary(dfk$LotFrontage)
any(is.na(dfk$Neighborhood))
ggplot(dfk[!is.na(dfk$LotFrontage),], 
       aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fill='pink')

for(i in 1:nrow(dfk)){
  if(is.na(dfk$LotFrontage[i])){
   dfk$LotFrontage[i] <- 
  as.integer(median(dfk$LotFrontage[dfk$Neighborhood==dfk$Neighborhood[i]], 
             na.rm=TRUE)) 
  }
}

#Lotshape
dfk$LotShape <- as.character(dfk$LotShape)
dfk$LotShape<-as.integer(revalue(dfk$LotShape, 
                                 c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))

table(dfk$LotShape)

#lot config
ggplot(dfk[!is.na(dfk$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='darkgrey') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


dfk$LotConfig <- as.factor(dfk$LotConfig)
table(dfk$LotConfig)


#GarageType: Garage location
#GarageYrBlt: Year garage was built
#GarageFinish: Interior finish of the garage
#GarageCars: Size of garage in car capacity
#GarageArea: Size of garage in square feet
#GarageQual: Garage quality
#GarageCond: Garage condition

length(which(dfk$GarageYrBlt == dfk$YearBuilt))

dfk$GarageYrBlt[is.na(dfk$GarageYrBlt)] <- dfk$YearBuilt[is.na(dfk$GarageYrBlt)]

length(which(is.na(dfk$GarageType) & 
               is.na(dfk$GarageFinish) & is.na(dfk$GarageCond) & is.na(dfk$GarageQual)))

dfk[!is.na(dfk$GarageType) & is.na(dfk$GarageFinish),
    c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]

table(dfk$GarageCond)
table(dfk$GarageQual)
table(dfk$GarageFinish)

dfk$GarageCond[2127] <- names(sort(-table(dfk$GarageCond)))[1]
dfk$GarageQual[2127] <- names(sort(-table(dfk$GarageQual)))[1]
dfk$GarageFinish[2127] <- names(sort(-table(dfk$GarageFinish)))[1]
dfk$GarageCars[2577] <- 0
dfk$GarageArea[2577] <- 0
dfk$GarageType[2577] <- NA

table(dfk$GarageType)
length(which(is.na(dfk$GarageType) & is.na(dfk$GarageFinish) & 
               is.na(dfk$GarageCond) & is.na(dfk$GarageQual)))


#Garage Type
dfk$GarageType <- as.character(dfk$GarageType)
dfk$GarageType[is.na(dfk$GarageType)] <- 'No Garage'
dfk$GarageType <- as.factor(dfk$GarageType)
table(dfk$GarageType)


#Garagefinish
dfk$GarageFinish <- as.character(dfk$GarageFinish)

dfk$GarageFinish[is.na(dfk$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
dfk$GarageFinish<-as.integer(revalue(dfk$GarageFinish, Finish))
table(dfk$GarageFinish)

#GarageQual

dfk$GarageQual <- as.character(dfk$GarageQual)
dfk$GarageQual[is.na(dfk$GarageQual)] <- 'None'
dfk$GarageQual<-as.integer(revalue(dfk$GarageQual, Qualities))
table(dfk$GarageQual)

#Garage Cond
dfk$GarageCond <- as.character(dfk$GarageCond)
dfk$GarageCond[is.na(dfk$GarageCond)] <- 'None'
dfk$GarageCond<-as.integer(revalue(dfk$GarageCond, Qualities))
table(dfk$GarageCond)

#Basement
length(which(is.na(dfk$BsmtQual) & is.na(dfk$BsmtCond) &
               is.na(dfk$BsmtExposure) & is.na(dfk$BsmtFinType1) & is.na(dfk$BsmtFinType2)))

dfk[!is.na(dfk$BsmtFinType1) & (is.na(dfk$BsmtCond)|is.na(dfk$BsmtQual)
                                |is.na(dfk$BsmtExposure)|is.na(dfk$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

#MOST Commom
dfk$BsmtFinType2[333] <- names(sort(-table(dfk$BsmtFinType2)))[1]
dfk$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(dfk$BsmtExposure)))[1]
dfk$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(dfk$BsmtCond)))[1]
dfk$BsmtQual[c(2218, 2219)] <- names(sort(-table(dfk$BsmtQual)))[1]

dfk$BsmtCond <- as.character(dfk$BsmtCond)
dfk$BsmtQual <- as.character(dfk$BsmtQual)
dfk$BsmtQual[is.na(dfk$BsmtQual)] <- 'None'

dfk$BsmtQual<-as.integer(revalue(dfk$BsmtQual, Qualities))
table(dfk$BsmtQual)

dfk$BsmtCond[is.na(dfk$BsmtCond)] <- 'None'
dfk$BsmtCond<-as.integer(revalue(dfk$BsmtCond, Qualities))
table(dfk$BsmtCond)

dfk$BsmtExposure <- as.character(dfk$BsmtExposure)
dfk$BsmtExposure[is.na(dfk$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

dfk$BsmtExposure<-as.integer(revalue(dfk$BsmtExposure, Exposure))
table(dfk$BsmtExposure)

dfk$BsmtFinType1 <- as.character(dfk$BsmtFinType1)
dfk$BsmtFinType1[is.na(dfk$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
dfk$BsmtFinType1<-as.integer(revalue(dfk$BsmtFinType1, FinType))
table(dfk$BsmtFinType1)

dfk$BsmtFinType2 <- as.character(dfk$BsmtFinType2)
dfk$BsmtFinType2[is.na(dfk$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

dfk$BsmtFinType2<-as.integer(revalue(dfk$BsmtFinType2, FinType))
table(dfk$BsmtFinType2)

dfk[(is.na(dfk$BsmtFullBath)|is.na(dfk$BsmtHalfBath)|
       is.na(dfk$BsmtFinSF1)|is.na(dfk$BsmtFinSF2)|is.na(dfk$BsmtUnfSF)|is.na(dfk$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
      'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

dfk$BsmtFullBath[is.na(dfk$BsmtFullBath)] <-0
table(dfk$BsmtFullBath)

dfk$BsmtHalfBath[is.na(dfk$BsmtHalfBath)] <-0
table(dfk$BsmtHalfBath)

dfk$BsmtFinSF1[is.na(dfk$BsmtFinSF1)] <-0
table(dfk$BsmtFinSF1)

dfk$BsmtFinSF2[is.na(dfk$BsmtFinSF2)] <-0
table(dfk$BsmtFinSF2)

dfk$BsmtUnfSF[is.na(dfk$BsmtUnfSF)] <-0

dfk$TotalBsmtSF[is.na(dfk$TotalBsmtSF)] <-0


#masonry

length(which(is.na(dfk$MasVnrType) & is.na(dfk$MasVnrArea)))

dfk[is.na(dfk$MasVnrType) & !is.na(dfk$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

dfk$MasVnrType[2611] <- names(sort(-table(dfk$MasVnrType)))[2] 

dfk$MasVnrType <- as.character(dfk$MasVnrType)
dfk$MasVnrType[is.na(dfk$MasVnrType)] <- 'None'

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
dfk$MasVnrType <- as.integer(revalue(dfk$MasVnrType, Masonry))
table(dfk$MasVnrType)

dfk$MasVnrArea[is.na(dfk$MasVnrArea)] <-0

#Mszoning
table(dfk$MSZoning)
dfk$MSZoning[is.na(dfk$MSZoning)] <- names(sort(-table(dfk$MSZoning)))[1]
dfk$MSZoning <- as.factor(dfk$MSZoning)
table(dfk$MSZoning)

#Kitchen
table(dfk$KitchenQual)
dfk$KitchenQual <- as.character(dfk$KitchenQual)
dfk$KitchenQual[is.na(dfk$KitchenQual)] <- 'TA' 
dfk$KitchenQual<-as.integer(revalue(dfk$KitchenQual, Qualities))
table(dfk$KitchenQual)

table(dfk$KitchenAbvGr)

#utilities
table(dfk$Utilities)
dfk$Utilities <- NULL

#Home

dfk$Functional[is.na(dfk$Functional)] <- names(sort(-table(dfk$Functional)))[1]

dfk$Functional <- as.integer(revalue(dfk$Functional, c('Sal'=0, 'Sev'=1, 
                                                       'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(dfk$Functional)

#Exterior
dfk$Exterior1st[is.na(dfk$Exterior1st)] <- names(sort(-table(dfk$Exterior1st)))[1]
dfk$Exterior1st <- as.factor(dfk$Exterior1st)
table(dfk$Exterior1st)

dfk$Exterior2nd[is.na(dfk$Exterior2nd)] <- names(sort(-table(dfk$Exterior2nd)))[1]
dfk$Exterior2nd <- as.factor(dfk$Exterior2nd)
table(dfk$Exterior2nd)

dfk$ExterQual <- as.character(dfk$ExterQual)
dfk$ExterQual<-as.integer(revalue(dfk$ExterQual, Qualities))
table(dfk$ExterQual)

dfk$ExterCond <- as.character(dfk$ExterCond)
dfk$ExterCond<-as.integer(revalue(dfk$ExterCond, Qualities))
table(dfk$ExterCond)

dfk$Electrical[is.na(dfk$Electrical)] <- names(sort(-table(dfk$Electrical)))[1]
dfk$Electrical <- as.factor(dfk$Electrical)
table(dfk$Electrical)

#Salestype
dfk$SaleType[is.na(dfk$SaleType)] <- names(sort(-table(dfk$SaleType)))[1]
dfk$SaleType <- as.factor(dfk$SaleType)
table(dfk$SaleType)

dfk$SaleCondition <- as.factor(dfk$SaleCondition)
table(dfk$SaleCondition)

##Character
summary(dfk)
str(dfk)

dfk$HeatingQC <- as.character(dfk$HeatingQC)
dfk$HeatingQC<-as.integer(revalue(dfk$HeatingQC, Qualities))

dfk$CentralAir <- as.character(dfk$CentralAir)
dfk$CentralAir<-as.integer(revalue(dfk$CentralAir, c('N'=0, 'Y'=1)))

table(dfk$CentralAir)
table(dfk$RoofStyle)

dfk$LandSlope <- as.character(dfk$LandSlope)
dfk$LandSlope<-as.integer(revalue(dfk$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(dfk$LandSlope)

dfk$Street <- as.character(dfk$Street)
dfk$Street<-as.integer(revalue(dfk$Street, c('Grvl'=0, 'Pave'=1)))
table(dfk$Street)


dfk$PavedDrive <- as.character(dfk$PavedDrive)
dfk$PavedDrive<-as.integer(revalue(dfk$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(dfk$PavedDrive)


sort(colSums(sapply(dfk, is.na)), decreasing = TRUE)

dfk$MoSold <- as.factor(dfk$MoSold)
dfk$MSSubClass <- as.factor(dfk$MSSubClass)

numericVars <- which(sapply(dfk, is.numeric)) 
factorVars <- which(sapply(dfk, is.factor))
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')


##Corelation
dfk_numVar <- dfk[, numericVars]
cor_numVar <- cor(dfk_numVar, use="pairwise.complete.obs")

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", 
               tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


##Feature Engineering
#YearBuilt: Original construction date
#YearRemodAdd: Remodel date

dfk$Remod <- ifelse(dfk$YearBuilt==dfk$YearRemodAdd, 0, 1)

dfk$Age <- as.numeric(dfk$YrSold)-dfk$YearRemodAdd

ggplot(data = dfk[!is.na(dfk$SalePrice),], 
       aes(x=Age, y=SalePrice)) + 
   geom_point(color = 'skyblue') + 
  geom_smooth(aes(group=1),se=FALSE,method = "lm")

cor(dfk$SalePrice[!is.na(dfk$SalePrice)], dfk$Age[!is.na(dfk$SalePrice)])

dfk$IsNew <- ifelse(dfk$YrSold==dfk$YearBuilt, 1, 0)
table(dfk$IsNew)

dfk$YrSold <- as.factor(dfk$YrSold)

#neighbourgood

ggplot(data = dfk[!is.na(dfk$SalePrice),],
       aes(x= reorder(Neighborhood,SalePrice, FUN=median),y=SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = "median") + 
  scale_y_continuous(breaks = seq(0,350000, by=50000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = 'red') 


dfk$NeighRich[dfk$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
dfk$NeighRich[!dfk$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
dfk$NeighRich[dfk$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

table(dfk$NeighRich)


##
##WoodDeckSF: Wood deck area in square feet
##OpenPorchSF: Open porch area in square feet
##EnclosedPorch: Enclosed porch area in square feet
##3SsnPorch: Three season porch area in square feet
##ScreenPorch: Screen porch area in square feet


dfk$TotalPorchSF <- dfk$OpenPorchSF + dfk$EnclosedPorch +
  dfk$X3SsnPorch + dfk$ScreenPorch

#Correlation
corrplot.mixed(cor_numVar, tl.col="black", 
               tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

#Garage yr bulit and year built 0.85
#totrmaboveground and Grliving area 0.81
#X1stfloor and totalbsmt floor 0.8
#Garagearea and garagecars 0.89
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

dfk <- dfk[,!(names(dfk) %in% dropVars)]



##Preprocessing

numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 
                                  'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))]

numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- dfk[, names(dfk) %in% numericVarNames]
DFfactors <- dfk[, !(names(dfk) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']
dim(DFnumeric)
dim(DFfactors)
str(DFfactors)
str(DFnumeric)
sum(is.na(DFfactors$Alley))

DFfactors$Alley <- NULL

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')


###categorical vairable
#one hot encoding

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))

## Removing levels with few or no observations 
ZerocolTest <- which(colSums(DFdummies[(nrow(dfk[!is.na(dfk$SalePrice),])+1):nrow(dfk),])==0)
colnames(DFdummies[ZerocolTest])

DFdummies <- DFdummies[,-ZerocolTest]

ZerocolTrain <- which(colSums(DFdummies[1:nrow(dfk[!is.na(dfk$SalePrice),]),])==0)

colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]

fewOnes <- which(colSums(DFdummies[1:nrow(dfk[!is.na(dfk$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] 
dim(DFdummies)

combined <- cbind(DFnumeric, DFdummies)

#skewness

library(psych) 
skew(dfk$SalePrice)
qqnorm(dfk$SalePrice)
qqline(dfk$SalePrice)

dfk$SalePrice <- log(dfk$SalePrice) 

skew(dfk$SalePrice)
qqnorm(dfk$SalePrice)
qqline(dfk$SalePrice)


#regression
train1 <- combined[!is.na(dfk$SalePrice),]
test1 <- combined[is.na(dfk$SalePrice),]

dim(combined)
dim(dfk)
library(caret)
library(gridExtra)

set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=dfk$SalePrice[!is.na(dfk$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred)
head(predictions_lasso)

id <- data.frame(seq(1461,2919 , by=1))
sub <- data.frame(id, predictions_lasso)
head(sub)
rename(sub, seq.1461..2919..by...1.= ID, predictions_lasso= SalePrice )

sub$ID <- sub$seq.1461..2919..by...1.
sub$SalePrice <- sub$predictions_lasso

sub$seq.1461..2919..by...1. <- NULL
sub$predictions_lasso <- NULL

directory <-getwd()
directory
write.csv(sub, "C:/Users/Kapri/Desktop/Jose")

install.packages("xlsx")
library(xlsx)
write.xlsx(sub, "subm.xlsx")


