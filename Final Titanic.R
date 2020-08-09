train <- read.csv("train.csv")
test <- read.csv('test.csv')
test$Survived <- NA

df <- rbind(train,test)


dim(df)
head(df,3)
library(dplyr)
library(pdp)
library(Amelia)
miss <- missmap(df, col = c('yellow', 'black'))

colSums(sapply(df, is.na))
str(df)
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)

###Embarked

filter(df, Embarked == "")
table(df$Embarked)
df$Embarked <- as.character(df$Embarked)
df$Embarked[830] <- 'S'
df$Embarked[62] <- 'S'

df$Embarked <- as.factor(df$Embarked)
table(df$Embarked)


##Fare
which(is.na(df$Fare)) ##1044
median(df$Fare[df$Embarked=='S'], na.rm = T)
mean(df$Fare[df$Embarked=='S'], na.rm = T)

df$Fare[1044] <- 27.5

##Age data missing

library(ggplot2)

ggplot(data = df[!is.na(df$Survived),], aes(x=Survived)) + 
  geom_bar(aes(fill = Survived)) + geom_label(stat = 'count', aes(label=..count..))


##Age and Gender



##Sex/gender

p1 <- ggplot(data = df, aes(x=Sex)) + geom_bar(aes(fill=Sex, stat = 'count'), )+ 
  geom_label(stat = 'count',aes(label=..count..)) +   scale_fill_manual(values = c("pink", "green"))


p2 <- ggplot(data = df[!is.na(df$Survived),], aes(x=Sex, fill=Survived))  +
  geom_bar(stat = 'count' ,position = 'dodge') + 
  geom_label(stat = 'count', aes(label=..count..))

grid.arrange(p1,p2, nrow=1)


##Passenger Class

p3 <- ggplot(data = df, aes(x=Pclass, fill=Pclass)) +
        geom_bar(stat = 'count') + geom_label(stat = 'count', aes(label=..count..))

p4 <- ggplot(data = df[!is.na(df$Survived),], aes(x=Pclass, fill=Survived)) +
  geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label=..count..))


p5 <- ggplot(data = df[!is.na(df$Survived),], aes(x=Pclass, fill=Survived)) +
  geom_bar(stat = 'count', position = 'stack') + facet_grid(.~Sex)

p6 <- ggplot(data = df[!is.na(df$Survived),], aes(x=Pclass, fill=Survived)) +
  geom_bar(stat = 'count', position = 'fill') + facet_grid(.~Sex)

grid.arrange(p3,p4,p5,p6, ncol =2)


##data cleaning

ggplot(data = df, aes(x=Pclass, y=Age)) + geom_boxplot(aes(fill=Pclass))

ggplot(data = df, aes(x=Sex, y=Age)) + geom_boxplot(aes(fill=Sex))

median(df$Age[df$Pclass==1], na.rm = T)
median(df$Age[df$Pclass==2], na.rm = T)
median(df$Age[df$Pclass==3], na.rm = T)


df$Age <- ifelse(df$Pclass == 1 & is.na(df$Age), 39, df$Age)   

df$Age <- ifelse(df$Pclass == 2 & is.na(df$Age), 29, df$Age)   

df$Age <- ifelse(df$Pclass == 3 & is.na(df$Age), 24, df$Age)   


miss <- missmap(df, col = c('yellow', 'black'))

head(df,3)
str(df)
table(df$SibSp)
table(df$Parch)

df$Fsize <- df$SibSp+df$Parch +1

df$SibSp <- NULL
df$Parch <- NULL
table(df$Fsize)
###
str(df$Sex)
df$Sex <- as.character(df$Sex)
df$Sex[df$Sex=='male'] <- 1
df$Sex[df$Sex=='female'] <- 0
table(df$Sex)
df$Sex <- as.factor(df$Sex)

head(df,5)
df <- select(df, -Cabin, -Name,-Ticket,-PassengerId)


df.train <- df[!is.na(df$Survived),]

df.test <- df[is.na(df$Survived),]

df.test$Survived <- NULL


str(df.train)


##Random Forest

library(randomForest)
library(caret)
library(e1071)

caret_matrix <- train(x= df.train[,-1], y=df.train$Survived,data=df.train,
                      method='rf', trControl=trainControl(method="cv", number=5))

caret_matrix

caret_matrix$results
varImp(caret_matrix)

y_pred2 <- predict(caret_matrix, df.test)
y_pred2

solution <- data.frame(PassengerID = test$PassengerId, Survived = y_pred2)


write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)




