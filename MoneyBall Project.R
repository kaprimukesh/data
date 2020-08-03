batting <- read.csv('Batting.csv')

head(batting,5)
str(batting)

#Batting Average
batting$BA <- batting$H / batting$AB

# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB


str(batting)

#Merging salaries
sal <- read.csv('Salaries.csv')

summary(sal)
summary(batting)

batting <- subset(batting,yearID >= 1985)

combo <- merge(batting,sal,by=c('playerID','yearID'))

summary(combo)

#3 lost players in 2001

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players <- subset(lost_players,yearID == 2001)

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)

#new players
library(dplyr)
avail.players <- filter(combo,yearID==2001)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

avail.players <- filter(avail.players,salary<8000000,OBP>0)
avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
possible

#Player selected

possible[2:4,]


