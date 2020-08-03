library(ggplot2)
library(data.table)
df <- fread('Economist_Assignment_Data.csv',drop=1)
any(is.na(df))

pl <- ggplot(df,aes(x=CPI,y=HDI,color=Region)) + geom_point(size=4,shape=1)
pl
pl + geom_smooth(aes(group=1))

pl2 <- pl + geom_smooth(aes(group=1),
                        method ='lm',formula = y~log(x),se=FALSE,color='red')
pl2
pl2 + geom_text(aes(label=Country))

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE) + theme_bw()

pl4 <- pl3 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                                limits = c(.9, 10.5),breaks=1:10) 

pl5 <- pl4 + scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                                limits = c(0.2, 1.0))

pl5 + ggtitle("Corruption and Human development")

