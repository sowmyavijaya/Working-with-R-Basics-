library(readr)
library(ggplot2)
library(maps)

wine <- read_csv("Wine data.csv")
View(wine)
wine_qual <- read_csv("wineQualitycomp_2.csv")
View(wine_qual)

library(dplyr)
wine_full <- inner_join(wine,wine_qual,by='title')
View(wine_full)

medianprice <- round(median(wine_full$price,na.rm = T),0)
wine_full$price[is.na(wine_full$price)] <- medianprice

wine_full$country[wine_full$country=='United Kingdom'] <- 'UK'
wine_full$country[wine_full$country=='US'] <- 'USA'

#Q1 Participation distribution of countries

Worlddata <- map_data('world')
View(Worlddata)

a <- ggplot(data=wine,aes(x=wine$country,fill=length(wine$title))) + geom_bar() + coord_flip() + scale_fill_continuous()
a <- a + theme(legend.position="none") + ggtitle("Countries participated with number of wines presented") + ylab('Wine Count') + xlab('Countries')
a + theme(plot.title = element_text(size = 20, face = "bold")) + theme(axis.title.y = element_text(colour="black",size=15,face="bold"),axis.title.x = element_text(colour="black",size=15,face="bold"))

a + coord_polar("y", start=0) # iteration

#Q2 Average points for wine by countries
library(sqldf)
grpby <- sqldf('select country,avg(points) as points from wine_full GROUP BY country')

map1 <- ggplot(grpby,aes(map_id = country)) + geom_map(aes(fill= points),map = Worlddata) + expand_limits(x= Worlddata$long,y=Worlddata$lat) + scale_fill_continuous(low='lightgreen',high='darkgreen') + theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  ggtitle("Country with highest ratings for wine") +
  theme(plot.title = element_text(size = 20, face = "bold"))
map1

Barplot <- ggplot(data=grpby,aes(x=country,y=points))+ geom_bar(stat='identity',aes(fill=points)) + theme(panel.background = element_rect(fill = "grey"))
Barplot <- Barplot + coord_flip() + scale_fill_gradient(low = "darkgreen", high = "darkblue") + ggtitle("Country with highest ratings for wine") + xlab("Countries")
Barplot + theme(plot.title = element_text(size = 20, face = "bold")) + theme(axis.title.y = element_text(colour="black",size=15,face="bold"),axis.title.x = element_text(colour="black",size=15,face="bold"))

#Q3 Distribution of rating versus prices

ggplot(wine_full,aes(x=points,y=price)) + geom_line() #iteration

gg1 <- ggplot(wine_full,aes(x=points,y=price)) + geom_point(colour='blue') + ggtitle("Does rating depend on price of wine") + xlab("Points") + ylab("Price")
gg1 <- gg1 +theme(plot.title = element_text(size = 20, face = "bold")) + theme(axis.title.y = element_text(colour="black",size=15,face="bold"),axis.title.x = element_text(colour="black",size=15,face="bold"))
gg1 + geom_smooth(method='lm',se=F,colour="Red")

#Q4 Country with wine having highest alcohol content

library(sqldf)
country_qual <- sqldf('select country,avg(alcohol) as alcohol from wine_full GROUP BY country')
country_qual

gg2 <- ggplot(country_qual,aes(x=country,y=alcohol)) + geom_point() + coord_flip()
gg2 <- gg2 + ggtitle("Countries with highest and lowest alcohol % in wine") + ylab("Average alcohol % in wine") + xlab("Countries")
gg2 <- gg2 +theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(axis.title.y = element_text(colour="black",size=15,face="bold"),axis.title.x = element_text(colour="black",size=15,face="bold"))
gg2

ggplot(country_qual,aes(x=country,y=alcohol)) + geom_bar(stat='identity',aes(fill=alcohol)) # iteration

