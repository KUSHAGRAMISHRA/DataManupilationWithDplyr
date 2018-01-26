#preparing the data

wine = read.csv("wine.csv", stringsAsFactors = FALSE)
wine = wine[,-c(1,3)]

#install.packages("dplyr")
library(dplyr)

#group_by function

wine %>% group_by(country) %>% summarize(count = n()) %>% arrange(desc(count))
q= wine %>% group_by(country) %>% summarize(count = n()) %>% arrange(desc(count))

selected_countries = wine %>% group_by(country) %>% summarize(count = n()) %>% arrange(desc(count))%>%top_n(10)%>%select(country)
selected_countries

#to find the count of a particular country ex US
length(which(wine$country=="US"))

#checking the data type of the variable

class(selected_countries)
#converting it to character
selected_countries=as.character(selected_countries$country)
class(selected_countries)

select_point = wine %>% filter(country %in% selected_countries)%>%select(country,points)%>%arrange(country)

#plotting some basic graphs\

#graph between points and price

#install.packages("ggplot2")
library(ggplot2)

ggplot(wine,aes(points,price))+geom_point()+geom_smooth()
ggplot(wine,aes(price,points))+geom_point()+geom_smooth()

#graph for top ten wine producers

ggplot(select_point,aes(x= reorder(country,points,median),y=points))+geom_boxplot(aes(fill=country))+xlab("country")+ylab("points")+
ggtitle("distribution of top 10 wine producing countries")+theme(plot.title = element_text(hjust = .5))  

#to find the countries which produce high quality wine but are not mass producers

wine %>% filter(!(country %in% selected_countries)) %>% group_by(country)%>% summarize(median = median(points)) %>% arrange(desc(median))
