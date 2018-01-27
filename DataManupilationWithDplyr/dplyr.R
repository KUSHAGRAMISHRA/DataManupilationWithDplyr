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

top = wine %>% group_by(country)%>% summarise(median = median(points))%>% arrange(desc(median))
class(top)
top
top = as.character(top$country)
top

# to find the countries in both the "top" list and "selected_country" list

both = intersect(top,selected_countries)
both

# to find the top 10 variety of wine

topwine= wine %>% group_by(variety) %>% summarise(number = n()) %>% arrange(desc(number))%>% top_n(10)
topwine= as.character(topwine$variety)
topwine

# tofind the wines which are of top quality and the cheapest

top15p = wine %>% arrange(desc(points)) %>% filter(points>quantile(points,prob = .85))
cheap15p = wine %>% arrange(price) %>% head(nrow(top15p))
goodvalue = intersect(top15p,cheap15p)
goodvalue

# to create new columns in the dataset
# here we are creating a column ppratio(point to price ratio)

wine %>% mutate(ppratio=points/price)
# this line will not make any stable changes in the daataset, to make a permanent change we will have to equate
#it with the variable
wine = wine %>% mutate(ppratio=points/price)

#next line uses transmute function which only keeps the mentioned column and drops all the others 
wine %>% transmute(ppratio = points/price)

#compute the missing values
wine[wine$country=="",]

wine$country=ifelse(wine$designation=="Askitikos","Greece",wine$country)
wine$country=ifelse(wine$designation=="Piedra Feliz","Chile",wine$country)
wine$country=ifelse(wine$country=="","Turkey",wine$country)

#creating new tables by making joins

newline = wine%>%group_by(country)%>%summarise(total=n())%>%arrange(desc(total))
subset1=head(wine)
subset2=head(newline)
full=full_join(subset1,subset2)
inner=inner_join(subset1,subset2)
left=left_join(subset1,subset2)
right=right_join(subset1,subset2)
