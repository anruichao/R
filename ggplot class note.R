movies = read.csv('MovieRatings.csv',header = T)
library(dplyr)
movies = movies %>% rename(crate = Rotten.Tomatoes.Ratings,arate = Audience.Ratings, budget=Budget..million, year= Year.of.release)
names(movies)
movies$year = factor(movies$year)
str(movies)
install.packages("ggplot2")
library(ggplot2)
## aesthetic
ggplot(data = movies, aes(x=crate,y=arate))
ggplot(data = movies, aes(x=crate,y=arate))+geom_point()
ggplot(movies,aes(x=Genre))+geom_bar()
ggplot(movies,aes(x=crate,y=arate))+geom_line()
ggplot(movies,aes(x=crate,y=arate))+geom_point(color='dark red')
ggplot(movies,aes(x=crate,y=arate))+geom_point(aes(color=Genre))
ggplot(movies,aes(x=crate,y=arate))+geom_point(aes(color=Genre),size=3)
ggplot(movies,aes(x=crate,y=arate))+geom_point(aes(color=Genre,size=budget))
#layer
p = ggplot(movies,aes(x=crate,y=arate))
p + geom_line(aes(color=Genre,size=budget))+geom_point(aes(color=Genre,size=budget))
s = ggplot(movies,aes(x=budget))
s+geom_histogram(bins = 10)
s+geom_histogram(bins = 10,binwidth = 10,fill='light blue')
s+geom_histogram(bins = 10,binwidth = 10,aes(fill=Genre),color='black')

s+geom_density()
s+geom_density(aes(fill=Genre),position='stack')

u=ggplot(movies,aes(x=crate,y=arate,color=Genre))
u+geom_point()+geom_smooth(fill=NA)
v = ggplot(movies,aes(x=Genre,y=arate,color=Genre))
v+geom_boxplot()
v+geom_point(position = 'jitter')+geom_boxplot(alpha=0.5)

ggplot(movies,aes(x=Genre))+geom_bar(stat = 'count')
ggplot(movies,aes(x=Genre,y=crate))+geom_bar(stat = 'identity')
ggplot(movies,aes(x=Genre,y=crate))+geom_bar(stat = 'summary',fun.y='mean')
movies%>%group_by(Genre)%>%summarise(mean=mean(crate))


a = ggplot(movies,aes(x=budget))
a+geom_histogram(binwidth = 10,aes(fill=Genre),color='grey')+facet_grid(Genre~.,scales = 'free')
a+geom_histogram(binwidth = 10,aes(fill=Genre),color='grey')+facet_grid(.~Genre,scales = 'free')

m = ggplot(movies,aes(x=crate,y=arate,color=Genre,size=budget))
m+geom_point()
m+geom_point()+xlim(50,100)+ylim(50,100)

h = ggplot(movies,aes(x=budget))+geom_histogram(aes(fill=Genre),binwidth = 10,color='grey')
h+xlab('B')+ylab('number of movies')
h+xlab('B')+ylab('number of movies')+
  theme(axis.title.x = element_text(color = 'red'),axis.title.y = element_text(color = 'pink'),legend.text = element_text(size=12),legend.title = element_text(size=15),legend.position = c(1,1),legend.justification = c(1,1))+
  ggtitle("movies")
