install.packages('ggplot2')
install.packages('GGally')
install.packages('Rmisc')
library(ggplot2)
library(Rmisc)
library(GGally)

#Plot1-2
mpgdata <- mpg
mpgdata$manufacturer <- as.factor(mpg$manufacturer)
plot <- ggplot(data = mpg)

plot+
  geom_bar(aes(x=manufacturer),
           stat = 'count',
           fill='white',
           color='black')+
  ggtitle('Manufacturer Count')+
  xlab('Manufacturer')+
  ylab('Count')

plot+
  geom_histogram(aes(x=class),
                 stat = 'count')+
  ggtitle('Car Types')+
  xlab('Class')+
  ylab('Count')

#Plot3
housing <- txhousing[txhousing$city=='Abilene',]

plot3 <- ggplot(data = housing, aes(x=date,
                                    y=volume))
plot3+
  geom_line()+
  geom_smooth(method=lm,
              se=FALSE)+
  ggtitle('Housing market trading volume over time')+
  xlab('Date')+
  ylab('Volume')

#Plot4-5
plot4 <- ggplot(data = iris, aes(x=Sepal.Length,
                                 y=Petal.Length))
p1 <- plot4+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle('Sepal Length V.S. Petal Length')

plot5 <- ggplot(data = iris, aes(x=Sepal.Width,
                                 y=Petal.Width))
p2 <- plot5+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle('Sepal Width V.S. Petal Width')

multiplot(p1,p2)

#Plot6
plot6 <- ggplot(data = iris, aes(x=Species,
                                 y=Sepal.Length))
plot6+
  geom_boxplot()+
  ggtitle('Boxplot of Species-Sepal.Length')

#Plot7
ggpairs(data = iris)
