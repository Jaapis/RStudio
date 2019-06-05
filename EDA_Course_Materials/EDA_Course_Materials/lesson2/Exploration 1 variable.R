library(ggplot2)
data(diamonds)
summary(diamonds)
diamonds$color
?diamonds

qplot(diamonds$price, geom="histogram") 

sum(diamonds$price < 500)

sum(diamonds$price < 250)

sum(diamonds$price >= 15000)


qplot(diamonds$price, 
      geom="histogram",
      bins = 50) 
+  xlim(0, 500)

qplot(x = price, data = diamonds) + 
  facet_wrap(~cut, ncol=5)

aggregate(price~cut, diamonds, max)
aggregate(price~cut, diamonds, min)
aggregate(price~cut, diamonds, median)

qplot(x = price, data = diamonds) + facet_wrap(~cut)

qplot(x = price, y = carat, data = diamonds) + 
  facet_wrap(~cut, ncol=5) + 
  scale_x_log10()

qplot(cut, price, data=diamonds, 
      geom=c("boxplot"))

by(diamonds$price, diamonds$color, summary)

IQR(subset(diamonds, color == "J")$price)
IQR(subset(diamonds, color == "D")$price)

qplot(cut, price, data=diamonds, 
      geom=c("boxplot")) +
  facet_wrap(~color, ncol=3)
  
qplot(x = carat, 
      data = diamonds,
      geom = 'freqpoly',
      binwidth = 0.05
      )



