#EDA Problem Set 3

library("ggplot2")
data(diamonds)
dim(diamonds)

summary(diamonds)

summary(diamonds$price)

dim(subset(diamonds, price <500))
dim(subset(diamonds, price <250))
dim(subset(diamonds, price >=15000))


qplot(x = price, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 100) + 
scale_x_continuous(limits= c(0,2400), breaks=seq(0,2400,100))

qplot(x =price, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 100) +
  facet_wrap(~cut)

by(diamonds$price,diamonds$cut,summary, digits = max(getOption('digits')))

qplot(x = price, data = diamonds) + facet_wrap(~cut)

qplot(x =price, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 200) +
facet_wrap(~cut, scales="free_y")

qplot(x =price/carat, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 400) +
facet_wrap(~cut, scales="free_y") 

qplot(x =log10(price/carat), data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 0.05) +
facet_wrap(~cut, scales="free_y") 

qplot(x = color, y = price, data = diamonds, geom = "boxplot") + 
coord_cartesian(ylim = c(0,8000))

qplot(x = cut, y = price, data = diamonds, geom = "boxplot")  + 
coord_cartesian(ylim = c(0,7000))

qplot(x = clarity, y = price, data = diamonds, geom = "boxplot") + 
coord_cartesian(ylim = c(0,7000))

by(diamonds$price,diamonds$color,summary)

qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot") 

qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot") + 
coord_cartesian(ylim = c(0,6000))

qplot(x = carat,data = diamonds,binwidth =0.01,geom = 'freqpoly') + 
scale_x_continuous(lim = c(0,3), breaks = seq(0,3,0.3))

qplot(x = carat, y= ..count../sum(..count..), data = diamonds, binwidth =0.01, 
      geom = 'freqpoly') + 
  scale_x_continuous(lim = c(0,3), breaks = seq(0,3,0.3))

  