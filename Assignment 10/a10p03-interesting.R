#
#
#    Assignment 10, Part 3
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    March 27th, 2015
#   – CHECK THE RESIDUAL PLOTS - ALWAYS
#
############################################

setwd("/Users/Saida/Desktop/STAT/Assignment 10")
library(ggplot2)

interesting<-read.csv('interesting2.csv',as.is=TRUE,
                 strip.white=TRUE,header=TRUE,fill=TRUE)

plot1 <- ggplot(data=interesting, aes(x=x1, y=y))+
  geom_point()
plot1

plot2 <- ggplot(data=interesting, aes(x=x2, y=y))+
  geom_point()
plot2

plot3 <- ggplot(data=interesting, aes(x=x3, y=y))+
  geom_point()
plot3

plot4 <- ggplot(data=interesting, aes(x=x4, y=y))+
  geom_point()
plot4

plot5 <- ggplot(data=interesting, aes(x=x5, y=y))+
  geom_point()
plot5

plot6 <- ggplot(data=interesting, aes(x=x6, y=y))+
  geom_point()
plot6


all.reg <- lm( y ~ x1+x2+x3+x4+x5+x6, data=interesting)
summary(all.reg)

residuals <- all.reg$residuals
fitted    <- all.reg$fitted.values

blah <- ggplot(data=all.reg, aes(x=all.reg$fitted.values, y=all.reg$residuals))+
  ggtitle("Residual vs Fitted values - the case of why \n residual plot should always be checked")+
  geom_point()+
  xlab("Fitted Values")+ylab("Residual values")+
  scale_x_continuous(limits=c(-2,2))


ggsave(plot=blah, file="wellthen.png",
       h=4, w=6, units="in", dpi=300)




