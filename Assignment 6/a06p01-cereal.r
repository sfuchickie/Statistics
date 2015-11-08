#
#
#    Assignment 6, Part 1
#    Read in cereal data set plot and 
#    perform basic analysis
############################################

library(ggplot2)
library(GGally)
cereal<-read.csv('cereal.csv',as.is=TRUE,
                 strip.white=TRUE,header=TRUE,fill=TRUE)

#look at the structure
str(cereal)
names(cereal)
dim(cereal)
cereal[1:10,]
head(cereal)
tail(cereal)

#remove missing values from all of the dataset
cereal[cereal==-1]<-NA

#scatterplot matrix
scatterplotmat<-ggpairs(cereal[,c("protein","fat","carbo",
                                "sugars","sodium","vitamins")],
                     title="Casement plot of variables from cereals dataset")

#Perform basic scatter without jitter which introduces overplotting

plotcalfat <- ggplot(data=cereal, aes(x=fat, y=calories))+
  ggtitle("Calories vs fat content")+
  geom_point()

plotcalfat

#Perform a basic scatter with jitter to avoid overplotting

plotcalfat2 <- ggplot(data=cereal, aes(x=fat, y=calories))+
  ggtitle("Calories vs fat content - point jittered")+
  geom_jitter()

plotcalfat2

#Fitting a linear model for fat vs. calories

my.fit <- lm( calories ~ fat, data=cereal)
#Linear models are lists
str(my.fit)
is.list(my.fit)       #returns whether the object is a list or not

#looking at coefficients

my.fit$coefficients    #this is one way to look at coefficients
my.fit$coefficients[1] #it is better to use coef()

#subtle differences between [k] and [[k]]
x<-my.fit[1]
x;str(x)
x[1]

y<-my.fit[[1]]
y; str(y)
y[1]

anova(my.fit)
summary(my.fit)        #coefficients

# Extract coefficients of interest
my.fit.coef <- coef(my.fit)
my.fit.coef

#Varience covarience matrix
vcov(my.fit)
my.fit.se <- sqrt(diag(vcov(my.fit))) #
my.fit.se
my.fit.ci <- confint(my.fit)
my.fit.ci

#Creating a table with coefficients of interest to 2 decimal points

my.table <- data.frame(Coef=round(my.fit.coef,2),
                       SE= round(my.fit.se,2),
                       CI=round(my.fit.ci,2))
my.table

#Saving the table to file
sink('assign06-part01-cereal-table1.txt', split=TRUE)
my.table
sink()
#Plot of scatterplot plus regression line with geom_abline()

plotcalfat3 <- plotcalfat2 +
  geom_abline( intercept=my.fit.coef[1], slope=my.fit.coef[2])

plotcalfat3

ggsave(plotcalfat3, file='assign06-part01-calfat.png',
       h=4, w=6, units="in", dpi=200)



# Create the model diagnostic plot using Dr. Schwartz program
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
diagplot <- sf.autoplot.lm(my.fit)
diagplot

newfat <- data.frame(fat=c(4))
newfat
my.fit.at4.mean <- predict(my.fit, newdata=newfat, se.fit=TRUE,
                           interval="confidence")
my.fit.at4.mean
my.fit.at4.indiv <- predict(my.fit, newdata=newfat, se.fit=TRUE,
                            interval="prediction")
my.fit.at4.indiv
