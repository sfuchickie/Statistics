#
#    Assignment 7, Part 2
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    February 23rd, 2015
#    – A (very) brief description of the dataset.
#    – Why a simple regression of sugars vs. shelf is not an appropriate
#    method.
#    – The estimated mean amount of sugar/serving for each shelf along
#    with a 95% confidence interval for the population mean.
#    – The plot of the mean amount of sugar/serving by shelf with the 95%
#    confidence intervals. Normally you would not give both the table and
#    the plot, but I want you to get practice in generating both types of
#    output. You can adjust the size of the plot in your word-processor.
#
############################################


#rm(list=ls())

library(ggplot2)
library(GGally)
accidents<-read.csv('accidents.csv',as.is=TRUE,
                    strip.white=TRUE,header=TRUE,fill=TRUE)

head(accidents)
accidents$myDate<-as.Date(accidents$Date,"%d/%m/%Y")
str(accidents)
#convert date to numeric
class(accidents$myDate)

library(plyr)

#First step is to get total accidents by Date
dfTotal <- ddply(accidents, "myDate", summarise, num=length(Date))
head(dfTotal)
str(dfTotal)

#Create a function

mysummaryA <- function(mydf, var, alpha=0.05){
  # Compute the number of element, number of non-missing elements
  # mean, sd, and se of mydf$var
  # mydf is assumed to be a Dataframe and values are collected using a CRD/SRS
  
  values <-mydf[,var]
  ntotal <- length(values)
  nonmiss <- length(na.omit(values))
  mean <- mean(values)
  sd <- sd(values)
  se <- sd/sqrt(nonmiss)
  ha<-1-alpha/2
  error<-qt(ha,nonmiss-1)*se
  lcl <- mean-error
  ucl<- mean+error
  return(data.frame(ntotal,nonmiss,mean,sd,se,lcl,ucl))
}

#Introduce month

dfTotal$month <- as.numeric(format(dfTotal$myDate, "%m"))
monthTotal<-ddply(dfTotal,"month",mysummaryA,var="num")
head(monthTotal)

##Plot the summary

plot.date.accident <- ggplot(data=monthTotal, aes(x=month, y=mean))+
  ggtitle("Number of accidents by month")+
  scale_x_discrete("Month",lim=1:12)+ylab("Mean # of Accidents")+
  geom_point()+
  geom_smooth()+
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
plot.date.accident
##Summarize the data
dfTotal$monthF<-as.factor(dfTotal$month)
str(dfTotal)

#Testing hypothesis

my.fit.monthF <- lm( num~ monthF, data=dfTotal)
anova(my.fit.monthF)
summary(my.fit.monthF)


##FOR THIS WE USE LSMEANS

library(lsmeans)
my.fit.monthF.lsmeans <- lsmeans::lsmeans(my.fit.monthF, ~monthF, adjust="tukey")
str(my.fit.monthF.lsmeans)
monthF.lsmeans <-summary(my.fit.monthF.lsmeans)
str(my.fit.monthF.lsmeans)
cld(my.fit.monthF.lsmeans)
pairs(my.fit.monthF.lsmeans)
confint(pairs(my.fit.monthF.lsmeans))

##Now for Timestamp

accidents$Time[accidents$Time==''] <- "12:00"
accidents$DateTime <- as.POSIXct(paste(accidents$Date," ",accidents$Time),
                            format="%d/%m/%Y %H:%M", tz="UTC")
accidents$DateTime[1:10]
str(accidents$DateTime)
as.numeric(accidents$DateTime[1:10])

accidents$month <- as.numeric(format(accidents$DateTime, "%m"))
accidents[1:10,c("DateTime","month")]

#Extracting Hour and Minute

accidents$hour <- as.numeric(format(accidents$DateTime, "%H"))
accidents$min <- as.numeric(format(accidents$DateTime, "%M"))
accidents[1:10,c("hour","min")]

##Making histogram
#First prepare the data
hourAccident <- ddply(accidents, c("hour","min"), summarise, num=length(hour))
head(hourAccident)

hourAccident$hour<-as.factor(hourAccident$hour)

plot.hour.histo <- ggplot(accidents, aes(x=hour))+
  ggtitle("Number of accidents by hour")+
  xlab("Hour")+ylab("# of Accidents")+
  scale_x_discrete("Hour",lim=0:24)+
  geom_histogram(alpha=.2,binwidth=1)
plot.hour.histo


# scale_x_discrete("Min",lim=0:60)

plot.min.histo<-ggplot(data=accidents, aes(x=min))+
  ggtitle("Number of accidents by minute in hour")+
  ylab("# of Accidents")+
  #scale_x_discrete("Min",lim=0:60)+
  geom_histogram(alpha=.2,binwidth=1,color="Blue",fill="Blue")
  
plot.min.histo

ggsave(plot.min.histo, file='assign07-part02-histo.png',
       h=4, w=6, units="in", dpi=200)


