#
#
#    Assignment 9, Part 1
#    Read in cereal data set plot and 
#    perform basic analysis
#
#    Saida Amirova 301086263
#    March 20, 2015
#    – do simple bootstrapping to estimate the se and confidence 
#      interval of a statistic
#    – interpret the bootstrap sampling distribution
#
#
###############################################################
###############################################################

#rm(list=ls())
my.boot.fnx<-function(var, ind,alpha=0.95){
  newdf<-na.omit(var[ind])
  mymean<-mean(newdf,na.rm=TRUE)
  mytrimmean<-mean(newdf, trim=0.1, na.rm=TRUE)
  mymedian<-median(newdf,na.rm=TRUE)
  mysd<-sd(newdf,na.rm=TRUE)
  nonmiss  <- length(na.omit(newdf))
  semean   <- mysd/sqrt(nonmiss)
  lcl      <- mymean - qt(1-alpha/2,nonmiss-1)*semean
  ucl      <- mymean + qt(1-alpha/2,nonmiss-1)*semean
  mygini<-sum(abs(outer(newdf,newdf,FUN="-")))/length(newdf)/(length(newdf)-1)*sqrt(pi)/2
  c(mean=mymean,median=mymedian,se=semean,lcl=lcl,ucl=ucl,sd=mysd,gsd=mygini)

}

library(reldist)
library(boot)
library(car)
library(ggplot2)
library(GGally)
library(plyr)
library(lsmeans)
library(gridExtra)

cereal<-read.csv('cereal.csv',as.is=TRUE,
                 strip.white=TRUE,header=TRUE,fill=TRUE)

#remove missing values from all of the dataset
cereal[cereal==-1]<-NA

my.boot.fnx(cereal$calories)
my.boot.fnx(cereal$calories, 1:nrow(cereal))
strap.calories<-boot(cereal$calories,statistic=my.boot.fnx, R=5)
str(strap.calories)
head(strap.calories)

strap.calories$t0
strap.calories$t[1:5,]

sd(strap.calories$t[,1]) ## specifies all rows, column 1
sd(strap.calories$t[,2])

#applying SD function to all columns of the strap object
aa.sd<-aaply(strap.calories$t, 2, sd)
aa.sd

#95% CI bounds for mean and median
quantile(strap.calories$t[,1], prob=c(0.025,0.975))
quantile(strap.calories$t[,2], prob=c(0.025,0.975))
aa.qt<-aaply(strap.calories$t, 2, quantile, prob=c(0.025,0.975))
aa.qt

plot(strap.calories,index=1)
title(main='Calories Sample Mean',line=3)

methods(class=class(strap.calories))
help(plot.boot)

#Sample mean percentile CI from bootstrapping

boot.ci(strap.calories,index=1,type="perc")


###############################################################
###############################################################
##SUGAR PER SERVING

 #my.boot.fnx<-function(df,ind){
     #newdf<-na.omit(df[ind])
     #n<-length(newdf)
     #mymean<-mean(newdf,na.rm=TRUE)
     #mytrimmean<-mean(newdf, trim=0.1, na.rm=TRUE)
     #mymedian<-median(newdf,na.rm=TRUE)
     #mysd<-sd(newdf,na.rm=TRUE)
     #blah<-sum(abs(outer(newdf,newdf,FUN="-")))/2
     #g<-(1/choose(n,2))*blah
     #sigmaest<-g*(sqrt(pi))/2
     #mygini<-sum(abs(outer(newdf,newdf,FUN="-")))/length(newdf)/(length(newdf)-1)*sqrt(pi)/2
     #lcl      <- mymean - qt(1-alpha/2,nonmiss-1)*se
     #ucl      <- mymean + qt(1-alpha/2,nonmiss-1)*se
     #aa.qt<-aaply(strap.calories$t, 2, quantile, prob=c(0.025,0.975))
     #aa.qt
       #c(mean=mymean,median=mymedian,sd=mysd,gsd=sigmaest)
   
     #}

#Performing the analysis on the SUGARS
my.boot.fnx(cereal$sugars)
strap.sugars<-boot(cereal$sugars,statistic=my.boot.fnx, R=1000)
str(strap.sugars)

#Getting an idea what the boot object looks like
strap.sugars$t0
strap.sugars$t[1:5,]


#Sample mean percentile CI from bootstrapping

sugar.sd<-aaply(strap.sugars$t, 2, sd)
sugar.sd
boot.ci(strap.sugars,index=1,type="perc")
boot.ci(strap.sugars,index=2,type="perc")
boot.ci(strap.sugars,index=6,type="perc")
boot.ci(strap.sugars,index=7,type="perc")


plot(strap.sugars,index=1)
title(main='Sugars Sample Mean',line=3)
