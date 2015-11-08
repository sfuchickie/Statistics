#
#
#    Assignment 9, Part 2
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    March 22th, 2015
#   – A (very) brief description of the dataset.
#   – A plot of the odds of fatality by hour of the data and your conclusions
#   from this. At what hour is the odds of a fatality highest and how
#   certain are you of that result?
#
############################################
#rm(list=ls())
#setwd("/Users/Saida/Desktop/STAT/Assignment 9")
library(ggplot2)
library (GGally)
library (lsmeans)
library (car)
library (plyr)
library(reshape2)
accidents <- read.csv('accidents.csv',as.is=TRUE,
                      strip.white=TRUE,header=TRUE,fill=TRUE)

head(accidents)

names(accidents)
accidents[accidents==-1]<-NA

xtabs(~Number_of_Vehicles+Number_of_Casualties,data=accidents)
accidents$AccidentIndex <- accidents$Number_of_Vehicles*accidents$Number_of_Casualties


#produces a random number without repalcement to create a sample size n
my.sample <- sample(1:nrow(accidents),size=10,replace=FALSE)
my.sample

#which then it is used to create a new variable with the random accident index

myfxn <- function(df, ind){
  #browser()
  newdf  <- df[ind,]
  n      <- length(newdf)
  mymean <- mean(newdf$AccidentIndex,na.rm=TRUE)
  quin   <- quantile(newdf$AccidentIndex, prob=c(0.90))
  c(mean=mymean,Q=quin)
}
AI.stat <- function(df, size){
  ## select a sample of size x and compute a statistic
  rows     <- 1:nrow(df)
  mysample <-sample(rows, size=size, replace=FALSE)
  mystat   <-myfxn(df, mysample)
  c(size=size,mystat)
}

AI.stat(accidents, 100)

my.result <- rdply(5, AI.stat(df=accidents, size=10),.id="rep")  #WHAT DOES ID DO?
my.result

AI.sim <- function (size, df , nsamples=1000){
  # Generate nsamples of size=sample size from radf
  # For each sample, compute the mean and per90 of the accident index
  # After all samples are generated, compute the sd of the mean and per90 values
  rows      <- 1:nrow(df)
  mysample  <- sample(rows, size=size, replace=FALSE)
  my.result <- rdply(nsamples, AI.stat(df=df, size=size),.id="rep")
  se        <- sd(my.result[,3])
  seper     <- sd(my.result[,4])
  c(size=size,se=se,se.per90=seper)
}


AI.sim(size=200, df=accidents, nsamples=100)

sim.table  <- ldply(c(100,200,400,1000,2000,4000), AI.sim, df=accidents, nsamples=1000)

#Adding bits to the table

sim.table$logN     <- log(sim.table$size)
sim.table$logSe    <- log(sim.table$se)
sim.table$logPer90 <- log(sim.table$se.per90)


# Fitting a line 

fit.line <- lm(logN~logSe, data=sim.table)
str(fit.line)


#Extracting bits
fit.line.coef <- coef(fit.line)
fit.line.ci   <- confint(fit.line)

my.result2    <- melt(sim.table, id="size")
my.result2

my.plot2 <- ggplot(data=my.result2, aes(x=size, y=value,
                                      group=variable, color=variable, shape=variable, linetype=variable))+
                 geom_point()+
                 geom_line ()+
                 geom_smooth(method="lm",se=FALSE)
my.plot2
my.plot  <- ggplot(data=my.result2, aes(x=log(size), y=log(value),
                                    group=variable, color=variable, shape=variable, linetype=variable))+
                geom_point()+
                geom_line ()+
                geom_smooth(method="lm",se=FALSE)

ggsave(my.plot,file='logSE.png',h=4,w=6,units="in",dpi=200)
                          
                           