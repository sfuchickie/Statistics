#
#
#    Assignment 8, Part 2
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    March 12th, 2015
#   – A (very) brief description of the dataset.
#   – A plot of the odds of fatality by hour of the data and your conclusions
#   from this. At what hour is the odds of a fatality highest and how
#   certain are you of that result?
#
############################################

required.packages <- c("plyr","ggplot2","gridExtra","lsmeans","car","GGally")
##Checks whether required packages are installed
checkAndInstall <- function(required.packages){
  
  # Define a function that checks if the package package.name is installed,
  # and if not, installs it
  if(!package.name%in%rownames(installed.packages())){
    install.packages(package.name,
                     dep=TRUE,
                     repos="http://cran.r-project.org")
  }
}

#rm(list=ls())

mysumstatfatality<-function(df, var, alpha=0.05){
  temp<-glm(df[,var]~1, data=df,family=binomial(link=logit))
  mylog<-coef(temp)
  myse3<-sqrt(diag(vcov(temp)))
  sampsize3<-summary(temp)$df[2]+1
  lcl3<-confint(temp)[1]
  ucl3<-confint(temp)[2]
  
  res<-data.frame(var=var, method=c("glm"),
                  n=c(sampsize3), mean=c(mylog), 
                  se=c(myse3), 
                  lcl=c(lcl3), ucl=c(ucl3), stringsAsFactors=FALSE)
  
  return (res)
}

library(ggplot2)
library (GGally)
library (lsmeans)
library (car)
library (plyr)

accidents <- read.csv('accidents.csv',as.is=TRUE,
                    strip.white=TRUE,header=TRUE,fill=TRUE)

head(accidents)

# Manipulating time for time extraction

accidents$DateTime <- as.POSIXct(paste(accidents$Date," ",accidents$Time),
                                 format="%d/%m/%Y %H:%M", tz="UTC")
accidents$DateTime[1:10]
str(accidents$DateTime)


summary(accidents$DateTime)
as.numeric(accidents$DateTime[1:10])

# How many are missing

totalmissing  <- sum(as.numeric(is.na(accidents$DateTime)))
totalmissing

# Recoding fatality to be a categorical variable
accidents$hour  <- as.numeric(format(accidents$DateTime,"%H"))
accidents$hourF <- as.factor(accidents$hour)
accidents$fatalityC <- recode(accidents$Accident_Severity,
                                     " 1='Fatal'; 2:3='Non-Fatal'")


accidents$fatalityCFO <- factor(accidents$fatalityC,
                         levels=c("Non-Fatal","Fatal"),ordered=TRUE)
str(accidents$fatalityCFO)

# Simple table of accidents

table.fatality.hour <- xtabs(~hourF+fatalityCFO, data=accidents)
table.fatality.hour

overall.surviv <- with(accidents, table(fatalityCFO))
overall.surviv

#GLM of log odds intercept only model
logodds.intr <- ddply(accidents,"hourF",mysumstatfatality,"fatalityCFO")
logodds.intr

logodds.intr.plot <- ggplot(data=logodds.intr,aes(x=hourF, y=mean))+
                            ggtitle("Log-odds of non-fatal accident by hour with 95% CI")+
                            geom_point(stat="identity")+
                            xlab("Hour")+ylab("Logodds")+
                            scale_x_discrete(breaks=c(0:25))+
                            scale_y_continuous(limits=c(-5.5,-2.5))+
                            geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
logodds.intr.plot

#GLM of log odds across the hours
class(accidents$hourF)

logodds.hour <- glm(fatalityCFO~hourF,data=accidents,family=binomial(link="logit"))
anova(logodds.hour,test="LRT")
blah<-summary(logodds.hour)
logodds.hour.lsmo <- lsmeans::lsmeans(logodds.hour,~hourF, adjust="tukey")
logodds.diff<-cld(logodds.hour.lsmo)
str(logodds.diff)

## CONVERTING TO ODDS OF FATALITY FROM LOGODDS OF NONFATALITY

logodds.diff$odds<-exp(logodds.diff$lsmean)
#logodds.diff$oddsfatality<-1/(logodds.diff$odds)
logodds.diff$lclodds<-exp(logodds.diff$asymp.LCL)
logodds.diff$uclodds<-exp(logodds.diff$asymp.UCL)
#logodds.diff$lclfatality<-1/(logodds.diff$lclodds)
#logodds.diff$uclfatality<-1/(logodds.diff$uclodds)

logodds.diff
logodds.hour.ls <- summary(logodds.hour.lsmo)

lsmeans.plot <- ggplot(data=logodds.diff, aes(x=hourF,y=odds))+
                       ggtitle("Odds of fatality by hour with 95% CI")+
                       geom_point(stat="identity")+
                       xlab("Hour as factor")+
                       ylab("Odds of fatality")+
                       geom_errorbar(aes(ymin=lclodds,ymax=uclodds),width=0.15)
                     
lsmeans.plot

ggsave(plot=lsmeans.plot, file="assign08-part02-logodds-plot.png",
       h=4, w=6, units="in", dpi=300)
