#
#
#    Assignment 8, Part 1
#    Read in cereal data set plot and 
#    perform basic analysis
#
#    Saida Amirova 301086263
#    March 11, 2015
#    – A (very) brief description of the dataset.
#    – A statement about the comparison of interest. An estimate of the difference
#      in mean, its standard error, and the p-value from the relevant
#      hypothesis. What do you conclude?
#   – The two plots (created using ggplot) comparing the means and an
#      explanation of why the plots are not identical.
#
############################################



mysumstat<-function(df, var, alpha=0.05){
  
  temp<-lm(df[,var]~1, data=df)
  mymean3<-coef(temp)
  myse3<-sqrt(diag(vcov(temp)))
  sampsize3<-summary(temp)$df[2]+1
  lcl3<-confint(temp)[1]
  ucl3<-confint(temp)[2]
  
  res<-data.frame(var=var, method=c("lm"),
                  n=c(sampsize3), mean=c(mymean3), 
                  se=c(myse3), 
                  lcl=c(lcl3), ucl=c(ucl3), stringsAsFactors=FALSE)
  
  return (res)
}

###############################################################
###############################################################

library(car)
library(ggplot2)
library(GGally)
library(plyr)
library(lsmeans)
library(gridExtra)
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

# Create a fiber factor 
cereal$fiberC <- recode(cereal$fiber,
                         "0:2 ='low'; 2.5:15='high'")
xtabs(~fiber+fiberC, data=cereal)

cereal$fiberCF <- factor(cereal$fiberC)
head(cereal)
str(cereal)

# Reorder the shelf factor
cereal$fiberCFO <- factor(cereal$fiberCF, levels=c("low","high"), 
                           ordered=TRUE)
head(cereal)
str(cereal)


#Getting a scatter plot of FiberCFO and Calories with boxplot overlay
plot.dot.box <- ggplot(data=cereal, aes(x=fiberCFO, y=calories))+
  ggtitle("Sugars by fiber class")+
  xlab("Fiber")+ylab("Mean Calories per serving (g)")+
  geom_boxplot(notch=TRUE,notchwidth = 0.5)+
  geom_point()+
  geom_jitter()
plot.dot.box

#Fitting lm()

fitted.lm<-ddply(cereal,"fiberCFO",mysumstat,"calories")
summary(fitted.lm)
head(fitted.lm)

plot.bar.box.LM <- ggplot(data=fitted.lm, aes(x=fiberCFO, y=mean))+
                   ggtitle("Calories by fiber class- LM")+
                   xlab("Fiber")+ylab("Mean Calories per serving (g)")+
                   geom_bar(stat="identity",alpha=.2)+
                   geom_errorbar(aes(ymin=lcl, ymax=ucl),width=0.2)
plot.bar.box.LM

mysumstat.T            <- t.test(calories~fiberCFO,data=cereal)
mysumstat.T$estdiff    <- sum( c(1,-1)*mysumstat.T$estimate)
mysumstat.T$estdiff.se <- mysumstat.T$estdiff/mysumstat.T$statistic
str(mysumstat.T)
str(mysumstat.T)
print(mysumstat.T)
print(mysumstat.T$estdiff)
print(mysumstat.T$estdiff.se)

#Getting LM fit

fit.LM.fiberCFO<-lm(calories~fiberCFO, data=cereal)
head(fit.LM.fiberCFO)

summary(fit.LM.fiberCFO)

#Comparing means through LSMEANS function
fiberCFO.lsm <- lsmeans::lsmeans(fit.LM.fiberCFO, ~fiberCFO)

summary.lsm <- summary(fiberCFO.lsm, type="link")
summary.lsm
cld    (fiberCFO.lsm)
pairs  (fiberCFO.lsm)
confint(pairs  (fiberCFO.lsm))

plot.bar.box.LS <- ggplot(data=summary.lsm, aes(x=fiberCFO, y=lsmean))+
                   ggtitle("Calories by fiber class - LSMEANS")+
                   xlab("Fiber")+ylab("Mean Calories per serving (g)")+
                   geom_bar(stat="identity",alpha=.2)+
                   geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),width=0.2)
plot.bar.box.LS

#Looking at residuals
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
sf.autoplot.lm(fit.LM.fiberCFO)

fit.all <- arrangeGrob(plot.bar.box.LS, plot.bar.box.LM, ncol=2)
fit.all

ggsave(plot=fit.all, file="assign08-part01-fitall.png",
       h=4, w=6, units="in", dpi=300)
