#
#
#    Assignment 7, Part 1
#    Read in cereal data set plot and 
#    perform basic analysis
#
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


#Function

mysummary <- function(mydf, var, alpha=0.05){
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

shelfC<-c("low","medium","high")[cereal$shelf]

library(car)
cereal$shelfC <- recode(cereal$shelf,
                        "1='low'; 2='medium'; 3='high'")
xtabs(~shelf+shelfC, data=cereal)

shelfCF<-factor(shelfC)
class(shelfCF)

str(cereal)

shelfCFO<-factor(shelfC,levels=c("low","medium","high"),
                 ordered=TRUE)
str(cereal)

library(ggplot2)
library(GGally)

plot.dot.box <- ggplot(data=cereal, aes(x=shelf, y=sugars))+
  ggtitle("Sugars by shelf - using character")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE,notchwidth = 0.5)+
  geom_point()+
  geom_jitter()
plot.dot.box

plot.dot.boxC <- ggplot(data=cereal, aes(x=shelfC, y=sugars))+
  ggtitle("Sugars by shelf - using character")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE,notchwidth = 0.5)+
  geom_point()+
  geom_jitter()
plot.dot.boxC

plot.dot.boxCF <- ggplot(data=cereal, aes(x=shelfCF, y=sugars))+
  ggtitle("Sugars by shelf - using a factor")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE,notchwidth = 0.5)+
  geom_point()+
  geom_jitter()
plot.dot.boxCF

plot.dot.boxCFO <- ggplot(data=cereal, aes(x=shelfCFO, y=sugars))+
  ggtitle("Sugars by shelf - using an ordered factor")+
  xlab("Shelf")+ylab("Sugar per serving (g)")+
  geom_boxplot(notch=TRUE,notchwidth = 0.5)+
  geom_point()+
  geom_jitter()
plot.dot.boxCFO

ggsave(plot=plot.dot.boxCFO, file='cereal-dot-plot.png',
       h=4, w=6, units="in", dpi=300)

library(gridExtra)

plot.dot.box.all <- arrangeGrob(plot.dot.boxCFO,
                                plot.dot.boxCF,
                                plot.dot.boxC,
                                plot.dot.box, ncol=2)
plot.dot.box.all

with(cereal, mean(sugars, na.rm=TRUE))

with(cereal, length(na.omit(sugars)))

with(cereal, sum( !is.na(sugars)))
with(cereal, sum( is.na(sugars)))

library(plyr)

##FOR SHELF
shelf.sum <- ddply(cereal, "shelf", summarize,
                   mean.calories = mean(calories),
                   std.calories = sd(calories),
                   mean.sugars = mean(sugars, na.rm=TRUE))
mysummary(cereal, "sugars")


summary.shelf <- ddply(cereal, "shelf", mysummary, var="sugars")
summary.shelf
str(summary.shelf)

##FOR SHELFC
shelf.sum.shelfC <- ddply(cereal, "shelfC", summarize,
                          mean.calories = mean(calories),
                          std.calories = sd(calories),
                          mean.sugars = mean(sugars, na.rm=TRUE))


summary.shelfC <- ddply(cereal, "shelfC", mysummary, var="sugars")
summary.shelfC

##SHELFCF
shelf.sum.shelfCF <- ddply(cereal, "shelfCF", summarize,
                           mean.calories = mean(calories),
                           std.calories = sd(calories),
                           mean.sugars = mean(sugars, na.rm=TRUE))


summary.shelfCF <- ddply(cereal, "shelfCF", mysummary, var="sugars")
summary.shelfCF

##ShelfCFO

shelf.sum.shelfCFO <- ddply(cereal, "shelfCFO", summarize,
                            mean.calories = mean(calories),
                            std.calories = sd(calories),
                            mean.sugars = mean(sugars, na.rm=TRUE))


summary.shelfCFO <- ddply(cereal, "shelfCFO", mysummary, var="sugars")
summary.shelfCFO

##PLOT OF MEAN  BY SHELF

plot.mean.shelf <- ggplot(data=summary.shelf, aes(x=shelf, y=mean))+
  ggtitle("Calories by shelf")+
  xlab("Shelf")+ylab("Mean Calories")+
  geom_point()+geom_line(aes(group=1))+geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
plot.mean.shelf

##PLOT OF MEAN BY SHELFC

plot.mean.shelfC <- ggplot(data=summary.shelfC, aes(x=shelfC, y=mean))+
  ggtitle("Calories by shelf")+
  xlab("Shelf")+ylab("Mean Calories")+
  geom_point()+geom_line(aes(group=1))+geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
plot.mean.shelfC

##PLOT OF MEAN BY SHELFCF

plot.mean.shelfCF <- ggplot(data=summary.shelfCF, aes(x=shelfCF, y=mean))+
  ggtitle("Calories by shelf")+
  xlab("Shelf")+ylab("Mean Calories")+
  geom_point()+geom_line(aes(group=1))+geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
plot.mean.shelfCF

##PLOT OF MEAN BY SHELFCFO

plot.mean.shelfCFO <- ggplot(data=summary.shelfCFO, aes(x=shelfCFO, y=mean))+
  ggtitle("Calories by shelf")+
  xlab("Shelf")+ylab("Sugar/serving")+
  geom_point()+geom_line(aes(group=1))+geom_point(size=3)+
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)+
  scale_colour_manual(name="Error Bars",values=c("lcl","ucl"))
plot.mean.shelfCFO

##FORMAL HYPOTHESIS TESTING

my.fit.shelf <- lm(sugars ~ shelf, data=cereal)
my.fit.shelfC <- lm(sugars ~ shelfC, data=cereal)
my.fit.shelfCF <- lm(sugars ~ shelfCF, data=cereal)
my.fit.shelfCFO<- lm(sugars ~ shelfCFO, data=cereal)

anova(my.fit.shelf)
anova(my.fit.shelfC)
anova(my.fit.shelfCF)
anova(my.fit.shelfCFO)

summary(my.fit.shelf)
summary(my.fit.shelfC)
summary(my.fit.shelfCF)
summary(my.fit.shelfCFO)   ##HARD TO INTERPRET DIFFERENT BASE COMPARISON

##FOR THIS WE USE LSMEANS

library(lsmeans)
my.fit.shelfCFO.lsmeans <- lsmeans::lsmeans(my.fit.shelfCFO, ~shelfCFO, adjust="tukey")
shelfCFO.lsmeans <-summary(my.fit.shelfCFO.lsmeans)
cld(my.fit.shelfCFO.lsmeans)
pairs (my.fit.shelfCFO.lsmeans)
confint(pairs (my.fit.shelfCFO.lsmeans))

summary(my.fit.shelfCFO.lsmeans)
str(shelfCFO.lsmeans)

##PLOTTING LSMEANS FOR SHELFCFO

plot.shelfCFO.means2 <- ggplot(data=shelfCFO.lsmeans, aes(x=shelfCFO, y=lsmean))+
  ggtitle("Calories by shelf")+
  xlab("Shelf")+ylab("Mean Calories")+
  geom_point()+geom_line(aes(group=1))+
  geom_point(size=3)+
  scale_x_discrete("Shelf",labels=c("Low","Medium","High"))+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15)
  
plot.shelfCFO.means2
summary(my.fit.shelfCFO)
ggsave(plot.shelfCFO.means2, file='assign07-part01-shelf.png',
       h=4, w=6, units="in", dpi=200)

##Just comparison of LSMEANS vs LM result
plot.means.all <- arrangeGrob(plot.shelfCFO.means2,
                              plot.mean.shelfCFO,ncol=2)
plot.means.all


source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
sf.autoplot.lm(my.fit.shelfCFO)