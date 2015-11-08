#
#
#    Assignment 10, Part 2
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    March 20th, 2015
#   – analyze the results from an experiment with a categorical response
#   – estimate population marginal parameters
#     plot these items on a suitable plots.
#
############################################

#rm(list=ls())
setwd("/Users/Saida/Desktop/STAT/Assignment 10")

library(ggplot2)
library(car)
library(lsmeans)

cell<-read.table('cellphone.txt',as.is=TRUE,
               strip.white=TRUE,header=TRUE,fill=TRUE)
str(cell)

cell$callsC <- recode(cell$calls,"1:4 ='yes'; 0='no'")

xtabs(~calls+callsC, data=cell)
str(cell$callsF) 


cell$callsF <-factor(cell$callsC)
xtabs(~ad+callsF,data=cell)

cell$adC <- recode(cell$ad, "'none' = 'none'; 'dh'= 'dh'; 'dv' = 'dv';
                              'jc' = 'jc'; 'ss' = 'ss'")
cell$adF <- factor(cell$adC,levels=c("none","dh","dv","jc","ss"),ordered=TRUE)
str(cell$adF)

glm.calls               <- glm(callsF ~ adF, data=cell,family=binomial(link=logit))
anova(glm.calls, test="LRT")
sum.calls               <- summary(glm.calls)
sum.calls

logodds.calls           <- coef(glm.calls)
logodds.se.calls        <- sqrt(diag(vcov(glm.calls)))

logodds.calls.lsmo      <- lsmeans::lsmeans(glm.calls,~adF, adjust="tukey")

summary.logodds         <- summary(logodds.calls.lsmo)
summary.logodds$odds    <- exp(summary.logodds$lsmean)
summary.logodds$oddsSE  <- exp(summary.logodds$SE)




#PROBABILITY OF GETTING INTERRUPTED 

prob.diff <-summary(logodds.calls.lsmo, type = "response")
prob.diff

logodds.diff<-summary(pairs(logodds.calls.lsmo))
logodds.diff

#GETTING SE AND CONFIDENCE INTERVALS for LOGODDS
str(logodds.diff)
logodds.diff <- logodds.diff [1:4,]


logodds.diff$oddsRatio    <- 1/exp(logodds.diff$estimate)
#logodds.diff$oddRatioSE   <- exp(logodds.diff$SE)
logodds.diff$lclLOG <- logodds.diff$estimate - 1.96 * logodds.diff$SE
logodds.diff$uclLOG <- logodds.diff$estimate + 1.96 * logodds.diff$SE
logodds.diff

# NOW FOR ODDS


logodds.diff$ucl <- 1/exp(logodds.diff$lclLOG)
logodds.diff$lcl <- 1/exp(logodds.diff$uclLOG)

logodds.diff


# Plotting the results


plot.now <- ggplot (data=logodds.diff, aes(x = contrast, y = oddsRatio))+
            ggtitle("Odds Ratio of being interrupted by a cellphone")+
            geom_point(stat="identity")+
            xlab("Advertisement")+
            ylab("Odds of interruption that followed")+
            geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.15)
plot.now

ggsave(plot=plot.now,file='FINALLY.png',h=4, w=6, units="in", dpi=300)
