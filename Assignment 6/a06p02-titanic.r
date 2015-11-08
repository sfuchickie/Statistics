#
#    STAT340 Instructor:  Dr. Schwarz
#    Assignment 6, Part 2
#    Read in cereal data set plot and 
#    perform basic analysis
#
#    2/22/2015
#
#
############################################

rm(list=ls())


# REad in a tab delimited text file

titanic <- read.table(url("http://www.statsci.org/data/general/titanic.txt"),
                      sep='\t', header=TRUE, fill=TRUE, as.is=TRUE, strip.white=TRUE)

#Checking the dataset

head(titanic)
str(titanic)
tail(titanic)

with(titanic, table(PClass, Sex, Survived, useNA='always'))
xtabs(~Survived+Sex+PClass, data=titanic)

#Probability of survival

overall.surviv <- with(titanic, table(Survived))
overall.surviv

#Probability  of death

overall.prob.death <- overall.surviv[1]/sum(overall.surviv)
cat("Overall prob of death ", overall.prob.death, '\n')

#Odds of death

overall.odds.death<- overall.surviv[1]/overall.surviv[2]
cat('Overall Odds of death: ',overall.odds.death,'\n')

#Log odds of death

logodds.death<-log(overall.odds.death)
cat('Overall logodds of death:', logodds.death,'\n')

#probability of survival

overall.prob.surv<-overall.surviv[2]/sum(overall.surviv)
cat('Overall Odds of survival: ',overall.prob.surv,'\n')

#Odds of survival

overall.odds.surv<- overall.surviv[2]/overall.surviv[1]
cat('Overall Odds of death: ',overall.odds.death,'\n')

#Log odds of survival

logodds.surv<-log(overall.odds.death)
cat('Overall logodds of death:', logodds.death,'\n')


overall.glm <- glm(Survived ~1,
                   data=titanic, family=binomial(link=logit))


anova(overall.glm)
summary(overall.glm) #coefficients

# Extract coefficients of interest
overall.glm.coef <- coef(overall.glm)              #ODDS
overall.glm.coef

overall.glm.ci <- confint(overall.glm)             #CI
overall.glm.ci

overall.glm.se <- sqrt(diag(vcov(overall.glm)))    #SE
overall.glm.se

#Summary

overall.logodds.glm<-c(overall.glm.coef,overall.glm.ci)
cat("Estimate of overall log-odds of survival",
    "and a 95% CI from GLM",overall.logodds.glm,"\n")

#Anti-log scale

overall.antilog.coef<-exp(overall.glm.coef)
overall.antilog.ci<-exp(overall.glm.ci)

overall.antilog.glm<-c(overall.antilog.coef,overall.antilog.ci)
cat("Estimate of overall log-odds of survival",
    "and a 95% CI from GLM",overall.antilog.glm,"\n")

  
#Looking at sex and passenger class:
  
titanic$PClassF<-as.factor(titanic$PClass) #Converting to categorical
titanic$SexF<-as.factor(titanic$Sex)
titanic$SurvivedF<-as.factor(titanic$Survived)
str(titanic)

#Two factor GLM model where (:) is an interaction term

sex.pass.glm <- glm(SurvivedF ~ SexF +PClassF + SexF:PClassF,
                    data=titanic, family=binomial(link=logit))

# Analysis of deviance table - CAUTION these are Type I (incremental)
# rather than Type III (marginal) tests, but only the interaction
# result is of interest
#The Type 1 and 3 tests are the same if the interaction term
# appears last in the model
anova(sex.pass.glm, test='Chi')

library(lsmeans)
sex.pass.glm.lsmo <- lsmeans::lsmeans(sex.pass.glm, ~SexF:PClassF)
sex.pass.glm.est.logodds <- summary(sex.pass.glm.lsmo, type="link")
sex.pass.glm.est.logodds
sex.pass.glm.est.p <- summary(sex.pass.glm.lsmo, type="response")
sex.pass.glm.est.p

# Make the same plot using ggplot
library(ggplot2)
logodds.plot <-
  ggplot(data=sex.pass.glm.est.logodds,
         aes(x=PClassF, y=lsmean, group=SexF, shape=SexF, color=SexF))+
  ggtitle("Comparison of logodds of survival by sex and passenger")+
  xlab("Passenger Class")+ylab("log(Odds) survival and 95% confidence interval")+
  geom_point()+
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2)+
  geom_line()



logodds.plot

p.plot <-
  ggplot(data=sex.pass.glm.est.p,
         aes(x=PClassF, y=prob, group=SexF, shape=SexF, color=SexF))+
  ggtitle("Comparison of probability of survival by sex and passenger")+
  xlab("Passenger Class")+ylab("Probability of survival and 95% confidence interval")+
  geom_point()+
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2)+
  geom_line()
p.plot


