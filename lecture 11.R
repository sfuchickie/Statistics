accidents$fatal<-recode(accidents$Accident_Severity,
                        "1='yes';else='no'")
#Estimate proportion
#1.Basic formula (only for CRD)
      #N trials and X "Successes" where phat=X/n
                                  #se(phat)=sqrt(phat(1-phat)/N)
                                  #phat+-zed(a/2)se(phat) NOT VALID FOR SMALL SAMPLE T

#EXACT method if small size

mysumstatproportion<-function(df,alpha=.05){
  #method 1 direct computation
  n1   <- nrow(df)
  x1   <- sum(df$fatal=='yes') #count how many fatalities FOR EXAM deal with missing values
  p1   <- x1/n1
  se1  <- sqrt(p1*(1-p1)/n1)
  lcl1 <- p1-qnorm(alpha/2)*se1
  ucl1 <- p1+qnorm(alpha/2)*se1
  
  #Method 2-Proportion test 
  
  res  <- prop.test(x1,n1)
  p2   <- res$estimate
  lcl2 <- res$conf.int[1]
  ucl2 <- res$conf.int[2]
  
  #Method 3 - GLM
  
  fatal.fit<-glm(fatalF~1,family=binomial(link=logit),data=df)
  logodds<-coef(fatal.fit)
  odds<-exp(logodds)
  p3<-odds/(1+odds)
  cilogodds<-confint(fatal.fit)
  ciodds<-exp(cilogodds)
  ci<-ciodds/(1+ciodds)
  lcl3<-ci[1]
  ucl3<-ci[2]
  
  
  res  <- data.frame(method=c('formula','prop','glm'),
                  sampsize=c(n1,n1,n1),
                  phat=c(p1,p2,p3),
                  se  =c(se1,NA,NA),
                  lcl =c(lcl1,lcl2,lcl3),
                  ucl =c(ucl1,ucl2,ucl3))
  return(res)
}
library(plyr)
n<-nrow(accidents)
x<-sum(accidents$fatal=='yes') 
res<-prop.test(x,n)
names(res)
res$conf.int

ddply(accidents,"Speed_limit",mysumstatproportion)
fatal.fit<-glm(fatalF~1,family=binomial(link=logit),data=accidents)
logodds<-coef(fatal.fit)
odds<-exp(logodds)
p<-odds/(1+odds)
p   #this is a proportion
accidents$Speed_limitF<-factor(accidents$Speed_limit)
all.fit<-glm(fatalF~Speed_limitF,family=binomial(link="logit"),data=accidents)
library(lsmeans) #lsmeans is flexible enough ot handle glm but its not means its logodds
allfit.lsmo<-lsmeans::lsmeans(all.fit,~Speed_limitF)
summary(allfit.lsmo)
##   CHI-SQUARED approximation is NOT appropriate
##   Fishers exact test?
##  YOU GET WARNING: assymptotic or exact
##  under what conditions this works what are the strengths and weaknesses
##  glm
#   NOTICE 000000000 BECAUSE ITS A SMALL SIZE AND FORMULA WONT WORK!!!!! TAKE STAT475
#   proportion test
##  THERE ARE 6 TYPES of CIs
##  DELTA method for SE of any function
##  MAximum likelihood estimation for glm
##  DEviance table for glm

##  There is not variance in binomial data