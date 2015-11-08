

library(lsmeans)
library(plyr)

cereal<- read.csv("cereal.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
cereal[1:5,]
cereal[, "type"]
cereal$type[1:5]

cereal[cereal==-1]<-NA
cereal[cereal$calories>100,]

mysumstat<-function(df, var){
  mymean<-mean(df[,var])
  mysd<- sd(df[,var])
  #browser()
  mycv<- mysd/mymean
  res<-data.frame(var=var, mean=mymean,sd=mysd, cv=mycv, stringsAsFactors=FALSE)
  return (res)
}

## almost always want to specify stringsAsFactors as FALSE. do not want it to convert

mysumstat(cereal,"calories")

splitvar<-c("shelf", "type")

ddply(cereal, splitvar, mysumstat, var="calories")
## remember mysumstat has 2 arguments.. anything after the function name ( the ... argument)
  ## is not part of ddply, its automatically passed to the first function (mysumstat)


## can also put the function directly into the ddply, instead of mysumstat
    ## anonymous function
ddply(cereal, splitvar, function(df, var){
  mymean<-mean(df[,var])
  mysd<- sd(df[,var])
  #browser()
  mycv<- mysd/mymean
  res<-data.frame(var=var, mean=mymean,sd=mysd, cv=mycv, stringsAsFactors=FALSE)
  return (res)},    
  var="calories")

## back to sumstat

mysumstat2<-function(df, var, alpha=0.05){
  mymean<-mean(df[,var], na.rm=TRUE)
  mysd<- sd(df[,var], na.rm=TRUE)
  sampsize<-length(df[,var])
  myse<- mysd/sqrt(sampsize)
  lcl<-mymean - qt(1-alpha/2, sampsize-1)*myse
  ucl<- mymean + qt(1-alpha/2, sampsize-1)*myse
  res<-data.frame(var=var, n=sampsize, mean=mymean, sd=mysd, se=myse, 
                  lcl=lcl, ucl=ucl, stringsAsFactors=FALSE)
  return (res)
}
ddply(cereal, "shelf", mysumstat2, var="calories")
ddply(cereal, "shelf", mysumstat2, var="fat")
ddply(cereal, "shelf", mysumstat2, var="weight")

## notice that weight has removed missing values but n still shows 36 for shelf 3. 
    ## that's why you  never want to code these functions yourself. use R functions


temp<- t.test(cereal$calories)
temp

## doesnt give you SE but it gives you t-statistic -- which is estimate/SE
str(temp)
temp$statistic
temp$estimate
temp$confint
temp$conf.int


mysumstat3<-function(df, var, alpha=0.05){
  mymean1<-mean(df[,var], na.rm=TRUE)
  mysd1<- sd(df[,var], na.rm=TRUE)
  sampsize1<-length(df[,var])
  myse1<- mysd1/sqrt(sampsize1)
  lcl1<-mymean1 - qt(1-alpha/2, sampsize1-1)*myse1
  ucl1<- mymean1 + qt(1-alpha/2, sampsize1-1)*myse1
  
  temp<-t.test(df[,var])
  myt.value<-temp$statistic
  mymean2<-temp$estimate
  sampsize2<-temp$parameter+1
  lcl2<-temp$conf.int[1]
  ucl2<-temp$conf.int[2]
  myse2<-temp$estimate/temp$statistic
  res<-data.frame(var=var, method=c("formula", "t.test"),
                  n=c(sampsize1,sampsize2), mean=c(mymean1,mymean2), sd=mysd1, se=c(myse1, myse2), 
                  lcl=c(lcl1,lcl2), ucl=c(ucl1,ucl2), stringsAsFactors=FALSE)
  return (res)
}

ddply(cereal, "shelf", mysumstat3, var="weight")


temp<-lm(calories~1, data=cereal)
coef(temp)
sqrt(diag(vcov(temp)))
confint(temp)

mysumstat4<-function(df, var, alpha=0.05){
  mymean1<-mean(df[,var], na.rm=TRUE)
  mysd1<- sd(df[,var], na.rm=TRUE)
  sampsize1<-length(df[,var])
  myse1<- mysd1/sqrt(sampsize1)
  lcl1<-mymean1 - qt(1-alpha/2, sampsize1-1)*myse1
  ucl1<- mymean1 + qt(1-alpha/2, sampsize1-1)*myse1
  
  temp<-t.test(df[,var])
  myt.value<-temp$statistic
  mymean2<-temp$estimate
  sampsize2<-temp$parameter+1
  lcl2<-temp$conf.int[1]
  ucl2<-temp$conf.int[2]
  myse2<-temp$estimate/temp$statistic
  
  #browser()
  temp<-lm(df[,var]~1, data=df)
  mymean3<-coef(temp)
  myse3<-sqrt(diag(vcov(temp)))
  sampsize3<-summary(temp)$df[2]+1
  lcl3<-confint(temp)[1]
  ucl3<-confint(temp)[2]
  
  res<-data.frame(var=var, method=c("formula", "t.test","lm"),
                  n=c(sampsize1,sampsize2, sampsize3), mean=c(mymean1,mymean2,mymean3), 
                  sd=mysd1, se=c(myse1, myse2, myse3), 
                  lcl=c(lcl1,lcl2, lcl3), ucl=c(ucl1,ucl2, ucl3), stringsAsFactors=FALSE)
 
  return (res)
}

ddply(cereal, "shelf", mysumstat4, var="weight")
summary(temp)

names(summary(temp)) ## to extract the degrees of freedom


## shelf must be declared as a factor if you want ANOVA! 
## don't use numeric codes forcategorical variables

cereal$shelfF<-factor(cereal$shelf)
fit.weight<-lm(weight~shelfF, data=cereal)
fit.weight.lsmo<-lsmeans::lsmeans(fit.weight,~shelfF)
summary(fit.weight.lsmo)



## R Inferno- "if you are using R and you think you're in hell, this is a map for you."