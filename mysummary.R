mysummary <- function(mydf, var, alpha=0.05){
  # Compute the number of element, number of non-missing elements
  # mean, sd, and se of mydf$var
  # mydf is assumed to be a Dataframe and values are collected using a CRD/SRS
  
  #mydf<-mydf[complete.cases(mydf),]
  values <- mydf[,var]
  ntotal <- length(values)
  nonmiss <- length(na.omit(values))
  mean <- mean(values,na.rm=TRUE)
  sd <- sd(values,na.rm=TRUE)
  se <- sd/sqrt(nonmiss)
  ha<-1-alpha/2
  error<-qt(ha,nonmiss-1)*se
  lcl <- mean-error
  ucl<- mean+error
  return(data.frame(ntotal,nonmiss,mean,sd,se,lcl,ucl))
}