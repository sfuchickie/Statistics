#
#
#    Assignment 10, Part 1
#    Making babies - MAximum likelihood estimation
#
#    Saida Amirova 301086263
#    March 24, 2015
#    – create function to evaluate the log-likelihood for a problem;
#    – plot the log-likelihood against the single parameter to see where the MLE
#      occurs;
#    – find the MLE numerically, along with an estimate of precision;
#    – find confidence intervals for MLE based on large sample theory;
#      find profile confidence intervals in the case of a single parameter.
#
#
###############################################################
###############################################################

# Let Y be the number of months PRIOR to becoming pregnant. Then
#     P(Y = y|p) = (1 − p)**y × p
#    For censored data, we add together the probability of becoming pregnant over all the months
#             greater than or equal to the censored value:
#             P(Y ≥ y|p) = ∞ SUM i = y(1 − p)i × p which reduces to (1 − p)**y
#rm(list=ls())
#[Yilog(1 − p) + log(p)] +

library(bbmle) 
library(ggplot2)
library(plyr)

priorPreg <- c (2, 7, 5, 0, 0, 3, 0, 4, 10)
censored  <- c (0, 0, 0, 0, 0, 0, 0, 0, 1 )
# p <- 1/mean(priorPreg + 1)
data <- data.frame(priorPreg, censored)


preg.in.month.log.L <- function(p, data, negll=FALSE) { # PUT PARAMETER AS FIRST ARGUMENT
  # Negative Binomial independent sampling [do not do coding yourself]
  
  expOne <- sum( (data[,2]==0)*(data[,1] * log(1-p) + log(p)))
  expTwo <- sum( (data[,2]==1)*(data[,1]* log(1-p)))

  res <- expOne + expTwo
  if(negll){res <- -res}
  return(res)
}



preg.in.month.log.L (.2, data, negll=TRUE)

nrow(data)
str(data)
plotdata<-NULL


## 3 Use a function from the plyr package to compute the log-likelihood between
## 0 and 0.5 in steps of .01 and plot the log-likelihood function

plotdata<-data.frame(p<-seq(0, 0.5, 0.01))
str(plotdata)

plotdata$logL <- laply(plotdata$p, preg.in.month.log.L, data=data)
head(plotdata$logL)

pguess <- 0.15
mymle       <- optim(pguess, preg.in.month.log.L, data=data, negll=TRUE, hessian=TRUE)
mymle

se <- sqrt(1/mymle$hessian)
se
preg.in.month.log.L (0.2051017, data)

plot <- ggplot(data=plotdata, aes(x=p, y=logL))+
        geom_line()

plot.mle <- ggplot(data=plotdata, aes(x=p, y=logL))+
       ggtitle("Maximum likelihood of conceiving through optimization")+
       geom_line()+
       geom_point(aes(x=mymle$par, y=-mymle$value), size=4, shape=2)+
       scale_shape_identity()
plot.mle


## Use the nlm() function to find the MLE

nlm.preg <- nlm(preg.in.month.log.L, pguess, data = data, negll=TRUE, hessian=TRUE)
nlm.preg

se.nlm  <- sqrt(1/nlm.preg$hessian)
mle.nlm <- nlm.preg$estimate
mle.nlm.Y <- preg.in.month.log.L (mle.nlm, data)


#Getting the Ys coresponding to the log of LCL and UCL
ucl.nlm <- mle.nlm + qnorm(0.975) * se.nlm
ucl.nlm.Y <- preg.in.month.log.L (ucl.nlm, data)
lcl.nlm <- mle.nlm - qnorm(0.975) * se.nlm
lcl.nlm.Y <- preg.in.month.log.L (lcl.nlm, data) 

## 5 Add the MLE to the previous plot by plotting a filled circle at the maximum
## of the likelihood and the returned MLE.

plot.nlm <- plot + geom_point(data=plotdata, mapping=aes(x=mle.nlm, y= mle.nlm.Y, colour="red",shape=21),size=4)+
  geom_point(data=plotdata, mapping=aes(x= ucl.nlm, y = ucl.nlm.Y, color ="red",shape=21),size=4)+
  geom_point(data=plotdata, mapping=aes(x=lcl.nlm, y = lcl.nlm.Y, color ="red",shape=21),size=4) +
  scale_shape_identity()

plot.nlm.anno <- plot.nlm +annotate('text', x = c(0.05,0.2,0.4), y = c(-22,-18.5,-20), label=c("LCL","MLE","UCL"), 
                               size= c(4,4,4),colour="red")+
                           annotate('text', x = c(0.05,0.2,0.4), y = c(-20.5,-17,-18.8), label=round(c(lcl.nlm, mle.nlm, ucl.nlm), digits=2), 
                               size= c(4,4,4),colour="red")
           
plot.nlm.anno

## 9 Load the bbmle package and repeat the optimization using the mle2()

mle2.preg <- mle2(preg.in.month.log.L, list(p = pguess ), data= list(data = data, negll=TRUE))

mle.bbl.preg <- coef(mle2.preg)
mle.bbl.Y <- preg.in.month.log.L (mle.bbl.preg, data)
CI <- confint(mle2.preg)
ucl.mle2.Y <- preg.in.month.log.L (CI[2], data)
lcl.mle2.Y <- preg.in.month.log.L (CI[1], data) 
se.mle2 <- sqrt(diag(vcov(mle2.preg)))


plot.mle2 <- plot.nlm.anno + geom_point(data=plotdata, mapping=aes(x=coef(mle2.preg), y= mle.bbl.Y, colour="blue", shape=21),size=2)+
  geom_point(data=plotdata, mapping=aes(x= confint(mle2.preg)[2], y = ucl.mle2.Y, color ="blue", shape=21), size=4)+
  geom_point(data=plotdata, mapping=aes(x= confint(mle2.preg)[1], y = lcl.mle2.Y, color ="blue", shape=21), size=4)+ 
  ggtitle("MLE of conceiving with 95% \nCI using NLM and BBMLE ")

plot.mle2

plot.all.anno <- plot.mle2 +annotate('text', x = c(0.1,0.2,0.4), y = c(-25,-22,-25), label=c("LCL","MLE","UCL"), 
                                     size= c(4,4,4),colour="blue")+
                            annotate('text', x = c(0.1,0.2,0.4), y = c(-26.5,-23.5,-26.5), label=round(c(confint(mle2.preg)[1], mle.bbl.preg,confint(mle2.preg)[2]), digits=2), 
                                      size= c(4,4,4),colour="blue")


plot.all.anno

ggsave(plot=plot.all.anno,file='PART1.png',h=4, w=6, units="in", dpi=300)
