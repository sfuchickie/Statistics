#
#
#    Assignment 9, Part 3
#    Read in cereal data set plot and 
#    perform basic analysis
#    Instructor: Dr. Schwarz
#    Saida Amirova 301086263
#    March 20th, 2015
#   – use summary functions to examine standard deviations and balance in
#     experimental designs;
#   – write a model for a two factor CRD ANOVA; notice that R gives you
#     incremental rather than marginal sums of squares
#   – write a model for a two factor CRD ANCOVA;
#   – “reverse engineer” an analysis in a paper to try and figure out what was
#     done.
#   – use the t.test() function
#   – how to specify subsets of observations in analyses using the subset= argument
#
############################################

#rm(list=ls())
setwd("/Users/Saida/Desktop/STAT/Assignment 9")
library(xlsx)
library(plyr)
library(ggplot2)
library(car)
ndf <- read.xlsx('nests.xls', sheetName="Correlational",
                 header=TRUE, stringsAsFactors=FALSE)

xtabs(~Species+Nest.content, data=ndf)

my.summary<- function(df, var, alpha=.05){
  values   <- df[,var]
  mymean   <- mean(values, na.rm=TRUE)
  mysd     <- sd(values,na.rm=TRUE)
  total    <- length(values)
  nonmiss  <- length(na.omit(values))
  se       <- mysd/sqrt(nonmiss)
  lcl      <- mymean - qt(1-alpha/2,nonmiss-1)*se
  ucl      <- mymean + qt(1-alpha/2,nonmiss-1)*se
  return(data.frame(mymean,mysd,se,lcl,ucl))
}

report<-ddply(ndf, c("Species","Nest.content"), my.summary, "Number.of.mites")
report

plotInt <- ggplot(data=report, aes(x=Nest.content, y=mymean,
                                   group=Species, color=Species, linetype=Species))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2, position="dodge")+
  ggtitle("Interaction plot with 95% interval")+
  ylab("Mean number of mites")+xlab("Nest Content")
plotInt

ndf$SpeciesF<-as.factor(ndf$Species)
str(ndf$SpeciesF)
ndf$Nest.contentF<-factor(ndf$Nest.content,levels=c("empty","eggs","chicks",ordered=TRUE))
str(ndf$Nest.contentF)

#Linear model

fit.lm<-lm(Number.of.mites ~ SpeciesF + Nest.contentF + SpeciesF:Nest.contentF, data=ndf)
fit.lm

fit.lm.2<-lm(Number.of.mites ~ Nest.contentF + SpeciesF + SpeciesF:Nest.contentF, data=ndf)
fit.lm.2

fit.lm.3<-lm(Number.of.mites ~ SpeciesF:Nest.contentF + Nest.contentF + SpeciesF, data=ndf)
fit.lm.3

#T-TEST which isnt very intersting
summary(fit.lm)
summary(fit.lm.2)
summary(fit.lm.3)

#Analysis of variance - interesting

anova(fit.lm)
anova(fit.lm.2)
anova(fit.lm.3)

#CAR packages has an extention to lm() analysis
  #however one must change the way lm() fits the model notice Anova not anova
?Anova

cat("\n\nUse the Type III tests from the Anova() function from the car package")
cat( "\nbut you need to set the treatment contrasts to sum rather than treatment")
cat( "\nBEFORE fitting the lm() model!")
cat(" \nSee http://r.789695.n4.nabble.com/Type-I-v-s-Type-III-Sum-Of-Squares-in-ANOVA")

#Notice the DF which adds up to the total amount of nests
#However the paper uses only 42! which 42 is another question.
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts
my.lm.model <- lm(Number.of.mites ~ SpeciesF + Nest.contentF + SpeciesF:Nest.contentF, 
                  data=ndf)
Anova(my.lm.model, type=3)
#options(old.options)

# Create a logical that will use the first 42 nests in the subset of lm()

x<-t(rep(TRUE,42))
y<-t(rep(FALSE,15))
xy<-cbind(x,y)
xy<-as.vector(xy)
ndf$isit<-xy

#TRYING TO REVERSE ENGINEER the analysis from the paper with the first 42 nests
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts
their.lm.model <- lm(Number.of.mites ~SpeciesF + Nest.contentF + 
                       SpeciesF:Nest.contentF + Butts.weight, data=ndf, subset=xy)
Anova(their.lm.model, type=3)
options(old.options)

#Looking at residuals
source("http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r")
diagplot <- sf.autoplot.lm(their.lm.model)
diagplot

#Looking at log number of mites
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts
my.log.lm.model <- lm(log(Number.of.mites) ~ SpeciesF + Nest.contentF + SpeciesF:Nest.contentF + Butts.weight, 
                      data=ndf)
Anova(my.log.lm.model, type=3)
Anova(my.lm.model, type=3)
options(old.options)
names(my.log.lm.model)

#Creating a logical that specifies which rows are outliers
ndf$residuals<-my.log.lm.model$residuals
ndf$outTrue<-"True"
ndf$outTrue[ndf$residuals %in% c(max(ndf$residuals),min(ndf$residuals))]<-"False"

#Now putting in the weight of the butts in the model
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts
nobutts.log.lm.model <- lm(log(Number.of.mites) ~ SpeciesF + Nest.contentF + 
                         SpeciesF:Nest.contentF, data=subset(ndf,outTrue=="True"))
Anova(nobutts.log.lm.model, type=3)
Anova(my.log.lm.model, type=3)

# Now using the entire dataset
old.options <- options()
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
options()$contrasts
butts.log.lm.model <- lm(log(Number.of.mites) ~ SpeciesF + Nest.contentF + 
                         SpeciesF:Nest.contentF + Butts.weight, data=subset(ndf,outTrue=="True"))
Anova(nobutts.log.lm.model, type=3)
Anova(butts.log.lm.model, type=3)






