### Statistics

Assignment 6#Assignment 6
Assignment 7#Assignment 7
Assignment 8#Assignment 8
Assignment 9#Assignment 9
Assignment 10#Assignment 10

## Assignment 6

#Part 1 Cereal data
• Read in data from a *.csv file.
• Check that you have read the data properly using the str(), dim(),
     head() and tail() functions.
• Construct a basic scatterplot with jittering using ggplot().
• Fit a simple linear regression using lm()
• Extract information from the fit using the standard method functions.
• Get predictions from this fit.
• Plot the fitted line on an existing plot.
• Save TEXTUAL and GRAPHICAL information to files.
• Create an HTML notebook that integrates source and output into
     one file.variety of short analysis

#Part 2 Titanic data

• Read data from the web
• Use the table() and xtabs() functions to check the data
• Compute odds, log-odds and the inverse from base probabilities.
• Fit a two-factor logistic ANOVA
• Extract the marginal estimates from the model using the lsmeans
     package.
• Plot a profile plot with confidence limits at each point.

## Assignment 7

#Part 1 Cereal Data
• create a factor variable; specifying the order of the levels of the factor;
• dealing with missing values with standard summary functions;
• learn how install a package;
• learn to use the recode() function in the car package.
• use the plyr package and the split-apply-combine paradigm;
• create dot, box, and notched-box plots using the ggplot() function;
• improve plots through jittering;
• what is the difference between a factor and a variable and the consequence
of mixing these up;
• do a single factor CRD ANOVA using the lm() function;
• extract estimated marginal means (LSMEANS) from lm() using the lsmeans
package and make a suitable plot.

#Part 2 Accident Data UK Database

• import dates and times;
• format dates and time for display purposes;
• extract features of dates and times, such as the year, month, day, hour,
minute;
• create summaries at a grosser level

##Assignment 8

#Part 1

• take a continuous variable and create a categorical version of it;
• create dot plots, box plots, notched-box plots, and bar charts using ggplot()
• improve plots through jittering;
• use the t.test() function and compute the se of the difference in means;
• do a single factor ANOVA using the lm() function;
• extract estimated marginal means using the lsmeans() function from the
     lsmeans package and make a suitable plot.
• why se and ci’s differ for marginal means when computed using summary
     data from each group separately or from a pooled analysis.
     
#Part 2

• import date-time values;
• compute the odds ratio for fatalities for individual hours using the plyr
   package
• compute the odds ratio for fatalities for individual hours using a modeling
   approach
• use the lsmeans package for glm() objects

##Assignment 9

#Part 1

• do simple bootstrapping to estimate the se and confidence interval of a
  statistic
• interpret the bootstrap sampling distribution

#Part 2

• how to select samples from a population
• compute an index from each sample
• find the SE of this index from replicate samples generated using the rdply()
   function in the plyr package.
• plot the se as a function of sample size
• see if the results on your plot above is consistent with generally accepted
   principles
   
#Part 3

Suarez-Rodriguez, M., Lopez-Rull, I. and Garcia, C.M. (2013).
Incorporation of cigarette butts into nests reduces nest ectoparasite
load in urban birds: new ingredients for an old recipe?
Biological Letters 9, 20120931.
http://dx.doi.org/10.1098/rsbl.2012.0931

• use R to read Excel spreadsheets directly;
• use summary functions to examine standard deviations and balance in
experimental designs;
• write a model for a two factor CRD ANOVA; notice that R gives you
incremental rather than marginal sums of squares
• write a model for a two factor CRD ANCOVA;
• “reverse engineer” an analysis in a paper to try and figure out what was
done.
• use the t.test() function
• how to specify subsets of observations in analyses using the subset= argument