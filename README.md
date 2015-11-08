# Statistics

Assignment 6#Assignment 6
Assignment 7#Assignment 7
Assignment 8#Assignment 8
Assignment 9#Assignment 9
Assignment 10#Assignment 10

## Assignment 6

###Part 1 Cereal data
..1. Read in data from a *.csv file.
..2. Check that you have read the data properly using the str(), dim(),
....head() and tail() functions.
..3. Construct a basic scatterplot with jittering using ggplot().
..4. Fit a simple linear regression using lm()
..5. Extract information from the fit using the standard method functions.
..6 Get predictions from this fit.
..7 Plot the fitted line on an existing plot.
..8 Save TEXTUAL and GRAPHICAL information to files.
..9 Create an HTML notebook that integrates source and output into
....one file.variety of short analysis

###Part 2 Titanic data

..1. Read data from the web
..2. Use the table() and xtabs() functions to check the data
..3. Compute odds, log-odds and the inverse from base probabilities.
..4. Fit a two-factor logistic ANOVA
....package.
..5. Plot a profile plot with confidence limits at each point.

## Assignment 7

###Part 1 Cereal Data
..1. create a factor variable; specifying the order of the levels of the factor;
..2. dealing with missing values with standard summary functions;
..3. learn how install a package;
..4. learn to use the recode() function in the car package.
..5. use the plyr package and the split-apply-combine paradigm;
..6. create dot, box, and notched-box plots using the ggplot() function;
..7. improve plots through jittering;
..8. what is the difference between a factor and a variable and the consequence
....of mixing these up;
..9. do a single factor CRD ANOVA using the lm() function;
..10. extract estimated marginal means (LSMEANS) from lm() using the lsmeans
....package and make a suitable plot.

###Part 2 Accident Data UK Database

..1. import dates and times;
..2. format dates and time for display purposes;
..3.extract features of dates and times, such as the year, month, day, hour,
....minute;
..4. create summaries at a grosser level

##Assignment 8

###Part 1

..1.take a continuous variable and create a categorical version of it;
..2. create dot plots, box plots, notched-box plots, and bar charts using ggplot()
..3. improve plots through jittering;
..4. use the t.test() function and compute the se of the difference in means;
..5. do a single factor ANOVA using the lm() function;
..6. extract estimated marginal means using the lsmeans() function from the
....lsmeans package and make a suitable plot.
..7. why se and ci’s differ for marginal means when computed using summary
     data from each group separately or from a pooled analysis.
     
###Part 2

..1.import date-time values;
..2. compute the odds ratio for fatalities for individual hours using the plyr
...package
..3.compute the odds ratio for fatalities for individual hours using a modeling
....approach
..4.use the lsmeans package for glm() objects

##Assignment 9

###Part 1

..1.do simple bootstrapping to estimate the se and confidence interval of a
....statistic
..2.interpret the bootstrap sampling distribution

###Part 2

..1.how to select samples from a population
..2.compute an index from each sample
..3.find the SE of this index from replicate samples generated using the rdply()
....function in the plyr package.
..4.plot the se as a function of sample size
..5.see if the results on your plot above is consistent with generally accepted
   principles
   
###Part 3

Suarez-Rodriguez, M., Lopez-Rull, I. and Garcia, C.M. (2013).
Incorporation of cigarette butts into nests reduces nest ectoparasite
load in urban birds: new ingredients for an old recipe?
Biological Letters 9, 20120931.
http://dx.doi.org/10.1098/rsbl.2012.0931

1. use R to read Excel spreadsheets directly;
2. use summary functions to examine standard deviations and balance in
....experimental designs;
3. write a model for a two factor CRD ANOVA; notice that R gives you
...incremental rather than marginal sums of squares
4. write a model for a two factor CRD ANCOVA;
5.“reverse engineer” an analysis in a paper to try and figure out what was
....done.
6. use the t.test() function
7. how to specify subsets of observations in analyses using the subset= argument