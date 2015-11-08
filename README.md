# Statistics

- [Assignment 6](#assignment-6)
- [Assignment 7](#assignment-7)
- [Assignment 8](#assignment-8)
- [Assignment 9](#assignment-9)
- [Assignment 10](#assignment-10)

## Assignment 6

###Part 1 Cereal data
1. Read in data from a *.csv file.
2. Check that you have read the data properly using the str(), dim(),
head() and tail() functions.
3. Construct a basic scatterplot with jittering using ggplot().
4. Fit a simple linear regression using lm()
5. Extract information from the fit using the standard method functions.
6. Get predictions from this fit.
7. Plot the fitted line on an existing plot.
8. Save TEXTUAL and GRAPHICAL information to files.
9. Create an HTML notebook that integrates source and output into
one file.variety of short analysis

###Part 2 Titanic data

1. Read data from the web
2. Use the table() and xtabs() functions to check the data
3. Compute odds, log-odds and the inverse from base probabilities.
4. Fit a two-factor logistic ANOVA package.
5. Plot a profile plot with confidence limits at each point.

## Assignment 7

###Part 1 Cereal Data
1. Create a factor variable; specifying the order of the levels of the factor;
2. Dealing with missing values with standard summary functions;
3. Use the recode() function in the car package.
4. Use the plyr package and the split-apply-combine paradigm;
5. Create dot, box, and notched-box plots using the ggplot() function;
6. improve plots through jittering;
7. Difference between a factor and a variable and the consequence
of mixing these up;
8. D a single factor CRD ANOVA using the lm() function;
9. Extract estimated marginal means (LSMEANS) from lm() using the lsmeans
package and make a suitable plot.

###Part 2 Accident Data UK Database

1. iIport dates and times;
2. Format dates and time for display purposes;
3. Extract features of dates and times, such as the year, month, day, hour,
minute;
4. Create summaries at a grosser level

##Assignment 8

###Part 1

1. Take a continuous variable and create a categorical version of it;
2. Create dot plots, box plots, notched-box plots, and bar charts using ggplot()
3. Improve plots through jittering;
4. Use the t.test() function and compute the se of the difference in means;
5. Do a single factor ANOVA using the lm() function;
6. Extract estimated marginal means using the lsmeans() function from the
lsmeans package and make a suitable plot.
7. Why SE and CI’s differ for marginal means when computed using summary
     data from each group separately or from a pooled analysis.
     
###Part 2

1. Import date-time values;
2. Compute the odds ratio for fatalities for individual hours using the plyr
package
3. Compute the odds ratio for fatalities for individual hours using a modeling
approach
4. Use the lsmeans package for glm() objects

##Assignment 9

###Part 1

1. Complete simple bootstrapping to estimate the se and confidence interval of a
statistic
2. Interpret the bootstrap sampling distribution

###Part 2

1. Select samples from a population
2. Compute an index from each sample
3. Find the SE of this index from replicate samples generated using the rdply()
function in the plyr package.
4. Plot the se as a function of sample size
5. See if the results on your plot above are consistent with generally accepted
   principles
   
###Part 3

Suarez-Rodriguez, M., Lopez-Rull, I. and Garcia, C.M. (2013).
Incorporation of cigarette butts into nests reduces nest ectoparasite
load in urban birds: new ingredients for an old recipe?
Biological Letters 9, 20120931.
http://dx.doi.org/10.1098/rsbl.2012.0931

1. use R to read Excel spreadsheets directly;
2. use summary functions to examine standard deviations and balance in
experimental designs;
3. write a model for a two factor CRD ANOVA; notice that R gives you
incremental rather than marginal sums of squares
4. write a model for a two factor CRD ANCOVA;
5.“reverse engineer” an analysis in a paper to try and figure out what was
done.
6. use the t.test() function
7. how to specify subsets of observations in analyses using the subset= argument
