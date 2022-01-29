# if needed, set the working directory using the following:
# getwd()
setwd("~/Dev/R_apps/5_procespin")

library(tidyverse)
library(lmtest) # https://www.rdocumentation.org/packages/lmtest/versions/0.9-38
library(olsrr) # https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
library(car)

######################################################################
##### Loading and overview of the dataset
######################################################################

data <- read.csv("procespin.txt", sep="\t")
ncol(data) #11 columns: y and 10 explanatory variables x1, ..., x10
nrow(data) #33 rows: data points
summary(data)

# We create the response variable ln(y) and plot the result
data$lny <- log(data$y)
plot(data$y, data$lny)

######################################################################
##### Simple Linear Model with explanatory variables x1, x2, ..., x10
######################################################################

### Model implementation
simple_model = lm(lny ~ ., data = data %>% select(-y))
summary(simple_model)

### Observations
# We observe that the x1, x2, x4, and x5 are the only significant variables
# with a respective p-value of 0.00795, 0.02521, 0.02632, and 0.02797
# Incidentally, the Intercept also shows a small (i.e. significant) p-value 
# 0.00171.
# the Adjusted R-squared of 0.5519 indicates that the model is not very good
# at predicting the response variable. As a reminder: The more variables, the 
# more variance a model is going to explain; the adj.-R2 allows to control for
# this.
# The F-statistic shows a value of 4.941 with a p-value of 0.0008645, which 
# indicates that at least one variable has explanatory power/is significantly 
# related to Y.

plot(simple_model)

### Observations
# Residuals vs. Fitted: This scatter plot is used to detect non-linearity. We 
# find that the assumption that the relationship is linear holds as the
# residuals form a 'horizontal band' around the residual (=0) line.
# helping sources:
#   - https://blog.minitab.com/blog/the-statistics-game/checking-the-assumption-of-constant-variance-in-regression-analyses
#   - https://online.stat.psu.edu/stat501/lesson/4/4.2
# 
# Normal Q-Q: It compares the distribution of two sets of data, here the goal 
# is to see whether the residuals follow a somewhat gaussian distribution. 
# Since most of the points on the graph fall along the line except for some at
# the extremities, we can say that residuals are roughly standardly distributed
# helping source:
#   - https://boostedml.com/2019/03/linear-regression-plots-how-to-read-a-qq-plot.html
# 
# Scale Location: It displays the fitted values of a regression model along the
# x-axis and the square root of the standardized residuals along the y-axis.
# it helps verify the homoscedasticity of the dataset for a given regression.
# Since the drawn pattern fails to be somewhat horizontal, showing instead a 
# vaguely sinusoidal look, we can say that the spread of the residuals
# is unequal for the fitted value.
# Checking the model's homosedasticity with the Breusch-Pagan test

bptest(simple_model)

# we find a p-value of 0.5054, i.e., we fail to reject the null hypothesis that 
# the residuals are homoscedastic. Our intuition was false
# helping source:
#   - https://www.statology.org/scale-location-plot/
# 
# Residual vs. Leverage: Leverage describes how far a variable x is from other
# variables in a model, i.e., how sensitive the fitted y_hat is to a change in 
# the response variable y. The plot shows the spread of standardized residuals' 
# change as the leverage increases. This can be used to detect 
# heteroskedasticity and non-linearity.
# As such We see that the spread of standardized residuals doesn't seem to 
# change as a function of leverage, indicating linearity. Points being outside
# the Cook's distance seem to highlight that no single variable have an
# overwhelming influence on the fitting
# helping source:
#   - https://boostedml.com/2019/03/linear-regression-plots-residuals-vs-leverage.html
#
# Independence of residuals' error terms: One of the main assumptions in linear 
# regression is that there is no correlation between consecutive residuals. 
# In other words, it’s assumed that the residuals are independent.
# When this assumption is violated, the standard errors of the coefficients in 
# a regression model are likely to be underestimated which means predictor 
# variables are more likely to be deemed statistically significant when they’re 
# actually not.
# One way to determine if this assumption is met is to perform a Durbin-Watson 
# test

durbinWatsonTest(simple_model)

# we find a p-value of 0.998, i.e., we fail to reject the null hypothesis that 
# the residuals are not correlated

# Finally, to test the validity of the model, we can resort to the F-test of 
# overall significance. It indicates whether a linear regression model provides 
# a better fit to the data than a model that contains no independent variables. 



######################################################################
##### Linear Model with variable selection: x1, x2, x4, and x5
######################################################################

# Based on the p-values obtained in our first model, we can try reducing our
# set of explanatory variables to only the significant ones

### Model implementation

reduced_model = lm(lny ~ ., data = data %>% select(c(lny, x1, x2, x4, x5)))
summary(reduced_model)
plot(reduced_model)

### Observations
# We observe that the kept variables are all significant. Furthermore, it 
# appears that our adjusted R-squared increased from 0.5519 to 0.5998. This is
# progress.

######################################################################
##### Trying the best possible permutations of explanatory variables
######################################################################

all_models <- lm(lny ~ ., data = data %>% select(-y))

### ------ /!\ WARNING: this might take a minute ------
all_models_results <- ols_step_best_subset(all_models)
print(all_models_results)
### ---------------------------------------------------

plot(all_models_results)

### Observations
# We find that the model will the best adjusted R-squared is the one with the
# following mixture of explanatory variables: x1, x2, x4, x5, x6, x9
# We find once again the four variables we had managed to single out above.
# However, we see that this model is only barely more performing than the
# models with the following mixtures: "x1, x2, x4, x5" (our previous model), 
# "x1, x2, x4, x5, x9" (of those two, our previous model has the lowest Akaike
# Information Criterion, indicating that our guess model was likely the right
# one).
# We find similar conclusions from all the other available criteria. For 
# instance, the best final prediction error is that of the "x1, x2, x4, x5" 
# (our previous model) again.

######################################################################
##### CONCLUSION
######################################################################

### Assumptions associated to a linear model
# 1. Linearity of the data:
# We used the "Residuals vs. Fitted"plot to check the linear relationship of 
# the model's data (i.e. the relationship between the predictors x1, ..., x10) 
# and the outcome lny. The relatively horizontal line without any clear, 
# distinct pattern indicates a likely linear relationship
#
# 2. Normality of residuals: 
# We used the "Q-Q" plot to check whether the truthfulness of the assumption 
# that the residuals are normally distributed. Most of the points on the graph 
# follow the (x=y) dashed line, indicating that the standard distribution of
# residuals seems to hold
#
# 3. Homogeneity of the residuals' variance/homoscedasticity:
# We used the "Scale-Location" plot to check the assumption that residuals have
# a constant variance (homoscedasticity). Since we could not firmly conclude 
# from the line drawn on the graph, we followed up with a Breusch-Pagan test 
# (resulting in a p-value of 0.5054) that confirmed the heteroscedasticity 
# assumption.
#
# 4. Independence of residuals' error terms
# We used the Durbin-Watson test to verify that there is no correlation between
# error terms. The test resulted in a 0.998 p-value meaning that we cannot 
# reject the null-hypothesis that the error terms are not correlated. The 
# independence assumption holds.
# 
# 5. Outliers and high leverage points:
# An outlier is a point that has an extreme outcome variable value. Their 
# presence in the dataset may affect the interpretation of the model (They 
# increase the Residual Standard Error).
# High leverage points are data points with extreme predictor x values. 
# We used the "Residuals vs Leverage" plot to check for those points. We find 
# that the most extreme points have standardized residuals at worst around 2
# and -2. Furthermore, there is no high leverage point in the data: most are 
# below the leverage statistic (2*(p+1)/n with p the number of predictors (10)
# and n the number of observations (33)): 0.667
# 
# helping source:
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

### Validity of the linear model
# Given our linear model y_hat = a + sum{1 to 10} b_i . x_i:
# We saw that the assumptions of the linear model (1) linearity of the data, 2)
# normality of residuals, 3) homoscedasticity, 4) independence of residuals' 
# error terms, 5) low amount of outliers and high leverage points) are verified 
# in R linear fit models with plots as seen above
# We saw that a baseline model reaches an adjusted R-squared of 0.5519.
# We saw that the F-statistic shows a value of 4.941 with a p-value of 0.0008645
# which indicates that at least one variable has explanatory power/is 
# significantly related to Y
# In conclusion, we can consider the model as valid. The logical next step would
# be to refine the model by performing variable selection.
# 
# helping source:
# https://www3.nd.edu/~steve/Rcourse/Lecture8v1.pdf

### Variable selection
# Based on the baseline model, we find that the variables x1, 2, x4, and x5 are 
# the most significant. Let's call this mix "mix_intuition".
# Thanks to the OLSRR library, we calculate the best models and find that the 
# following mixes have the highest adjusted R-squared:
#    1. x1, x2, x4, x5 (our mix_intuition)
#    2. x1, x2, x4, x5, x9
#    3. x1, x2, x4, x5, x6, x9
# For instance our mix_intuition yields an adjusted R-squared of 0.5998, beating
# our baseline model, along with the best Akaike Information Criteria (84.4646).
# The mix with the best adjusted R-squared is: x1, x2, x4, x5, x6, x9.

