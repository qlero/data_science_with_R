# if needed, set the working directory using the following:
# getwd()
# setwd("~/Programming/R_markdowns_shiny-apps/5_procespin")

library(tidyverse)
library(lmtest) # https://www.rdocumentation.org/packages/lmtest/versions/0.9-38
library(olsrr) # https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html

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
# Scale Location: It displays the fitted values of a regresison model along the
# x-axis and the square roote of the standardized residuals along the y-axis.
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
##### Trying all possible permutations of explanatory variables
######################################################################

all_models <- lm(lny ~ ., data = data %>% select(-y))
### ------ /!\ WARNING: this might take a minute ------
all_models_results <- ols_step_best_subset(all_models)
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


### Validity of the linear model


### Variable selection





