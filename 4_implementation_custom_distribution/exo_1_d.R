library(distr6)
set.seed(42)

pdf <- function(x){
  pdf <- numeric(length(x))
  lower <- self$getParameterValue("lower")
  middle <- self$getParameterValue("middle")
  upper <- self$getParameterValue("upper")
  pdf[x >= lower & x < middle] = 2/3
  pdf[x >= middle & x < upper] = -2/3*(x-3)
  return(pdf)
}

cdf <- function(x){
  cdf <- numeric(length(x))
  lower <- self$getParameterValue("lower")
  middle <- self$getParameterValue("middle")
  upper <- self$getParameterValue("upper")
  cdf[x>= upper] = 1
  cdf[x >= middle & x < upper] =-1/3*x*x+2*x-2
  cdf[x >= lower & x < middle] = 2/3*x-2/3
  return(cdf)
}

ps <- ParameterSet$new(id = list("lower","middle", "upper"), value = c(1,2,3),
                       support = list(set6::Reals$new(),
                                      set6::Reals$new(),
                                      set6::Reals$new()),
                       settable = list(TRUE, TRUE, TRUE))

support <- set6::Interval$new(1, 3)
type <- set6::Reals$new()

U <- Distribution$new(name = "F", 
                      pdf = pdf,
                      cdf = cdf,
                      parameters = ps, 
                      support = support,
                      type = type)

decorate(U, c("CoreStatistics", "ExoticStatistics", "FunctionImputation"))

print(ps)

U$traits
summary(U)

sample_mean <- mean(U$rand(100))
sample_mean

