#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)
# Generating 1,000 Cauchy random variables
cauchyData <- rcauchy(1000, location = 0, scale = 1)

# Creating the function
ksTestFunction <- function(data){
  # creating empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generating test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  # calculating p-value
  pValue <- sqrt(2*pi)/D * sum(sapply(1:1000, function(k) {
    exp(-(2*k - 1)^2 * pi^2 / (8 * D^2))
  }))
  # printing result of the function
  list(D = D, P_value = pValue)
  }

# Performing the K-S test on the Cauchy data comparing it 
# to a normal distribution using created function
ksTestFunction(cauchyData)

# Checking my results by using ks.test built-in function
ks.test(cauchyData, "pnorm")


#####################
# Problem 2
#####################

# Creating data
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Define the log-likelihood function 
logLikelihoodFunction <- function(parameter, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  # estimating betas
  beta <- parameter[1:k]
  # estimating sigma squared
  sigma2 <- parameter[k+1]^2
  # estimating residuals
  e <- y - X %*% beta
  # calculating Log-likelihood
  logLikelihood <- -0.5*n*log(2*pi) - 0.5*n*log(sigma2) - (sum(e^2) / (2*sigma2))
  # returning result of the function
  return(-logLikelihood)
}

# Optimization using BFGS method
optim_results <- optim(fn = logLikelihoodFunction, par = c(1, 1, 1), 
                       hessian = TRUE, y=data$y, X=cbind(1, data$x), method = "BFGS")

# Display optimization results
round(optim_results$par, 2)[1:2]

# performing OLS regression using lm function
lmModel <- lm(y~x, data = data)
stargazer::stargazer(lmModel)
