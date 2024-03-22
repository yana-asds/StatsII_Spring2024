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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# Loading data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Creating factor variable for outcome 
gdp_data$GDPWdiff_category <- factor(
  with(gdp_data, ifelse(GDPWdiff == 0, "no change",
                        ifelse(GDPWdiff > 0, "positive", "negative"))),
  levels = c("no change", "positive", "negative"))

# Setting reference category
gdp_data$GDPWdiff_category <- relevel(as.factor(gdp_data$GDPWdiff_category), ref="no change")

# Running unordered multinominal logit regression
unordered_logit <- multinom(GDPWdiff_category ~ OIL + REG, data=gdp_data)
summary(unordered_logit)

# Creating factor to have ordering
gdp_data$GDPWdiff_category <- relevel(gdp_data$GDPWdiff_category, ref="negative")

# Running ordered multinominal logit regression
ordered_logit <- polr(GDPWdiff_category ~ OIL + REG, data=gdp_data)
summary(ordered_logit)


#####################
# Problem 2
#####################

# Loading data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Estimating Poisson model
mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family=poisson)
summary(mexico_poisson)

# Calculating the estimated mean number of visits using predict() function
pred <- data.frame(competitive.district = 1, 
                      marginality.06 = 0, 
                      PAN.governor.06 = 1)
predict(mexico_poisson, newdata = pred, type="response")





