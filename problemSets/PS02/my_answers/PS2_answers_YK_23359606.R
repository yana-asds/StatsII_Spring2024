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

# Loading data
data <- load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Converting variables 'countries' and 'sanction' into factors and setting reference categories
climateSupport$countries <- factor(climateSupport$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE)
climateSupport$countries <- relevel(climateSupport$countries, ref = "20 of 192")
climateSupport$sanctions <- factor(climateSupport$sanctions, levels = c("5%", "None", "15%", "20%"), ordered = FALSE)
climateSupport$sanctions <- relevel(climateSupport$sanctions, ref = "5%")

# Fitting an additive logistic model
climateSupport_logit <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = climateSupport)
summary(climateSupport_logit)

## Likelihood ratio test (LRT)
#  Create a null model
nullMod <- glm(choice ~ 1, family = binomial(link = "logit"), data = climateSupport)
# Checking results running anova test on the additive model compared to the null model 
anova(nullMod, climateSupport_logit, test = "Chisq") #chi-squared test
anova(nullMod, climateSupport_logit, test = "LRT") #LRT test (equivalent)

# 2b. 
# Calculating predicted probability using function predict
predictedProb <- predict(climateSupport_logit, newdata = data.frame(countries = "80 of 192", sanctions = "None"), type = "response", se = TRUE)
print(predictedProb$fit)

# 2c.
# Fitting an interactive logistic model
climateSupport_logit_interact <- glm(choice ~ countries*sanctions, family = binomial(link = "logit"), data = climateSupport)
stargazer::stargazer(climateSupport_logit, climateSupport_logit_interact, title = "Comparison of Additive and Interactive Models",
                     model.names = TRUE)

# Comparing interactive model with additive model using anova test
anova(climateSupport_logit, climateSupport_logit_interact, test = "LRT")
