# Chapter 5 Applied Problem  6:

#a) Using the summary() and glm() functions, determine the estimated standard errors for the coefficients associated with income
# and balance in a multiple logistic regression model that uses both predictors.
install.packages("ISLR2")
install.packages("boot")
library(ISLR)
set.seed(1)
attach(Default)
head(Default)


glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

# Using the summary() and glm() functions, the estimated standard error for the estimated 
# coefficient of income is 4.985 * 10^-6, 
# while the estimated standard error for the estimated coefficient of balance is 2.274×10^-4 .



#b)Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, and that outputs
# the coefficient estimates for income and balance in the multiple logistic regression model.

boot.fn = function(data, index){
  coefs = coef(glm(default ~ income + balance, data = data, subset = index, 
                   family = "binomial"))[c("income", "balance")]
  return(coefs)
}


#c)Use the boot() function together with your boot.fn() function to
# estimate the standard errors of the logistic regression coefficients for income and balance.

library(boot)
boot(Default, boot.fn, 1000)

# The estimated standard errors of the logistic regression coefficients for income and balance, using the bootstrap method, are 4.866 * 10 ^ - 6 and 2.299 * 10 ^ - 4, respectively.



#d) Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function.

# The estimated standard errors obtained by the two methods are pretty close to those obtained using the statistical formulas underlying the glm() function.

