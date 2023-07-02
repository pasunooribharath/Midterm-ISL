#Chapter 5 Applied Problem 9:

install.packages("ISLR")
install.packages("ISLR2")
library(ISLR2)
attach(Boston)
head(Boston)

#a) Based on this data set, provide an estimate for the population mean of medv. Call this estimate ˆµ.

mu.hat <- mean(medv)
mu.hat

# Mean of medv is 22.53


#b)Provide an estimate of the standard error of ˆµ. Interpret this result.

se.hat <- sd(medv) / sqrt(dim(Boston)[1])
se.hat

# For a random sample from the population, we would expect sample mean of “medv” (mu.hat) to differ from population mean of “medv” by approximately 0.41, on average.


#c) Now estimate the standard error of ˆµ using the bootstrap. How does this compare to your answer from (b)?

library(boot)
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
bstrap = boot(medv, boot.fn, 1000)
bstrap

# The bootstrap estimated standard error of μ^ of 0.4106 is very close to the estimate found in (b) of 0.4089.


#d) Based on your bootstrap estimate from (c), provide a 95 % confidence interval for the mean of medv. Compare it to the results obtained using t.test(Boston$medv).

t.test(medv)

CI.mu.hat <- c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)
CI.mu.hat

# The bootstrap confidence interval is very close to the one provided by the t.test() function.


#e) Based on this data set, provide an estimate, ˆµmed, for the median value of medv in the population.

med.hat <- median(medv)
med.hat


#f) We now would like to estimate the standard error of ˆµmed. 
# Unfortunately, there is no simple formula for computing the standard error of the median. 
# Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.


boot.fn <- function(data, index) {
  mu <- median(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)

# Estimated median of 21.2 with SE of 0.378. Small standard error relative to median value.


#g) Based on this data set, provide an estimate for the tenth percentile of medv in Boston census tracts. Call this quantity ˆµ0.1.

medv.tenthperc = quantile(medv, c(0.1))
medv.tenthperc

# The estimated population tenth percentile of CMEDV is ^μ0.1 =12.75.


#h) Use the bootstrap to estimate the standard error of ˆµ0.1. Comment on your findings.

boot.fn <- function(data, index) {
  mu <- quantile(data[index], c(0.1))
  return (mu)
}
boot(medv, boot.fn, 1000)

# Estimated Tenth-percentile of 12.75 with SE of 0.492. Small standard error relative to estimated tenth-percentile value.
