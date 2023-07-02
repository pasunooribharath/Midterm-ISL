# Chapter 5 Applied Problem 5:

install.packages("ISLR")
install.packages("ISLR2")
install.packages("boot")
library(ISLR)
head(Default)
set.seed(1)


# a Fit a logistic regression model that uses “income” and “balance” to predict “default” on the “Default” data set.

glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)


# b) Using the validation set approach, estimate the test error of this model.

# i) Split the sample set into a training set and a validation set.


# I will use a 75-25 split when dividing my data into a training set and a validation set.
train <- sample(dim(Default)[1], 0.75*dim(Default)[1])

# ii) Fit a multiple logistic regression model using only the training observations.
glm.fit <- glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
summary(glm.fit)

# iii) Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of
# default for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.

glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.preds <- rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] = "Yes"

# iV) Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.
mean(glm.preds != Default[-train, "default"])


# c) Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set. 
# Comment on the results obtained.

#split 1
train <- sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit <- glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.preds <- rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] <- "Yes"
mean(glm.preds != Default[-train, "default"])

# split 2
train <- sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit <- glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.preds <- rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] <- "Yes"
mean(glm.preds != Default[-train, "default"])

# split 3
train <- sample(dim(Default)[1], 0.75*dim(Default)[1])
glm.fit <- glm(default ~ income + balance, data = Default, subset = train, family = "binomial")
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.preds <- rep("No", dim(Default)[1])
glm.preds[glm.probs > 0.5] <- "Yes"
mean(glm.preds != Default[-train, "default"])


1 - mean(Default[-train, "default"] == "No")

# We see that the validation estimate of the test error rate can be variable, 
# depending on precisely which observations are included in the training set and which observations are included in the validation set.



#d) Now consider a logistic regression model that predicts the probability of default using income, balance, 
# and a dummy variable for student. Estimate the test error for this model using the validation set approach.
# Comment on whether or not including a dummy variable for student leads to a reduction in the test error rate.

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
glm.fit<- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
glm.probs <- predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred <- rep("No", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Yes"
mean(glm.pred != Default[-train, ]$default)

# It doesn’t seem that adding the “student” dummy variable leads to a reduction in the validation set estimate of the test error rate.




