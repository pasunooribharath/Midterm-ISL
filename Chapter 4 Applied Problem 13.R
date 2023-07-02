# Chapter 4 Applied Problem 13:
install.packages("ISLR")
install.packages("ISLR2")
library(ISLR2)
library(MASS)
library(class)

#a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

data(Weekly)
str(Weekly)
summary(Weekly)

pairs(Weekly)

cor(Weekly[, -9])

attach(Weekly)
plot(Year, Volume)
#When we plot “Year” against “Volume”, there is an increasing trend from Year 1990 to 2008, and it started decreasing from Year 2009 to 2010.

plot(Volume)
# By plotting the data, we see that Volume is increasing over time.

# b) Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. 
# Use the summary function to print the results.
# Do any of the predictors appear to be statistically significant? If so, which ones?


glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family="binomial")
summary(glm.fits)

# The only statistically significant predictor is Lag2 with a p-value of 0.0296 that it is not related to the response Direction. 
# None of the other predictors are statistically significant, though Lag1 is somewhat near the border of being significant at the 10% level, with a p-value of 0.1181.


# c) Compute the confusion matrix and overall fraction of correct predictions. 
# Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.

glm.probs = predict(glm.fits, Weekly, type = "response")
glm.pred = rep("Down", 1089)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Weekly$Direction)

mean(glm.pred == Weekly$Direction)

# As we can see in the confusion matrix, the logistic regression model correctly predicted 54 down weeks out of a total of 484 actual down weeks and 557 up days out of a total of 605 actual up weeks. 
# This means that the model correctly predicted the direction for 611 weeks out of the 1089 for an accuracy of 0.5612.  


#d)Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. 
# Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).

train=(Year<2009)
Weekly.2009= Weekly[!train ,]
dim(Weekly.2009)

glm.fit = glm(Direction ~ Lag2, data = Weekly, subset = train, family = "binomial")
summary(glm.fit)

glm.probs = predict(glm.fit, Weekly.2009, type = "response")
glm.pred = rep("Down", 104)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly.2009$Direction)

mean(glm.pred == Weekly.2009$Direction)

mean(Weekly[!train, ]$Direction == "Up")

# After fitting a logistic regression model on the data from 1990 through 2008 using only Lag2 as the predictor, 
# the model correctly predicted the market direction for 62.5% of the weeks in the held-out data (the data from 2009 and 2010).


#e) Repeat (d) using LDA.
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit, Weekly.2009)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class ,Weekly.2009$Direction)

mean(lda.class == Weekly.2009$Direction)

# After performing LDA on the data from 1990 through 2008 using only Lag2 as the predictor, 
# we ended up with an identical confusion matrix to the one from Part (d) with the logistic regression model. 
# As we saw in Part (d), 
# the model correctly predicted the market direction for 62.5% of the weeks in the held-out data (the data from 2009 and 2010).



#f) Repeat (d) using QDA.
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit

qda.class=predict(qda.fit ,Weekly.2009)$class
table(qda.class ,Weekly.2009$Direction)

mean(qda.class==Weekly.2009$Direction)

# The model correctly predicted the market direction for 58.65% of the weeks in the held-out data (the data from 2009 and 2010).



#g) Repeat (d) using KNN with K = 1.

library(class)

train.X = cbind(Weekly[train, ]$Lag2)
test.X = cbind(Weekly.2009$Lag2)
train.Direction = Weekly[train, ]$Direction

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Weekly.2009$Direction)

mean(knn.pred == Weekly.2009$Direction)

# The results using K = 1 are not very good, since only 50 % of the observations are correctly predicted.



#h) Repeat (d) using naive Bayes.
install.packages('e1071', dependencies=TRUE)
library(e1071)
NB.fits <- naiveBayes(Direction ~ Lag2, data = Weekly, subset = train)
NB.preds <- predict(NB.fits, Weekly.2009)
table(NB.preds, Weekly.2009$Direction)

mean(NB.preds == Weekly.2009$Direction)

# The model correctly predicted the market direction for 58.65% of the weeks in the held-out data (the data from 2009 and 2010).



# i) Which of these methods appears to provide the best results on this data?

#  It seems that the models that performed the best on these data were logistic regression, linear discriminant analysis, and naive Bayes.
 
