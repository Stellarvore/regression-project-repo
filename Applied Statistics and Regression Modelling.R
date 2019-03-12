############# Applied Statistics - Modelling ##############

# Load in the library

library(MASS)

# Read in the Dataset Boston

data(Boston)

# View the Dataset

View(Boston)

# Accessing the information on the Boston dataset

?Boston

# a) 

# Using the summary funtion we can access the descriptive statistics of the dataset

summary(Boston)

# Calculate the pearson correlation coefficient

cor(Boston)[1,]

# corrplot plots the correlation cefficients

# install.packages("corrplot")

library(corrplot)

corrplot.mixed(cor(Boston), upper = "circle")
title("Correlation plot of Boston Housing Dataset",line = 3.2,adj=.5)

# Using the pair function we can view the scatter plot of the per capita crime rate 
# based all the elements of the Boston Dataset 

# install.packages("ggplot2")

# install.packages("reshape2")

library(ggplot2)

library(reshape2)

ggplot(data = melt(Boston, id="crim"), aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") +
  geom_point(color = "black") + 
  ggtitle("Scatterplot of crim based on all variables of Boston") +
  xlab("") + 
  ylab("Per capita crime rate")

# Creating a histogram of each variable of the Boston data set

ggplot(data = melt(Boston),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram(bins = 10,color = "black",fill = "green") + 
  ggtitle("Histogram of each variables in Boston") +
  xlab("") + 
  ylab("")


ggplot(Boston, aes(x=1:nrow(Boston), y=crim)) + geom_point() + 
  ggtitle("Per capita crime on each of the predictor") + 
  ylab("Per capita crime rate") +
  xlab("Predictors")

#----------------------------------------------------------------------------------------------------------#

# b) 
# Using per capita crime rate as the response and the other variables as the predictors we can fit a simple 
# linear regression model with each predictors individually using the following function.

my.lm <- function(y, x) {
  res <- vector("list", ncol(x))
  for(i in 1:ncol(x)) {
    res[[i]] <- lm(y ~ x[,i])
  }
  names(res) <- names(x)
  return(res)
}

# Extract y (response: Per capita crime rate) and 
# x (Predictors:zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)

y <- Boston[,1]
x <- Boston[,-1]

# Run the function:

slm <- my.lm(y, x)

# 1.Y (Response: Per capita crime rate) and X (Predictor:zn)

# Viewing the summary of the model

summary(slm$zn)

par(mfrow=c(2,2)) # Plotting the graph 4 by 4 in a single tab

# Plotting the residuals

plot(slm$zn)

# 2.Y (Response: Per capita crime rate) and X (Predictor: indus)

# Viewing the summary of the model

summary(slm$indus)

# Plotting the residuals
  
plot(slm$indus)

# 3.Y (Response: Per capita crime rate) and X (Predictor: chas)

# Viewing the summary of the model

summary(slm$chas)

# Plotting the residuals

plot(slm$chas)

# 4.Y (Response: Per capita crime rate) and X (Predictor: nox)

# Viewing the summary of the model

summary(slm$nox)

# Plotting the residuals

plot(slm$nox)

# 5.Y (Response: Per capita crime rate) and X (Predictor: rm)

# Viewing the summary of the model

summary(slm$rm)

# Plotting the residuals

plot(slm$rm)

# 6.Y (Response: Per capita crime rate) and X (Predictor: age)

# Viewing the summary of the model

summary(slm$age)

# Plotting the residuals

plot(slm$age)

#  7.Y (Response: Per capita crime rate) and X (Predictor: dis)

# Viewing the summary of the model

summary(slm$dis)

# Plotting the residuals

plot(slm$dis)

#  8.Y (Response: Per capita crime rate) and X (Predictor: rad)

# Viewing the summary of the model

summary(slm$rad)

# Plotting the residuals

plot(slm$rad)

#  9.Y (Response: Per capita crime rate) and X (Predictor: tax)

# Viewing the summary of the model

summary(slm$tax)

# Plotting the residuals

plot(slm$tax)

#  10.Y (Response: Per capita crime rate) and X (Predictor: ptratio)

# Viewing the summary of the model

summary(slm$ptratio)

# Plotting the residuals

plot(slm$ptratio)

#  11.Y (Response: Per capita crime rate) and X (Predictor: black)

# Viewing the summary of the model

summary(slm$black)

plot(slm$black)

#  12.Y (Response: Per capita crime rate) and X (Predictor: lstat)

# Viewing the summary of the model

summary(slm$lstat)

# Plotting the residuals

plot(slm$lstat)

#  13.Y (Response: Per capita crime rate) and X (Predictor: medv)

# Viewing the summary of the model

summary(slm$medv)

# Plotting the residuals

plot(slm$medv)

#----------------------------------------------------------------------------------------------------------#

# c)
# Fitting a Multiple regression model using y (response: Per capita crime rate) and 
# x (Predictors:zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)

lm.fit=lm(crim~.,data=Boston)

# Viewing the summary of the model

summary(lm.fit)

confint(lm.fit,level=.95)

# Plotting the residuals

plot(lm.fit)

par(mfrow=c(1,1)) #  Return to one plot per page

#----------------------------------------------------------------------------------------------------------#

# d)
# Comparison of univariate & multiple regression coefficients 

# Creating a vector with the coefficients of the predictors of the simple linear regression 

slmcof <- c(slm$zn$coefficients[2],slm$indus$coefficients[2],slm$chas$coefficients[2],
            slm$nox$coefficients[2],slm$rm$coefficients[2],slm$age$coefficients[2],
            slm$dis$coefficients[2],slm$rad$coefficients[2],slm$tax$coefficients[2],
            slm$ptratio$coefficients[2],slm$black$coefficients[2],
            slm$lstat$coefficients[2],slm$medv$coefficients[2])

# Creating a vector with the coefficients of the predictors of multiple regression 

mulcof <- coef(lm.fit)[2:14]

# Creating a data frame with the coefficents of the predictors of simple linear regression and multiple regression

df <- data.frame(slmcof,mulcof)

ggplot(df, aes(x=slmcof, y=mulcof))+
  geom_point(color = "darkred") + 
  ggtitle("Comparison of Univariate and Multiple regresson coefficients") +
  xlab("Univariate regression coefficients") + 
  ylab("Multiple regression coefficients")

#----------------------------------------------------------------------------------------------------------#

# e) 
# Non- Linear transformation of the predictors using the model 
# 𝑌 = 𝛽0+𝛽1 𝑋 + 𝛽2𝑋2+𝛽3𝑋3+ Ԑ

my.lm2 <- function(y, x) {
  res <- vector("list", ncol(x))
  for(i in 1:ncol(x)) {
    res[[i]] <- lm(y ~ x[,i]+I(x[,i]^2)+I(x[,i]^3))
  }
  names(res) <- names(x)
  return(res)
}

# Extract y (response: Per capita crime rate) and 
# x (Predictors:zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)

y <- Boston[,1]
x <- Boston[,-1]

# Run the function:

nla <- my.lm2(y, x)

# 1.Non linear transformation of zn

# Viewing the summary of the model

summary(nla$zn)

par(mfrow=c(2,2))

# Plotting the residuals

plot(nla$zn)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$zn,nla$zn)

# 2.Non linear transformation of indus

# Viewing the summary of the model

summary(nla$indus)

# Plotting the residuals

plot(nla$indus)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$indus,nla$indus)

# 3.Non linear transformation of chas

# Viewing the summary of the model

summary(nla$chas)

# Plotting the residuals

plot(nla$chas)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$chas,nla$chas)

# 4.Non linear transformation of nox

# Viewing the summary of the model

summary(nla$nox)

# Plotting the residuals

plot(nla$nox)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$nox,nla$nox)

# 5.Non linear transformation of rm

# Viewing the summary of the model

summary(nla$rm)

# Plotting the residuals

plot(nla$rm)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$rm,nla$rm)

# 6.Non linear transformation of age

# Viewing the summary of the model

summary(nla$age)

# Plotting the residuals

plot(nla$age)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$age,nla$age)

# 7.Non linear transformation of dis

# Viewing the summary of the model

summary(nla$dis)

# Plotting the residuals

plot(nla$dis)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$dis,nla$dis)

# 8.Non linear transformation of rad

# Viewing the summary of the model

summary(nla$rad)

# Plotting the residuals

plot(nla$rad)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$rad,nla$rad)

# 9.Non linear transformation of tax

# Viewing the summary of the model

summary(nla$tax)

# Plotting the residuals

plot(nla$tax)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$tax,nla$tax)

# 10.Non linear transformation of ptratio

# Viewing the summary of the model

summary(nla$ptratio)

# Plotting the residuals

plot(nla$ptratio)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$ptratio,nla$ptratio)

# 11.Non linear transformation of black

# Viewing the summary of the model

summary(nla$black)

# Plotting the residuals

plot(nla$black)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$black,nla$black)

# 12.Non linear transformation of lstat

# Viewing the summary of the model

summary(nla$lstat)

# Plotting the residuals

plot(nla$lstat)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$lstat,nla$lstat)

# 13.Non linear transformation of medv

# Viewing the summary of the model

summary(nla$medv)

# Plotting the residuals 

plot(nla$medv)

# Using anova to compare the simple linear model and the cubic fit model

anova(slm$medv,nla$medv)

par(mfrow=c(1,1)) # Return to one plot per page


#----------------------------------------------------------------------------------------------------------#

#f)
# Declaring crim.cv as a continous variable ranging from 0 to 1 based on the length of crim in Boston 

crim.cv <- rep(0, length(Boston$crim))

# Assigning values to crim.cv(below the median of crim will be 0 and above the median will be 1)

crim.cv[Boston$crim > median(Boston$crim)] <- 1

# Removing the crim variable from boston and making a new data frame N.Boston with crim.cv

N.Boston <- Boston[,-1]

N.Boston <- data.frame(N.Boston,crim.cv)

# Accessing the summary of the new data frame 

summary(N.Boston)

# Calculate the pearson correlation coefficient

cor(N.Boston)[,14]

library(corrplot)

corrplot::corrplot.mixed(cor(N.Boston), upper="circle")

# Logistics regression 

glm.mod <- glm(crim.cv ~., data = N.Boston, family = binomial)

# Viewing the summary of the model

summary(glm.mod)

# Confidence intervals of the coefficents

confint(glm.mod,level=.95)

# Fitting the logistics regression model based on the significant predictors

glm.mod1 <- glm(crim.cv ~zn+nox+dis+rad+tax+ptratio+black+medv, data = N.Boston, family = binomial)

# Viewing the summary of the model

summary(glm.mod1)

glm.prob <- predict(glm.mod1,N.Boston,type="response")

glm.pred <- rep(0, length(glm.prob))

glm.pred[glm.prob > 0.5] <- 1

# Building a confusion matrix

table(glm.pred, crim.cv)

# Calculating the efficency of the model

(229+223)/(229+30+24+223)

mean(glm.pred != crim.cv)

#For the logistic regression, we have a test error rate of 10.67 %.

# LDA

# Fitting the LDA model on the significant predictors to predict the categorical variable crim

lda.mod <- lda(crim.cv ~zn+nox+dis+rad+tax+ptratio+black+medv, data = N.Boston)

# Viewing the model

lda.mod

lda.pred <- predict(lda.mod,N.Boston)

# Building a confusion matrix

table(lda.pred$class,crim.cv)

(247+194)/(247+59+6+194)

mean(lda.pred$class != crim.cv)

# For the LDA Regression model we have a test error of 12.84 %.

# Applying a 50% threshold to the posterior probabilities allows us to 
# recreate the predictions contained in lda.pred.

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)


#----------------------------------------------------------------------------------------------------------#


