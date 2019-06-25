if (!require(ISLR)) install.packages("ISLR")
library(ISLR)


set.seed (1)
# we need to randomly select 196 units from the original data set so first create an index 
train <- sample(392,196)  # creates an index of the observations to be 
# used for the training set
# We now run a linear regression using the training data.
lm.fit <- lm(mpg~horsepower , data = Auto, subset=train)

# use predict() to estimate the response for all 392 observations
# use mean() to calculate MSE for the observations in the validation set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
## [1] 26.14142
lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
## [1] 19.82259
summary(lm(mpg~poly(horsepower ,2),data=Auto, subset=train))$coef
## [1] 19.82259
lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto,
           subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
## [1] 19.78252
summary(lm(mpg~poly(horsepower ,3),data=Auto, subset=train))$coef

#LOOCV 
# run a linear model using glm function in R
# the boot package has cross validation functions
# the cv.glm does cross validation and 
# the default sets K equal to the number of observations in data 
# which gives the  leave-one-out cross-validation (LOOCV)

if (!require(boot)) install.packages("boot")
library(boot)

glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
## [1] 24.23151  24.23114

# We can repeat this procedure for increasingly complex polynomial fits.
#look at polynomial fits up to order 5
#
cv.error=rep(0,5)  # creat cv.error as an vector of 5 values all of which are initialized to 0
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
## [1] 24.23151 19.24821 19.33498 19.42443 19.03321
# We see a sharp drop in the estimated test MSE between the linear
# and quadratic fits, but then no clear improvement from using
# higher-order polynomials.


# k-fold cross-Validation with K=10

set.seed(17)
#
# look at polynomials of order up to 10
#
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
## [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 ## [8] 19.71201 18.95140 19.50196



