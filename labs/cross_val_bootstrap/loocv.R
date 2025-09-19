# Leave One Out Cross-Validation
library(ISLR2)
library(boot) # bootstrap lib

glm.fit = glm(mpg ~ horsepower, data=Auto)
print(coef(glm.fit))
# The point: glm fits a linear model if we do not pass in model

# LOOCV
cv.err = cv.glm(Auto, glm.fit)
print(cv.err$delta) # correspond to LOOCV statistic
# cross-validation estimate for test error approx 24.23


# Testing multiple fits with a for loop
cv.error = rep(0, 10)
for (i in 1:10) {
    glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
    cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
print(cv.error)
# sharp drop in error, but then diminishing returns.