library(ISLR2)
library(boot)

attach(Auto)
set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
    glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
    cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

print(cv.error.10) # shorter computation time
# this is because LOOCV does not use the magic formula for ISLR2
# note that k-fold gives a similar result to LOOCV
# when using k-fold, the two numbers associated with delta are a little different

# first: standard k-fold estimate
# second: bias-corrected version
