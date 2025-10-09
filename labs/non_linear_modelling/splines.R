library(ISLR2)
library(splines)

attach(Wage)

agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])

# we pre-specify knots at ages 25, 40, 60
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data=Wage)
pred = predict(fit, newdata=list(age=age.grid), se=T)

plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2, col="yellow")
lines(age.grid, pred$fit + 2 * pred$se, lty="dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty="dashed")

# we can use the df option to produce knots at uniform quantiles of the data
# dim(bs(age, df=6)) dim = 3000 x 6
print(attr(bs(age, df=6), "knots")) # [1] 33.75 42.00 51.00 (25%, 50%,  75%)


# Fitting a natural spline instead, using ns():
fit2 = lm(wage ~ ns(age, df=4), data=Wage)
pred2 = predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

# Fitting a smoothing spline:
fit3 = smooth.spline(age, wage, df=16) # func automatically determines lambda which yields 16 DOF
fit4 = smooth.spline(age, wage, cv=TRUE)

# NOTES ON LOCAL REGRESSION:
# This is done with the loess() function. See book, p. 318
# Can also use locfit library for this.

 