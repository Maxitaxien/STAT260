library(ISLR2)
attach(Wage)


# Each column is a linear combination of age, age^2, age^3, age^4
# Using raw = T argument in poly call we could obtain age, ..., age^4 directly.
fit = lm(wage ~ poly(age, 4, raw = T), data=Wage)
print(coef(summary(fit)))

# equivalent:
fit2 = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
print(coef(summary(fit2)))

# Again, equivalent:
#fit2b <- lm(wage ∼ cbind(age , age^2, age^3, age^4), data = Wage)

# create a grid of values to predict based on
# also create confidence bands of 95%
agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit, newdata=list(age=age.grid),se=TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fiut, preds$fit - 2 * preds$se.fit)

par(mfrow=c(1, 2), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)



# Note that using the raw and non-raw coefs would not affect the model in a meaningful way
# To find a suitable degree for the polynomial regression, we could use hypothesis tests.
# Here is an example of doing this using ANOVA (analysis of variance, using F-test)
# This tests the null hypothesis that model M1 is sufficient against the alternative hypothesis
# that a more complex model M2 is required.
# M1 and M2 must be nested models - predictors in M1 a subset of predictors in M2.

fits = vector("list", 5)

for (i in 1:5) {
    fits[[i]] = lm(wage ~ poly(age, i), data=Wage)
}

anova_results = do.call(anova, fits)
print(anova_results)

# However, in this case we can use the fact that poly creates orthogonal polynomials.
# The p-values for the coefs here are the same as in ANOVA
# Anova works also when we have non-orthogonal polynomials though
print(coef(summary(fits[[5]])))

# Finally, note that we could also use cross validation for selecting polynomial degree

# The book goes one to create a binary predictor with
# family = binomial, and uses some fancy function for creating CI's.
# Will not bother with that here.