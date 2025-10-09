library(ISLR2)
library(gam)
attach(Wage)

# Fit the GAM using natural splines
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data=Wage)

# print(summary(gam1))

# Fit the GAM using smoothing splines, from the gam library (s())
# We can specify DOF for each var (4 for year, 5 for age here)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data=Wage)

# Display results
par(mfrow=c(1, 3))
plot(gam.m3, se=TRUE, col="blue", pages=1) # plot invokes plot.Gam() for a special plot

# Conveniently we can still use plot with lm objects because they included natural splines
plot(gam1, se=TRUE, col="red")

# Function of year looks linear. Do ANOA to check if excluding year, using a linear func or it or a spline function is best
gam.m1 = gam(wage ~ s(age, 5) + education, data=Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data=Wage)
# gam.m3 is already defined.

analysis = anova(gam.m1, gam.m2, gam.m3, test="F")
print(analysis)
#
#Model 1: wage ~ s(age, 5) + education
#Model 2: wage ~ year + s(age, 5) + education
#Model 3: wage ~ s(year, 4) + s(age, 5) + education
#  Resid. Df Resid. Dev Df Deviance       F    Pr(>F)
#1      2990    3711731
#2      2989    3693842  1  17889.2 14.4771 0.0001447 ***
#3      2986    3689770  3   4071.1  1.0982 0.3485661

# Seems like including linear year is better, but no evidence that using splines is better

# M2 is preferred.

# We can also produce summaries of gam fits:
print(summary(gam.m2))

# Making predictions:
preds = predict(gam.m2, newdata=Wage) # overfitting deluxe but nevermind

# Using local regression fits as building blocks in a GAM using lo()
gam.lo = gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education, data=Wage)
plot(gam.lo, se=TRUE, col="green")

# Can create interactions as well:
gam.lo.1 = gam(wage ~ lo(year, age, span=0.5) + education, data=Wage)
# first term is interaction between year and age, last is educatio

# plot resulting 2d surface with akima package.
library(akima)
par(mfrow=c(1, 2))
plot(gam.lo.1)


# Finally: Fitting a logistic regression GAM:
# Using I() to turn wage into binary variable
gam.lr = gam(I(wage > 250) ~ year + s(age, df=5) + education, family=binomial, data=Wage)
par(mfrow=c(1, 3))
plot(gam.lr, se=TRUE, col="green")

# There are no high earners in the < HS category
print(table(education, I(wage > 250)))

# let's just fit a model without them:
gam.lr.s = gam(I(wage > 250) ~ year + s(age, df=5) + education, family=binomial, data=Wage, subset= (education != "1. < HS Grad"))
plot(gam.lr.s, se=TRUE, col="green")