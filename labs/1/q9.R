library(ISLR2)
attach(Auto)

# a)
#pairs(Auto)

# b)
auto_subset = Auto[ , !names(Auto) %in% c("name")]
auto_cor = cor(auto_subset)
print('Covariance matrix:')
print(auto_cor)

# c)
lm.fit = lm(mpg ~ . - name, data=Auto)
print('Multiple linear regression summary: ')
print(summary(lm.fit))

# There was a relationship - many variables ere significant.
# displacement, weight, year, origin was significant.
# The coefficient for year suggest that increasing year increasing mpg,
# which makes sense as newer cars have higher mpg values.

# d)
par(mfrow = c(2, 2))
# plot(lm.fit)
par(mfrow = c(1, 1))
# A few points stand out - perhaps 327 which has a high residual
# and high on Q-Q residual.
# Also point 14 has abnormally high leverage,
# but doesn't appear to be an outlier

# e)
lm.fit2 = lm(mpg ~ . - name + horsepower:year + horsepower:weight, data=Auto)
# print(summary(lm.fit2))
# adding interaction terms increased quality of fit, and are statistically significant
# probably because there is some relationship between year and horsepower,
# and between horsepower and weight

# f)
# Log-transform:
log_auto = log(auto_subset) # everything except name
lm.fit_log = lm(mpg ~ ., data=log_auto)
print(summary(lm.fit_log))
# Seemingly better, but RSE cannot be compared
# It is much lower on log scale.
# However, R-squared is lower, meaning we explain more variance relatively

# Check diagnostics
# par(mfrow = c(2, 2))
# plot(lm.fit_log)
# par(mfrow = c(1, 1))

# Square-root transform
sqrt_auto = sqrt(auto_subset)
lm.fit_sqrt = lm(mpg ~ ., data=sqrt_auto)
print(summary(lm.fit_sqrt))
# Similar to log transform - RSE looks lower

# Square transform
# We expect higher errors here
squared_auto = auto_subset^2
lm.fit_squared = lm(mpg ~ ., data=squared_auto)
print(summary(lm.fit_squared))
# As expected, here the RSE is on a much larger square.
# R-squared is a bit lower (probably instability due to high values)
