library(ISLR2)
library(car)

lm.fit = lm(medv ~ lstat + age, data=Boston)
print(summary(lm.fit))

# All predictors:
lm.fit = lm(medv ~ ., data=Boston)
print(summary(lm.fit))
print(summary(lm.fit)$r.sq) # show specifically R^2 value, for instance
print(vif(lm.fit)) # display variance inflation factors