library(pls)
library(ISLR2)

Hitters = na.omit(Hitters)

# Principal component with cross validation (default ten-fold)
pcr.fit = pcr(Salary ~., data=Hitters, scale=TRUE, validation="CV")

print(summary(pcr.fit)) # shows how much variance is explained by each principal component
validationplot(pcr.fit, val.type="MSEP")


x = model.matrix(Salary ~., Hitters)[, -1]
y = Hitters$Salary

set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]
pcr.fit = pcr(Salary ~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP") # now we see that lowest error occurs when M=5 components used

pcr.pred = predict(pcr.fit, x[test, ], ncomp=5)
print(mean((pcr.pred - y.test)^2)) # 142812
# competitive with ridge and lasso-results
# but final model is more difficult to represent - we have transformed the data

# Fit on whole dataset
pcr.fit = pcr(y ~ x, scale=TRUE, ncomp=5)
print(summary(pcr.fit))
