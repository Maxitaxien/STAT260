library(glmnet)
library(ISLR2)

Hitters = na.omit(Hitters)

# model .matrix produces data matrix and automatically transforms qualitative vars into dummy vars
x = model.matrix(Salary ~., Hitters)[, -1]
y = Hitters$Salary

grid = 10^seq(10, -2, length=100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid) # pass inn this range of lambda values

print(dim(coef(ridge.mod))) # each value of lambda has a vector of coefficients
print(coef(ridge.mod)[, 50]) # when lambda is larger, coefficient values should be smaller

print(predict(ridge.mod, s=50, type="coefficients")[1:20, ]) # get coefficients for lambda = 50

# let's estimate the test error of ridge and lasso
# using lambda = 4
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx = x[test, ])
print(mean((ridge.pred - y.test)^2))

# Compare with standard least squares (equivalent to lambda = 0):
# passing in exact to not interpolate over grid of lambda values
ridge.pred = predict(ridge.mod, s=0, newx=x[test, ], exact = TRUE, x=x[train, ], y=y[train])
print(mean((ridge.pred - y.test)^2))
# Here we get a worse error. But using a standard lm() would give more useful statistics,
# p-values and standard errors.

# Instead of arbitrarily choosing lambda = 4, we can do cv:
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlambda = cv.out$lambda.min
print(bestlambda) # best lambda: approx 326

ridge.pred = predict(ridge.mod, s=bestlambda, newx=x[test, ])
print(mean((ridge.pred - y.test)^2)) # 139856.6 

# finally, refit this model on the full data set
out = glmnet(x, y, alpha=0)
print(predict(out, type="coefficients", s=bestlambda)[1:20, ])
# coefficients quite small, but none are zero!