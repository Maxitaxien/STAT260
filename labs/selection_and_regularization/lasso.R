library(ISLR2)
library(glmnet)

Hitters = na.omit(Hitters)

x = model.matrix(Salary ~., Hitters)[, -1]
y = Hitters$Salary

set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

grid = 10^seq(10, -2, length=100)

lasso.mod = glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod) # some coefficients will be zero depending on tuning parameter

# let's do some CV
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlambda = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlambda, newx=x[test,  ])
print(mean((lasso.pred - y.test)^2))
# Much better than least squares, ismilar to ridge performance
# But we have the advantage of 0-coefficients:
out = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlambda)[1:20, ]
print(lasso.coef) # lots of the coefficients are now zero (although there are some much larger ones)