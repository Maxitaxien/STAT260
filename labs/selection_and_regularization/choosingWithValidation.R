library(ISLR2)
library(leaps)

set.seed(1)

Hitters = na.omit(Hitters)

# VALIDATION SET
train = sample(c(TRUE, FALSE), nrow(Hitters), replace=TRUE)
test = !train
regfit.best = regsubsets(Salary ~ ., data=Hitters[train, ], nvmax=19)

test.mat = model.matrix(Salary ~ ., data=Hitters[test, ])
val.errors = rep(NA, 19)

for (i in 1:19) {
    coefi = coef(regfit.best, id=i)
    pred = test.mat[, names(coefi), drop=FALSE] %*% coefi
    val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}
print(val.errors)

best = which.min(val.errors)
print(best) # Best with 7 variables

# Generalizable func
predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id=id)
    xvars = names(coefi)
    mat[, xvars] %*% coefi
}

# Note: After we have found the best model, we just train a new one on the whole dataset
# Why? Idk, I guess we just wanted to find the best amount of variables.
regfit.best = regsubsets(Salary ~., data=Hitters, nvmax = 19)
print(coef(regfit.best, 7))

# CROSS VALIDATION
# First, create vector that allocates each observation to one of k folds
k = 10 
n = nrow(Hitters)
set.seed(1)
folds = sample(rep(1:k, length=n))
cv.errors = matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

# Then, train and call the predict function we made for the missing fold
for (j in 1:k) {
    best.fit = regsubsets(Salary ~ ., data=Hitters[folds != j, ], nvmax=19)
    for (i in 1:19) {
        pred = predict(best.fit, Hitters[folds == j, ], id = i)
        cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
    }
}

mean.cv.errors = apply(cv.errors, 2, mean)
print(mean.cv.errors)

par(mfrow = c(1, 1))
plot(mean.cv.errors, type="b")

# CV selected a 10-variable model
# Perform best-subset-selection on full data set to get 10-var model
reg.best = regsubsets(Salary ~ ., data=Hitters, nvmax=19)
print(coef(reg.best, 10))
