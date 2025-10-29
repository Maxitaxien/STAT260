library(leaps)

# a)
n = 1000
p = 20

# x matrix (1000 * 20):
set.seed(1)
x = matrix(rnorm(n * p), nrow=n, ncol=p)

# beta coefficients (20 * 1), setting some to zero
# first sampling, then setting about half to zero
beta = rnorm(p, 0, 1)
beta[c(1, 3, 6, 8, 11, 13, 16)] = 0

# random noise (st.dev. 0.25)
eps = rnorm(n, 0, 0.25)

y = x %*% beta + eps

data = data.frame(x, y)

# b)
train_size = 100
set.seed(1)
train = sample(1:n, train_size)
test = (-train)
y.test = y[test]

# c)
# Performing best subset selection using regsubsets
regfit.full = regsubsets(y ~ ., data=data[train, ], nvmax=p)
reg.summary = summary(regfit.full)
mse = reg.summary$rss / train_size

plot(mse, xlab="p", ylab="MSE", type="l")
# Results: 
# Training MSE decreases monotonically as the number of features
# included increases, until we have included all the features that do not have a zero-coefficient from beta. 
# From this point, the MSE stays thr same.
# This makes sense as we generated the distribution of data from a calculation involving x - 
# therefore, all x's with a non-zero beta coefficient has some significance in explaining y.
# However all the x's with a zero beta coefficient have no relationship with y, therefore
# not improving training MSE.

# d)
# The following function is from the Statistical Learning Example section, p. 273
predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id=id)
    xvars = names(coefi)
    mat[, xvars] %*% coefi
}

test_mses = rep(0, times=p)
for (i in 1:p) {
    pred = predict.regsubsets(regfit.full, newdata=data[test, ], id=i)
    test_mses[i] = mean((y.test - pred)^2)
}

dev.new()
plot(test_mses, xlab="p", ylab="Test MSE", type="l")
# Results: 
# We see a similar effect as that which is seen on the training data, although with larger MSE values
# This is consistent with the training MSE often overestimating the test MSE
# As all x's with non-zero coefficients are correlated with y,
# including them does help our fit on the test data as well.