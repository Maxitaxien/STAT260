library(ISLR2)
library(e1071)
set.seed(1)

x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x=x, y=as.factor(y))

# Not linearly separable
# plot(x, col=y)

# Fit with RBF
train = sample(200, 100)
svmfit = svm( y ~ ., data=dat[train, ], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train, ])

print(summary(svmfit))
# Parameters:
#    SVM-Type:  C-classification
#  SVM-Kernel:  radial
#        cost:  1

# Number of Support Vectors:  31

#  ( 16 15 )


# Number of Classes:  2

# Levels:
#  1 2

# There were quite a few training errors. Increasing cost changes this.
svmfit = svm( y ~ ., data=dat[train, ], kernel="radial", gamma=1, cost=1e5)
# plot(svmfit, dat[train, ])
# We risk overfitting though

# Cross-validation with tune()
tune.out = tune(svm, y ~ ., data=dat[train, ], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))

print(summary(tune.out))
# Parameter tuning of ‘svm’:

# - sampling method: 10-fold cross validation

# - best parameters:
#  cost gamma
#     1   0.5

# - best performance: 0.06