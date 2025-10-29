library(e1071)
set.seed(1)

x = matrix(rnorm(20*2), ncol=2)

y = c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 1
plot(x, col = (3 - y))
# Not linearly separable.

dat = data.frame(x=x, y=as.factor(y))

# The cost argument gives cost of violation of margin
# Low cost: Wide margins, many points on margin or violating
svmfit = svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
# scale = FALSE tells svm not to scale each feature, scaling is often useful.

# plot(svmfit, dat)
# print(svmfit$index) #  1  2  5  7 14 16 17 index of support vectors

print(summary(svmfit))
# Parameters:
#    SVM-Type:  C-classification
#  SVM-Kernel:  linear
#        cost:  10

# Number of Support Vectors:  7

#  ( 4 3 )
# (four in one class, 3 in other class)

# Number of Classes:  2

# Levels:
#  -1 1

# Trying a smaller value:
svmfit = svm(y ~ ., data=dat, kernel="linear", cost=0.1, scale=FALSE)
# plot(svmfit, dat)
# print(svmfit$index) # 1  2  3  4  5  7  9 10 12 13 14 15 16 17 18 20
# many more support vectors

# We can use built-in function tune() for cross-validation
set.seed(1)

tune.out = tune(svm, y ~ ., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# print(summary(tune.out))
# 1 1e-03  0.55  0.4377975
# 2 1e-02  0.55  0.4377975
# 3 1e-01  0.05  0.1581139
# 4 1e+00  0.15  0.2415229
# 5 5e+00  0.15  0.2415229
# 6 1e+01  0.15  0.2415229
# 7 1e+02  0.15  0.2415229
# We see that c = 1e-01 (0.1) gives the lowest cv error rate

bestmod = tune.out$best.model
# print(summary(bestmod))

# PREDICTION
# Generate test data
xtest = matrix(rnorm(20 * 2), ncol=2)
ytest = sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest == 1] = xtest[ytest == 1, ] + 1
testdat = data.frame(x=xtest, y=as.factor(ytest))

ypred = predict(bestmod, testdat)
print(table(predict=ypred, truth=testdat$y))
# 17 / 20 correct classifications.

# ALREADY LINEARLY SEPARABLE
x[y == 1, ] = x[y == 1, ] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y ~ ., data=dat, kernel="linear", cost=1e5)
s = summary(svmfit)

plot(svmfit, dat)