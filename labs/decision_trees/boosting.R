library(gbm)
library(ISLR2)

# use distribution = "gaussian" for regression,
# distribution = "bernoulli" for binary classification

set.seed(1)
attach(Boston)

train = sample(1:nrow(Boston), nrow(Boston) / 2)
boston.test = Boston[-train, "medv"]

boost.boston = gbm(medv ~ ., data=Boston[train, ], distribution="gaussian",
n.trees=5000, interaction.depth=4)

# print(summary(boost.boston))
# Gives importances and makes plot of relative influence statistics

# Basically: rm was most important followed by lstat
# For some reason partial dependence plots didn't work:
# dev.new()
# plot(boost.boston, i="rm")
# plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
print(mean((yhat.boost - boston.test)^2))
# Gives 17.9442 test MSE (superior to random forests and bagging)
# we can also specify the value we want for shrinkage parameter lambda (default 0.001)

boost.boston = gbm(medv ~ ., data=Boston[train, ], distribution="bernoulli",
n.trees=5000, interaction.depth=4, shrinkage=0.01, verbose=F)
yhat.boost = predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
print(mean((yhat.boost - boston.test)^2))
# Here this decreased test MSE to 16.81507

