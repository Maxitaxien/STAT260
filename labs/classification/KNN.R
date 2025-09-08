library(ISLR2)
library(class)

attach(Smarket)
train = (Year < 2005)

# knn forms preds using a single command,
# not first fitting and then predicting

# Input: 
# matrix of X train
# matrix of X test
# vector of y train
# value of K

# Use cbind() (column bind) to bind Lag1 and Lag2 into two matrices
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

# Need to set seed to break ties
set.seed(1)

# Let's predict!
knn.pred = knn(train.X, test.X, train.Direction, k=1)
print(table(knn.pred, Direction.2005))
print(mean(knn.pred == Direction.2005))
# Only 0.5 acc
# Can do the same with k = 3,
# but choice of k does not help too much on this data
# turns out QDA was just better

# Here's and example of where KNN works better:
# Also standardize data as KNN is sensitive to scale
attach(Caravan)
standardized.X = scale(Caravan[, -86])
print(var(Caravan[, 1]))
print(var(standardized.X[, 1])) # var is 1 now

test = 1:1000
train.X = standardized.X[-test, ] # yields observations not in 1->1000
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
print(mean(test.Y == knn.pred))
# 0.882 acc

print(table(knn.pred, test.Y))