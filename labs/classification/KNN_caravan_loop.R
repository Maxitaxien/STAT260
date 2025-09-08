library(ISLR2)
library(class)
library(glue)
attach(Caravan)

standardized.X = scale(Caravan[, -86])
test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

ks = list(1, 3, 5, 10, 15)
set.seed(1)

for (k in ks) {
    knn.pred = knn(train.X, test.X, train.Y, k=k)
    print(glue("Results for k = {k}:"))
    print(mean(test.Y == knn.pred))
}