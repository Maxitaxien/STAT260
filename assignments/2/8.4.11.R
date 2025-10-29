library(ISLR2)
attach(Caravan)
library(gbm)

# a)
train = 1:1000

Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]

# Set Yes/No responses to be numeric (1 yes, 0 no:)
Caravan.train$Purchase = ifelse(Caravan.train$Purchase == "Yes", 1, 0)
Caravan.test$Purchase = ifelse(Caravan.test$Purchase == "Yes", 1, 0)

# b)
set.seed(1)

boost.caravan = gbm(Purchase ~., data=Caravan.train, 
distribution="bernoulli", n.trees=1000, interaction.depth=4,shrinkage=0.01)

# print(summary(boost.caravan))

# c)
# Pass in type="response" to get probabilities
yhat.boost = predict(boost.caravan, newdata=Caravan.test, n.trees=1000, type="response")

yhat.label = ifelse(yhat.boost > 0.2, 1, 0)
print(table(yhat.label, Caravan.test$Purchase))

# Fraction of people predicted to make a purchase that do make one
# Precision = (TP) / (TP + FP)
TP = sum(yhat.label == 1 & Caravan.test$Purchase == 1)
FP = sum(yhat.label == 1 & Caravan.test$Purchase == 0)
print(TP / (TP + FP)) # 0.1359649

# Comparison with KNN and logistic:
# KNN
library(class)
k = 3 

# Standardizing data for KNN (KNN algo is sensitive to scale)
# Remove column 86 - "Purchase" for X
standardized.X = scale(Caravan[, -86])
train.X = standardized.X[train, ]
test.X = standardized.X[-train, ]

train.Y = Caravan.train$Purchase
test.Y = Caravan.test$Purchase


knn.pred = knn(train.X, test.X, train.Y, k=k)
TP = sum(knn.pred == 1 & Caravan.test$Purchase == 1)
FP = sum(knn.pred == 1 & Caravan.test$Purchase == 0)
print(table(knn.pred, Caravan.test$Purchase))
print(TP / (TP + FP)) # 0.2066116

# Logistic regression
lg = glm(Purchase ~., data=Caravan.train, family=binomial)
lg.probs = predict(lg, newdata=Caravan.test, type='response')

lg.pred = ifelse(lg.probs > 0.2, 1, 0)
print(table(lg.pred, Caravan.test$Purchase))

TP = sum(lg.pred == 1 & Caravan.test$Purchase == 1)
FP = sum(lg.pred == 1 & Caravan.test$Purchase == 0)
print(TP / (TP + FP)) # 0.1359649