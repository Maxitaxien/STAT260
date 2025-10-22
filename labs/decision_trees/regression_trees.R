library(tree)
library(ISLR2)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston = tree(medv ~ ., Boston, subset=train)
print(summary(tree.boston))
# summary shows that only four of the variables were used in constructing the tree

plot(tree.boston)
text(tree.boston, pretty=0)
# amount of rooms important for price, in conjunction with lstat (percentage of individuals with lower socioeconomic status)

# Note: Could have fit a larger tree by passing a control into the tree() func, such as:
# control = tree.control(nobs=length(train), mindev=0)

# Now use cv
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
# most complex tree is selected by cv.

# however, can still prune:
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# get predictions with unpruned tree (it was best from cv-results)
yhat = predict(tree.boston , newdata = Boston[-train , ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
print(mean((yhat - boston.test)^2))
# test MSE: 35.29.
# sqrt of this is around 5.941, so model leads to test predicitons within
# approximately 5.941$ of the true median home value for the census tract.