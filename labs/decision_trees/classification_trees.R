library(tree)
library(ISLR2)


# attach(Carseats) 

# create binary variable based on midpoint of sales
Carseats$High = factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))


# fit tree, similar to lm
tree.carseats = tree(High ~ . - Sales, data = Carseats) # make sure not to use continuous sales column!

print(summary(tree.carseats))
# Training error: 9%.
# Small residual mean deviance - similar to entropy, indicates good fit to training data

# Trees can be graphically displayed!
plot(tree.carseats)
text(tree.carseats, pretty=0) # to display labels
# plot only works when running through vscode for some reason

# results:
# most important indicator of sales is shelving loc, good distinguished from bad, medium


# let's estimate the test error
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]

tree.carseats = tree(High ~ . - Sales, data = Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
print(table(tree.pred, Carseats.test$High)) # acc ca. 0.77


# PRUNING 
set.seed(7)
# cv performs cross-validation to find optimal level of tree complexity
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
# Output from printing cv.carseats:
# $size
# [1] 21 19 14  9  8  5  3  2  1
#
# $dev
# [1] 75 75 75 74 82 83 83 85 82

# $k
# [1] -Inf  0.0  1.0  1.4  2.0  3.0  4.0  9.0 18.0

# $method
# [1] "misclass"

# attr(,"class")
# [1] "prune"         "tree.sequence"

# where k-values are alpha values
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# use prune.misclass() function to prude tree and get nine-node tree
prune.carseats = prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# Do we get better test accuracy?
tree.pred = predict(prune.carseats, Carseats.test, type="class")
print(table(tree.pred, Carseats.test$High))

# acc slighty improved with 0.775 instead of 0.77.
# but more importantly, we have a simpler and more interpretable tree!

