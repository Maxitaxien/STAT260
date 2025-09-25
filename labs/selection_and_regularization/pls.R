library(ISLR2)
library(pls)

Hitters = na.omit(Hitters)
x = model.matrix(Salary ~., Hitters)[, -1]
y = Hitters$Salary

set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

# do partial-least square
# syntax is same as PCR
pls.fit = plsr(Salary ~., data=Hitters, subset=train, scale=TRUE, validation="CV")
print(summary(pls.fit))

validationplot(pls.fit, val.type="MSEP") # Here only M=1 component gave the best!

pls.pred = predict(pls.fit, x[test, ], ncomp=1)
print(mean((pls.pred - y.test)^2))
# worse than the other methods

# with full dataset:
pls.fit = plsr(Salary ~ ., data=Hitters, scale=TRUE, ncomp=1)
print(summary(pls.fit))