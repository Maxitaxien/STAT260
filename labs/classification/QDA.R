library(ISLR2)
library(MASS)

attach(Smarket)

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]

qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket,
    subset = train)
print(qda.fit)

qda.class = predict(qda.fit, Smarket.2005)$class
print(table(qda.class, Direction.2005))
print(mean(qda.class == Direction.2005))
# Impressive! 0.6 accuracy

