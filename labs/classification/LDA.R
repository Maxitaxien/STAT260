library(MASS)
library(ISLR2)

attach(Smarket)

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]

lda.fit = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
print(lda.fit)
plot(lda.fit)