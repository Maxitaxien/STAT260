library(ISLR2)
set.seed(1)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data=Auto, subset=train)
attach(Auto)
mse = mean((mpg - predict(lm.fit, Auto))[-train]^2)
print(mse) # Test mse: 23.27

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)
mse2 = mean((mpg - predict(lm.fit2, Auto))[-train]^2)
print(mse2) # Test mse: 18.71

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)
mse3 = mean((mpg - predict(lm.fit3, Auto))[-train]^2)
print(mse3) # 18.79

# Results vary with seed set.
# Little evidence of cubic being better

