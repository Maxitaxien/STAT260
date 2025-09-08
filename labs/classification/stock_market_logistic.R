library(ISLR2)

# Stock market data: Percentage returns for S&P 500
names(Smarket)
# We want to predict "up" or "down"

# print(cor(Smarket)) Gives error because Direction is qualitative
print(cor(Smarket[, -9])) # only notable correlation: Between year and Volume
# Hard prediction task!

attach(Smarket)
# plot(Volume)

# GLM: Generalized Linear Model - includes a family of regression models
# We pass in which we want - let's start with Logistic Regression
glm.fits = glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family=binomial    
)

print(summary(glm.fits))
# None were significant! Maybe Lag1 biggest indicator - previous day being negative can mean next day going up

print(coef(glm.fits))

# Let's make some predictions. When not passed data, predict() does so for training data used in model
glm.probs = predict(glm.fits, type='response')
print(glm.probs[1:10]) # we get probability values. What do they represent?
print(contrasts(Direction)) # Up is given value 1. So p > 0.5 means a prediction of Up, else Down

glm.pred = rep("Down", 1250) # create vector of initially all Down
glm.pred[glm.probs > .5] = "Up" # convert those with p > 0.5 to Up

print(table(glm.pred, Direction)) # show confusion matrix
print(mean(glm.pred == Direction)) # accuracy
# Pretty poor - we are basically guessing. And this is on the training set!

# Let's try which train/test

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
# dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fits = glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family=binomial, subset=train # automatically tells it to use train
)

glm.probs = predict(glm.fits, Smarket.2005,
    type="response"
)

glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
print(table(glm.pred, Direction.2005))
print(mean(glm.pred == Direction.2005)) # even wortse!!!  mean is 0.48

# A better strategy might be to use only the Lag1 and Lag2 features, which gives 0.582.