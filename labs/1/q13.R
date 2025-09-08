library(ISLR2)

attach(Weekly)

# a)
print(head(Weekly))
print(cor(Weekly[, -9])) # higher year to volume cor from 1990 to 2010
print(summary(Weekly))
plot(Weekly$Year, Weekly$Volume)

# b)
glm.fit = glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data=Weekly, family=binomial
)

print(summary(glm.fit))
# Only Lag2 appears to be statistically significant, Lag1 is close to being sig

# c)
glm.probs = predict(glm.fit, type='response')

glm.pred = rep("Down", 1089) # create vector of initially all Down
glm.pred[glm.probs > .5] = "Up" # convert those with p > 0.5 to Up

print(table(glm.pred, Direction))

# Shows that the model predicts Up most of the time.

# d) 
train = (Year < 2009)
