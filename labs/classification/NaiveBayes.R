library(ISLR2)
library(e1071)

attach(Smarket)

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]

nb.fit = naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
print(nb.fit) # shows estimated mean and stdev for each var in each class
# can also see it like this:
print(mean(Lag1[train][Direction[train] == "Down"]))
print(sd(Lag1[train][Direction[train] == "Down"]))

nb.class = predict(nb.fit, Smarket.2005)
print(table(nb.class, Direction.2005))
print(mean(nb.class == Direction.2005))
# Good performance, 59%!

# Can also generate raw probs:
nb.preds = predict(nb.fit, Smarket.2005, type = "raw")
print(nb.preds[1:5, ])