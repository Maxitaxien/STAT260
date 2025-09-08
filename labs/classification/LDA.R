library(MASS)
library(ISLR2)

attach(Smarket)

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]

lda.fit = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
print(lda.fit)
# plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
print(names(lda.pred)) # display attributes -
# class: predictions, posterior: posterior probabilities that observation belongs to kth class,
# x: linear discriminants
lda.class = lda.pred$class
print(table(lda.class, Direction.2005))
print(mean(lda.class == Direction.2005)) # 0.56

# Recreate predictions:
print(sum(lda.pred$posterior[, 1] >= .5))
print(sum(lda.pred$posterior[, 1] < .5))
# Posterior probs corresponds to probability of Down
