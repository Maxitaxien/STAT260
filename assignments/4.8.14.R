library(ISLR2)
library(MASS)
attach(Auto)

# a)
median_mpg = median(mpg)
mpg01 = ifelse(mpg > median_mpg, 1, 0)
NewAuto = data.frame(Auto, mpg01)

# b)
# Heatmap:


correlations = cor(NewAuto[, -9])
heatmap(
    correlations,
    labRow = colnames(correlations),
    labCol = colnames(correlations)
)

dev.new()


# Boxplots

par(mfrow = c(2, 2))
boxplot(horsepower ~ mpg01, data=Auto, main = 'Horsepower vs mpg01')
boxplot(displacement ~ mpg01, data=Auto, main = 'Displacement vs mpg01')
boxplot(weight ~ mpg01, data=Auto, main = 'Weight vs mpg01')
boxplot(acceleration ~ mpg01, data=Auto, main = 'Acceleration vs mpg01'
)


# Scatterplots

dev.new()
pairs(~ horsepower + displacement + weight + acceleration + year,
data = NewAuto,
col = ifelse(mpg01 == 1, 'blue', 'red'),
)

# Findings:
# There is a clear separation in multiple categories,
# and the boxplots reveal that the means of for instance displacement
# are quite different when comparing mpg01 == 1 and mpg01 == 0.
# This indicates that displacement and weight for instance could be a useful predictors.

# c)
# Train: 80%, test: 20%
# Inspired by this example: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train_size = floor(0.8 * nrow(NewAuto))
set.seed(1)
train_ind = sample(seq_len(nrow(NewAuto)), size=train_size)

train = NewAuto[train_ind, ]
test = NewAuto[-train_ind, ]
print(nrow(train)) # 313 rows of training data
print(nrow(test)) # 79 rows of test data

# d)
lda.fit = lda(mpg01 ~ displacement + weight, data=train)

lda.pred = predict(lda.fit, test)
lda.class = lda.pred$class
print(table(lda.class, test$mpg01))
print(1 - mean(lda.class == test$mpg01))
# Results:
# No false negative, but 8 false positives.
# Test error: 0.101

# e)
qda.fit = qda(mpg01 ~ displacement + weight, data=train)
qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class
print(table(qda.class, test$mpg01))
print(1 - mean(qda.class == test$mpg01))
# Results:
# 2 false negatives, 5 false positives (one less incorrect than lda)
# Test error: 0.089

# f)
log.fit = glm(mpg01 ~ displacement + weight, data=train, family=binomial)
log.probs = predict(log.fit, test, type="response")
log.pred = ifelse(log.probs > 0.5, 1, 0)
print(table(log.pred, test$mpg01))
print(1 - mean(log.pred == test$mpg01))
# Results:
# 2 false negatives, 7 false positives
# Test error: 0.114
