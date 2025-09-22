library('ISLR2')
attach(Auto)

# a)
median_mpg = median(mpg)
mpg01 = ifelse(mpg > median_mpg, 1, 0)
NewAuto = data.frame(Auto, mpg01)

# b)
# Plots are commented out to make re-runs faster
# Heatmap:
"
correlations = cor(NewAuto[, -9])
heatmap(
    correlations,
    labRow = colnames(correlations),
    labCol = colnames(correlations)
)

dev.new()
"

# Boxplots
"
par(mfrow = c(2, 2))
boxplot(horsepower ~ mpg01, data=Auto, main = 'Horsepower vs mpg01')
boxplot(displacement ~ mpg01, data=Auto, main = 'Displacement vs mpg01')
boxplot(weight ~ mpg01, data=Auto, main = 'Weight vs mpg01')
boxplot(acceleration ~ mpg01, data=Auto, main = 'Acceleration vs mpg01'
)
"

# Scatterplots
"
dev.new()
pairs(~ horsepower + displacement + weight + acceleration + year,
data = NewAuto,
col = ifelse(mpg01 == 1, 'blue', 'red'),
)
"

# Findings:
# There is a clear separation in multiple categories,
# and the boxplots reveal that the means of for instance displacement
# are quite different when comparing mpg01 == 1 and mpg01 == 0.
# This indicates that displacement for instance could be a useful predictors.

print(NewAuto[, "year"])