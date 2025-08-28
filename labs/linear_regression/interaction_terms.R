print(summary(lm(medv ~ lstat * age, data = Boston)))

# including lstat * age makes R automatically include interaction term
# between lstat and age - this includes lstat + age + lstat:age