print(head(Carseats))

# R generates dummy variables automatically
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
print(summary(lm.fit))
print(contrasts(Carseats$ShelveLoc)) # shows dummy variable coding