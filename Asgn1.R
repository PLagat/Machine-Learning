data("mtcars")
View(mtcars)
attach(mtcars)
str(mtcars)
summary(mtcars)
factor(mtcars$carb)

plot(y=mpg, x=cyl)
plot(y=mpg, x=disp)
plot(y=mpg, x=hp)
plot(y=mpg,x=drat)
plot(y=mpg,x=wt)
plot(y=mpg, x=qsec)
plot(y=mpg, x=vs)
plot(y=mpg, x=am)
plot(y=mpg, x=gear)
plot(y=mpg, x=carb)

hist(mtcars$mpg) #dependent variable
 
hist(mtcars$cyl)
hist(mtcars$disp)
hist(mtcars$hp)
hist(mtcars$drat)
hist(mtcars$wt)
hist(mtcars$qsec)
hist(mtcars$vs)

#correlation matrix
cor(mtcars[c("mpg", "cyl", "disp", "hp", "drat","wt","qsec","vs","am", "gear", "carb")])
pairs(mtcars[c("mpg", "cyl", "disp", "hp", "drat","wt","qsec","vs","am", "gear", "carb")])

install.packages("psych")
library(psych)
pairs.panels(mtcars[c("mpg", "cyl", "disp", "hp", "drat","wt","qsec","vs","am", "gear", "carb")])


#STEP 3: TRAINING THE MODEL
model1 <- lm(mpg~cyl + disp + hp +drat + wt + qsec + vs + am + gear + carb)
model1 <-lm(mtcars$mpg~.,data = mtcars)
predmodel <- predict(model1,testmodel)

#STEP 4: EVALUATING MODEL PERFORMANCE
summary(model1)

#STEP 5:IMPROVING MODEL PERFORMANCE
cyl2 <- mtcars$cyl^2

model2 <- lm(mpg~cyl +cyl2 + disp + hp +drat + wt + qsec + vs + am + gear + carb)
summary(model2)
# our R-squared has increased from 0.869 to 0.8816
# the model is now explaining 88% of the variation
