#STEP 1: COLLECTING THE DATA

data(mtcars)

#STEP 2: EXPLORING AND PREPARING THE DATA

#1. Use attach function to be able to use the data collected. 
attach(mtcars)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)

#2. Explore the structure of the data 
str(mtcars)

#3. Conduct summary statistics for the data
summary(mtcars)

#4. Run a plot for every predictor variable with our response variable, MPG
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
plot(mtcars$cyl,mtcars$mpg, type = "p",main = "Relationship between number of Cylinders and
miles per gallon",
     xlab = "No. of Cylinders", ylab = "Miles per Gallon")
plot(mtcars$disp,mtcars$mpg, type = "p", main = "Relationship between displacement and Miles
per Gallon",
     xlab = "Displacement", ylab = "Miles per Gallon")
plot(mtcars$hp,mtcars$mpg, type = "p", main = "Relationship between horsepower and Miles per
gallon",
     xlab = "Horsepower", ylab = "Miles per Gallon")
plot(mtcars$drat,mtcars$mpg, type = "p", main = "Relationship between the Rear Axle Ratio and
Miles per Gallon",
     xlab = "Rear Axle Ratio", ylab = "Miles per Gallon")
plot(mtcars$wt,mtcars$mpg, type = "p", main = "Relationship between Weight and Miles Per
Gallon",
     xlab = "Weight in 1000 lbs", ylab = "Miles per Gallon")
plot(mtcars$qsec,mtcars$mpg,type = "p", main = "Relationship between Quarter Mile Seconds and
Miles Per Gallon",
     xlab = "Quarter Mile (Sec)", ylab = "Miles per Gallon")
plot(mtcars$vs, mtcars$mpg, type = "p", main = "Relationship between V/Straight Engine and Miles
per Gallon",
     xlab = "V/Straight Engine", ylab = "Miles per Gallon")
plot(mtcars$am, mtcars$mpg, type = "p", main = "Relationship between Transmission type and
Miles per Gallon",
     xlab = "Transmission type", ylab = "Miles per Gallon")
plot(mtcars$gear,mtcars$mpg, type = "p", main = "Relationship between No. of Forward Gears and
Miles Per Gallon",
     xlab = "Forward Gears", ylab = "Miles per Gallon")
plot(mtcars$carb, mtcars$mpg, type = "p", main = "Relationship between No. of Carburetors and
Miles per Gallon",
     xlab = "No. of Carburetors",ylab = "Miles per Gallon")

#5. Draw a histogram of the response variable
graph11<-hist(mtcars$mpg, main="A histogram of the response variable", xlab = "mpg")
graph11

#6. Conduct a correlation analysis of the variables
cor(mtcars[c("cyl", "disp", "hp", "drat","wt","qsec","gear","carb")])

#7. Visualize the relationship between the features using a scatterplot matrix function
pairs(mtcars)

#8. Create an enhanced scatterplot matrix can be created with the pairs.panels() function
library(psych)
pairs.panels(mtcars)

#STEP 3: TRAINING THE MODEL 

#1. Fit a linear regression model on the data 
library(stats)
model1<-lm(mpg ~ ., data = mtcars)
model1

#2. Interpret the coefficients of the model. 
#For every one cylinder increase there is a corresponding 0.111 decrease on the Miles Per Gallon.
#For every one unit increase in horsepower there is a corresponding 0.0214 decrease on the Miles Per Gallon.
#For every one pound increase in Weight, there is a corresponding 3.715 decrease on the Miles Per Gallon.
#For every one carburetor increase there is a corresponding 0.199 decrease on the Miles Per Gallon.
# Our model explains 86.9% of the variation in the dependent variable. This model almost perfectly explains the variation in the dependent variable.
# Evaluating our model by its residuals depicts that the model under-predicted Miles Per Gallon by 4.6 MPG.

#STEP 4: EVALUATING MODEL PERFORMANCE 

#1. Evaluate the performance of the model by doing the summary of the model. Comment on the results. 
summary(model1)

#STEP 5: IMPROVING MODEL PERFROMANCE 

#1. Add nonlinear relationships to any 2 variables of your choosing 
mtcars$cyl2<-mtcars$cyl^2
mtcars$wt2<-mtcars$wt^2
#In a recent study (Bimarsh Pokharel, 2015) it was concluded that number of cylinders and weight are good predictors of fuel efficiency, but transmission type is not.

#2. Conduct a binary transformation to any one variable of your choosing and explain why you have conducted that transformation. 
mtcars$disp2 <- ifelse(mtcars$disp >= 100, 1, 0)
#Displacement may have minimal to no impact on miles per gallons for cars with an engine displacement of less than 100 cubic inches.
#but it may be strongly related to miles per gallon for larger engines

#3. Add an interaction effect and explain why you have chosen the variables to conduct the interactions effect on. 
mpg ~ cyl*wt
#A larger vehicle (weight) would require more cylinders
#There is an interaction between the two and hence have a combined impact on the dependent variable

#4. Train your model taking into account these 3 changes in points 1 to 3. 
model2 <- lm(mpg ~ . + cyl2 + wt2 + disp2 + cyl*wt, data = mtcars)

#5. Evaluate the performance of the model by doing the summary of the model. Comment on the results. 
summary(model2)
#Relative to our first model, the R-squared value has improved from 0.87 to about 0.94.
#Similarly, the adjusted R-squared value, which takes into account the fact that the
#model grew in complexity, improved from 0.81 to 0.89.
