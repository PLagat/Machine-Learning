#splitting using Standard Deviation Reduction (SDR)
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) +
                        length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) +
                        length(bt2) / length(tee) * sd(bt2))
sdr_a
sdr_b #Since the standard deviation was reduced more for the split on B,
            #the decision tree would use B first.


#EXAMPLE- WHITEWINES
library(readr)
wine <- read.csv("whitewines.csv")
str(wine)
#do not need to normalize or standardize the features

hist(wine$quality)
summary(wine)

#dividing dataset
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

#Eval model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)
  
  #using Mean Absolute Error (MAE)
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
  }
MAE(p.rpart, wine_test$quality)

mean(wine_train$quality)
#If we predicted the value 5.87 for every wine sample, we would have a mean
  #absolute error of only about 0.67:
MAE(5.87, wine_test$quality)

#Improving model performance
install.packages("Cubist")
library(Cubist)
m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
m.cubist
summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)
cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)



install.packages("RWeka")
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)