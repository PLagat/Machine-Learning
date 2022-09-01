#GROUP 1 
#121034- Lagat Patricia
#115264- Rotich Maestro
#119839- Kiobe Tracy
#122608- Gitonga Brian
#120642- Wambua ELizabeth
#123776- Olaka Brandon
#110525- Omwenga Joseph
----
library(readr)
DATASET <- read.csv("DATASET.csv")
str(DATASET)
DATASET <- DATASET[-1]
DATASET$waterfront<- factor(DATASET$waterfront, levels= c("0","1"))
DATASET$view <- factor(DATASET$view, levels=c("0","1","2","3","4"))
DATASET$condition <- factor(DATASET$condition, levels= c("1","2","3","4","5"))
DATASET$is_above_mean_price <- factor(DATASET$is_above_mean_price, levels = c("0","1"))

#QUESTION 1
#i: exploring correlation
pairs(DATASET[c("price", "bedrooms", "yr_built", "sqft_above")])
library(psych)
pairs.panels(DATASET[c("price", "bedrooms", "yr_built", "sqft_above")])

#the loess curve for;
          #price and  bedrooms is gradually sloping upwards meaning price increases with bedrooms
          #bedrooms and sqft above implies that there is more bedrooms with a middle above sqft 
          #price and sqft above is sharply sloping line implying that a large increase in price the higher the sqft_above.
#commenting on coefficients
#Bedrooms; -3.547e+04 implies that When the number of bedrooms increases by 1, the price will decrease 3.547e+04 times
#Bathrooms; 4.099e+04  when the number of bathrooms increases by 1, the price will also increase 4.099e+04 times
#sqft_living; 1.489e+02 If the number of square feet was to increase by 1, then the price would also go up 1.489e+02 times, holding all other factors constant.
#sqft_lot; 1.308e-01 When the sqft lot goes up by 1, then the price would increase 1.308e-01 times, holding all other factors constant.
#Floors; 7.044e+03 when the number of floors increases by 1, the price will also increase 7.044e+03 times, holding all other factors constant.
#grade; 9.660e+04 when the number of grades increases by 1, the price will also increase 9.660e+04 times, holding all other factors constant. 
#sqft_above;  3.177e+01 When the square feet above goes up by 1, then the price would increase  3.177e+01times, holding all other factors constant.    
#yr_built; -2.610e+03 When the year built goes up by 1, then the price would decrease 2.610e+03 times, holding all other factors constant. 
#yr_renovated;  1.986e+01 when the number of years renovated increases by 1, the price will also increase 7.973e+00 times, holding all other factors constant. 
#zipcode; -5.842e+02  When the zipcode increases up by 1, then the price would decrease 5.842e+02  times, holding all other factors constant.


#ii: training a linear model
model1 <- lm(price~., data = DATASET[-2])
model1
summary(model1) #Multiple R-squared:  0.7015,	Adjusted R-squared:  0.7012
 #the first level is left out as a reference
  # model under-predicted prices by 4,359,793 for at least one observation
  # majority of predictions were between $99,279 over the true value and $77,726 under the true value.
  #model explains nearly 70%  of the variation in prices
  #The model's Multiple R-squared is moderately high depicting that the fitted model only explains about 70% of the dependent variable.#
  #Most of the independent have a significant effect either at the 1%,5% and 10% levels of significance.


#iii: Improving model performance
 #transformation
summary(DATASET$price) #mean is 540182
sqft_living54 <- ifelse(DATASET$sqft_living >= 540182 , 1, 0)
sqft_above54 <- ifelse(DATASET$sqft_above >= 540182, 1, 0)


model2 <- lm(price ~.+bathrooms*bedrooms + sqft_above*sqft_basement, data = DATASET[-2])
summary(model2)  #Multiple R-squared:  0.7229,	Adjusted R-squared:  0.7225
  #both interaction terms are statistically significant
    #Bedrooms;  implies that When the number of bedrooms increases by 1, the price will decrease  5.225e+04 times
    #Bathrooms;  when the number of bathrooms increases by 1, the price will decrease 7.064e+03 times
    #sqft_living;  If the number of square feet was to increase by 1, then the price would decrease 7.823e+01times, holding all other factors constant.
    #sqft_lot;  When the sqft lot goes up by 1, then the price would increase  1.463e-01 times, holding all other factors constant.

# model under-predicted prices by 3,425,264 which is a reduction from the previous model of 4,359,793 for at least one observation
# majority of predictions were between $93,220 over the true value and $75,191 under the true value.
#The model's Multiple R-squared has increased to 72% implying that it explains nearly 72%  of the variation in prices
#Most of the independent have a significant effect either at the 1%,5% and 10% levels of significance.

## Thus, the consultant's advice was valuable


#QUESTION 2
#i:
set.seed(12345)
DATASET2 <- DATASET[-17, -18]
DATASET_rand <- DATASET2[order(runif(21613)),]
DATASET_train <- DATASET_rand[1:15129,]
DATASET_test <- DATASET_rand[15130:21613,]

#training model
library(C50)
DATASET_model <- C5.0(DATASET_train[-2], DATASET_train$is_above_mean_price)
DATASET_model
summary(DATASET_model)
  #the decision tree has a error rate of 0%
  #if price <= 540,000 then house is above mean price
  # if price > 540,000 then house is not above mean price

#ii:evaluating model performance
DATASET_pred <- predict(DATASET_model, DATASET_test)
library(gmodels)
CrossTable(DATASET_test$is_above_mean_price, DATASET_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual above mean price', 'predicted above mean price'))

library(caret)
confusionMatrix(DATASET_pred,DATASET_test$is_above_mean_price, positive = "1")
  #accuracy = kappa = sensitivity = specificity = 1
  # the model has performed excellently
  #the observed and actual values agree 100% of the time
  #we can infer that is_above_mean_price was correctly classified

 
#iii:
matrix_dimensions <- list(c("0", "1"), c("0", "1"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost2 <-  matrix(c(0, 1, 1.8, 0), nrow = 2,
                          dimnames = matrix_dimensions)

DATASET_cost<-C5.0(DATASET_train[-2], DATASET_train$is_above_mean_price, costs = error_cost2)
DATASET_cost #tree size is still 2

DATASET_cost_pred <- predict(DATASET_cost,DATASET_test)
CrossTable(DATASET_test$is_above_mean_price, DATASET_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

confusionMatrix(DATASET_test$is_above_mean_price,DATASET_cost_pred)
 #the model implies 100% accuracy 
 #in light of this new information, the classifier is still performing excellently



#iv: improving model performance
#BOOSTING 
library(C50)
DATASET_boost10 <- C5.0(DATASET_train[-2], DATASET_train$is_above_mean_price, trials = 10)
DATASET_boost10  
summary(DATASET_boost10) #boosting reduced to 1 trial since last classifier is very accurate
                        #classifier made no mistakes
#RESAMPLING
#k-fold cross validation, k=10
library(caret)
install.packages("irr")
library(irr)
set.seed(12345)
folds <- createFolds(DATASET$is_above_mean_price, k = 10)
cv_results <- lapply(folds, function(x) {
  DATASET_train <- DATASET[-x, ]
  DATASET_test <- DATASET[x, ]
  DATASET_model <- C5.0(is_above_mean_price ~ ., data = DATASET_train)
  DATASET_pred <- predict(DATASET_model, DATASET_test)
  DATASET_actual <- DATASET_test$is_above_mean_price
  kappa <- kappa2(data.frame(DATASET_actual, DATASET_pred))$value
  return(kappa)})
str(cv_results)
mean(unlist(cv_results)) #kappa is 0.7778563 which is a decrease from the previous model
    

#Bootstrap sampling
install.packages("boot")
library(boot)
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
boot(DATASET2, ratio,  R=1000, stype = "w")
ratio 

    #Bagging
install.packages("ipred")
library(ipred)
set.seed(300)
mybag <- bagging(is_above_mean_price ~., data= DATASET2, nbagg =25)
DATASET_pred2 <- predict(mybag, DATASET2)
table(DATASET_pred2, DATASET2$is_above_mean_price)
 #the model seems to fit the training data extremely well

#evaluating future performance
ctrl <- trainControl(method = "cv", number = 10)
train(is_above_mean_price ~ ., data = DATASET2, method = "treebag",
        trControl = ctrl)
#Bagging returns a kappa and accuracy of 1
#the model suggests that the bagged tree model performs excellently