#121034 
library(readr)
#QUESTION 1
Dataset <- read.csv("DATASET.csv")
str(Dataset)

#i : z-score standardization
Dataset_z <- cbind(as.data.frame(scale(Dataset[c(-4,-6,-7,-8,-9)])),as.data.frame(Dataset[c(4,6,7,8,9)]))


#ii : stratified random sampling
Dataset_z$Attrition_Flag <-factor(Dataset$Attrition_Flag, levels = c("0","1")) #Attrition_Flag is the target variable, hence a factor
table(credit$Attrition_Flag) #checking proportion 

library(caret)
in_train <- createDataPartition(y = Dataset$Attrition_Flag, p = 0.70,
                                list = FALSE)
Dataset_train <- Dataset_z[in_train, ]  #70% split
Dataset_test <- Dataset_z[-in_train, ]

#iii: c50 model
library(C50) 
Dataset_model <- C5.0(Dataset_train[-2],Dataset_train$Attrition_Flag) #Target variable, attrition was left out
summary(Dataset_model) 
#Evaluating performance
#the accuracy is 97.61%. The model has performed well

#iv: improving model performance
Dataset_boost10 <- C5.0(Dataset_train[-2], Dataset_train$Attrition_Flag,trials = 10) 
summary(Dataset_boost10) 
# the accuracy has increased to 99.9%.

#v
Dataset_pred <- predict(Dataset_model, test) 
confusionMatrix(Dataset_pred, Dataset_test$Attrition_Flag, dnn = c("Prediction", "Reference")) 
#model has a kappa of 0.78 implying a good agreement between the predicted values and actual values, hence good performance. 
#specificity implies 78.66% of negative values were correctly classified
#Sensitivity implies 97.24% of positive values were correctly classified. 

#vi : 10 fold repeated Cross Validation
library(irr)
set.seed(12345)
folds <- createFolds(Dataset$Attrition_Flag, k = 10)
cv_results <- lapply(folds, function(x) {
  Dataset_train <- Dataset[-x, ]
  Dataset_test <- Dataset[x, ]
  Dataset_model <- C5.0(Attrition_Flag ~ ., data = Dataset_train)
  Dataset_pred <- predict(Dataset_model, Dataset_test)
  Dataset <- Dataset_test$Attrition_Flag
  kappa <- kappa2(data.frame(Dataset, Dataset_pred))$value
  return(kappa)})
str(cv_results)
mean(unlist(cv_results)) 




#QUESTION 2
credit <- read.csv("DATASET.csv", stringsAsFactors = TRUE)
str(credit)
attach(Dataset)
#i : Drawing hist and boxplots
hist(Customer_Age)
hist(Card_Category)
hist(Total_Relationship_Count)
hist(Months_on_book)

# box plots
boxplot(Customer_Age)
boxplot(Card_Category)
boxplot(Total_Relationship_Count)
boxplot(Months_on_book)

#ii: KNN model
 
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
credit_n <- as.data.frame(lapply(credit[c(-4,-6,-7,-8,-9)], normalize))
summary(credit_n$Customer_Age) #checking if standardization worked

#Assumption made is spiliiting 90%
credit_train <- credit_n[1:9114, ]
credit_test <- credit_n[9115:10127, ]
 
credit_train_labels <- credit[1:9114, 1]
credit_test_labels <- credit[9115:10127, 1]

library(class)
credit_test_pred <- knn(credit_train,credit_test, cl=credit_train_labels,k=100) 

library(gmodels)
CrossTable(x = credit_test_labels, y = credit_test_pred,
           prop.chisq=FALSE)

library(neuralnet) #loading a package for neural networks
attach(credit)
credit_model <- neuralnet(Attrition_Flag ~ .,
                          data = credit_train, hidden = 5) #building the neural network
plot(credit_model) #visualizing the network topology


#Evaluating performance
model_results <- compute(credit_model, credit_test[-1])
model_results
predicted_strength <- model_results$net.result
cor(predicted_attrition, credit_test$attrition) 



#QUESTION 4
cars <- read.csv("DATA.csv", stringsAsFactors = TRUE)
str(cars)
summary(cars)

#i : correlation
pairs(cars[c("price","mileage","tax","mpg","engineSize")])
library(psych)
pairs.panels(cars[c("price","mileage","tax","mpg","engineSize")])

#The LOESS CURVE for:
 #price and enginesize is upward sloping implying that price of a used car goes up with size of the engine
 #price and both mileage and miles per gallon, it is downward sloping gradually implying that as mileage and mpg increases, price decreases
     ## this shows a negative correlation between price and mpg(-0.60) and mileage(-0.54)
 #mpg and enginesize is sloping downwards indicating a weak negative correlation of (-0.37)
 #tax and enginesize is upward sloping with a strong correlation of 0.39. This implies as the size of the engine increases, the amount of road tax goes up

#ii : linear regression model
model <- lm(price~., data = cars)
model
summary(model)
#COEFFICIENTS
# as year increases by 1, price goes up by $36.53559
# as mileage increases by 1, price decreases by 1.071585
# as mile per gallon goes up by 1, price of a used car decreases by 21.2953
# as road tax increases by 1, price goes down by 7.883017
# as size of engine increases by 1, price of a used car goes up by 89.21996
# a used car with a manual transmission costs $30.77 less than an automatic
#and a car with semi-auto transmission costs $26.38 more than an automatic
# a car which uses Hybrid fuel costs $182.30 more than one that uses diesel
#while one that uses Petrol costs $67.04 less than one that uses diesel

#MODEL PERFORMANCE
#Multiple R-squared:  0.8918,	Adjusted R-squared:  0.8914
#model explains nearly 89%  of the variation in prices of used Audi cars.
# majority of predictions were between $2,097 over the true value and $1,547 under the true value.
#the maximum error of 44509 suggests that the model under-predicted price of used cars by nearly $45,000 for at least one observation.


#iii :
summary(cars$price) #mean is 22897
cars$mpg_cum <- ifelse(cars$mpg >= 22897 , 1, 0)
cars$mileage_cum<- ifelse(cars$mileage >= 22897 ,1 ,0)


model2 <- lm(price~. + mpg_cum*engineSize + year*mileage_cum, data=cars)
model2
summary(model2)
-1.852*exp(2)
#COEFFICIENTS
# as year increases by 1, price goes up by $52.64
# as mile per gallon goes up by 1, price of a used car decreases by 13.68453
# as size of engine increases by 1, price of a used car goes up by $108.86
# a used car with a manual transmission costs $30.77 less than an automatic
#and a car with semi-auto transmission costs $26.38 more than an automatic
# a car which uses Hybrid fuel costs $182.30 more than one that uses diesel
#while one that uses Petrol costs $67.04 less than one that uses diesel

#MODEL PERFORMANCE
#Multiple R-squared:  0.8973,	Adjusted R-squared:  0.8969 
# The R-squared has increased implying that now the model can predict almost 90% of the variation in prices
#the model under-predicted price by $37343  which is below the previous model
#this signifies an improvement in the model
#Hence, the consultant's advice was valuable