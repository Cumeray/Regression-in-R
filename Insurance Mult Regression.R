# Insurance data Multiple Regression

#Read the dataset:
dataset=read.csv('insurance.csv')
#Exploration of dataset

typeof(dataset) #type
head(dataset) #look at first rows of dataset
ncol(dataset) # Columns of dataset
nrow(dataset) # Rows in dataset
colnames(dataset) #column names in dataset

summary(dataset) #Summary of the dataset df

str(dataset) # Description of data frame by type of data

#Summarizing categorical variables
table(dataset$sex)
table(dataset$smoker)
table(dataset$region)

#Summary of integers

hist(dataset$age)
hist(dataset$bmi)
hist(dataset$children)
hist(dataset$charges)

#Scatter Plot Matrix of Numeric Variables
library(GGally)
ggpairs(dataset[,c("age","bmi","children","charges")])

hist(dataset$charges) #Histogram of Y (Target)

# Encoding categorical data using 'factor" function
dataset$sex=factor(dataset$sex, 
                     levels=c('female', 'male'),
                     labels=c(1,0)) #Note: a factor in R is NOT a numeric value
dataset$smoker=factor(dataset$smoker, 
                   levels=c('yes', 'no'),
                   labels=c(1,0)) 
dataset$region=factor(dataset$region, 
                   levels=c('northeast', 'northwest','southeast','southwest'),
                   labels=c(1,2,3,4))

# #Creating Dummy variables
# library(fastDummies)
# dataset<-dummy_cols(dataset, remove_first_dummy = TRUE)
# dataWdummies<-dataset
#Drop categorical variables because we have dummy variables in them
#dataset<-subset(dataset, select=-c(sex,smoker,region))

#Split data from vector Y into two sets in predefined ratio 
# while preserving relative ratios of different labels in Y:split function

# Splitting data into Training and Test sets (install.packages('caTools')
library(caTools)
set.seed(44) #Setting the seed for random split
split=sample.split(dataset$charges, SplitRatio=0.8) #sample.split {caTools}. 
#cont..Also, split uses Split Ratio as fraction on TRAINING set
training_set=subset(dataset, split==TRUE)
test_set=subset(dataset, split==FALSE)

#Fitting model into Training dataset
# Note: the regression model takes care of the Dummy trap 
# by eliminating one of the dummy columns in each case
library(car)
regressor<-lm(formula=charges~., data<-training_set) 
summary(regressor)
vif(regressor)
par(mfrow=c(2, 2))
plot(regressor)
par(mfrow=c(1, 1))
durbinWatsonTest(regressor)

# Running stepwise and best subset methods
#install.packages("olsrr")
library(olsrr)

ols_step_both_p(regressor, pent = 0.1, prem = 0.1, details = FALSE)
ols_step_both_aic(regressor, details = FALSE)
ols_step_all_possible(regressor) #Best subsets method

#Running best model after stepwise and Best Subsety:
regressor<-lm(formula=charges~age+bmi+children+smoker, data<-training_set) 
summary(regressor)
vif(regressor)
par(mfrow=c(2, 2))
plot(regressor)
par(mfrow=c(1, 1))
durbinWatsonTest(regressor)

regressor_test<-lm(formula=charges~age+bmi+children+smoker, data<-test_set)
summary(regressor_test)
#Running model on test data
library(ggpubr)
ggscatter(x = "prediction",
          y = "actual",
          data = data.frame(prediction = predict(regressor_test),
                            actual = test_set$charges)) +
  geom_abline(intercept = 0,
              slope = 1) 
library(Metrics)
rmse(predict(regressor_test),test_set$charges)

y_hat<-predict(regressor_test, interval="prediction")
test_set<-cbind(y_hat, test_set)

# ggplot(test_set, aes(order, charges))+
#   geom_point(colour="blue", shape=19) +
#   #geom_line(aes(y=fit), color = "green", size=1)+
#   geom_point(aes(y=fit), color = "cyan", shape=19)
#   #geom_errorbar(aes(y=fit, ymin=lwr, ymax=upr), color="red")

# ggplot(test_set, aes(order, charges))+
#   geom_errorbar(aes(y=fit, ymin=lwr, ymax=upr), color="cyan")+
#   geom_point(aes(y=fit), color = "cyan", shape=19)+
#   geom_point(colour="blue", shape=19)
 