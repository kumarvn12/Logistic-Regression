#objective - to predict survival

# Step 1: Load + Clean Data
Train = read.csv("Titanic_train.csv")
Test = read.csv("Titanic_test1.csv")

str(Train)
summary(Train)


summary(Train$Age)

# fill in missing values for Age
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)


# Step 2: Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Embarked","Cabin")
Train = Train[,!(names(Train) %in% nonvars)]
str(Train)


Test = Test[,!(names(Test) %in% nonvars)]
str(Test)


#Step 3: Check for MultiCollinearity
Train$Sex = as.numeric(Train$Sex)
Test$Sex = as.numeric(Test$Sex)
cor(Train)


# Step 4: Build a Logistic Regression Model
model1 = glm(Survived~., data = Train, family = binomial)
summary(model1)



anova(model1, test = 'Chisq')


#Following are the insights we can collect for the output above:
  
# In the presence of other variables, variables such as Parch, 
#Embarked, and abs_col are not significant. We'll try building 
#another model without including them.
#The AIC value of this model is 883.79.

#Let's create another model and try to achieve a lower AIC value. 

#  Revise Model

model2 = glm(Survived ~ . - Parch - Fare, data = Train, family = binomial)
summary(model2)


#As you can see, we've achieved a lower AIC value and a better model. 
#Also, we can compare both the models using the ANOVA test. 
#Let's say our null hypothesis is that second model is better than 
#the first model. p < 0.05 would reject our hypothesis and in 
#case p > 0.05, we'll fail to reject the null hypothesis. 

#compare two models
anova(model1,model2,test = "Chisq")

#With p > 0.05, this ANOVA test also corroborates the fact that 
#the second model is better than first model.

log_predict <- predict(model2, Test, type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)


##For now, I've set the probability threshold value as 0.5. Let's get 
#the flavor of our model's accuracy. We'll use AUC-ROC score to 
#determine model fit. Higher the score, better the model. You can 
#also use confusion matrix to determine accuracy using confusionMatrix
#function from caret package. 

#plot ROC 
#installed.packages("ROCR")
#help(package=ROCR)
library(ROCR) 
#install.packages("Metrics") 

library(Metrics)
str(Test)
pr <- prediction(log_predict,Test$Survived)
# pr <- prediction(log_predict,Test$Survived)
 perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
 plot(perf)
 #> auc(Test$Survived,log_predict) #0.76343
 auc(Test$Survived,log_predict)
 
 
 
 log_predict1 <- predict(model2,newdata = Test,type = "response")
 log_predict1 <- ifelse(log_predict1 > 0.6,1,0)
 
  pr1 <- prediction(log_predict1,Test$Survived)
  perf <- performance(pr1,measure = "tpr",x.measure = "fpr")
 plot(perf)
 auc(Test$Survived,log_predict1)
 