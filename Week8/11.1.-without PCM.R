# -------------------- Answer for Question 11.1 ----------------------------- # Clear environment
rm(list = ls())
# Set seed to reproduce results
set.seed(100)
# ---------------------------- Data manipulation -------------------------------------
# First, read in the data
#
c_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
#
# Optional check #
head(c_data)
# ---------------------------- Start Stepwise Regression -------------------------------------
# Stepwise Regression using original variables and Cross Validation # In backward stepwise regression.
#Scaling the data except the response variable and categorical
s_Data = as.data.frame(scale(c_data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)])) 
s_Data <- cbind(c_data[,2],s_Data,c_data[,16]) # Add column 2 back in 
colnames(s_Data)[1] <- "So"
colnames(s_Data)[16] <- "Crime"
library(caret)
# Perform 5 fold CV
ct_data <- trainControl(method = "repeatedcv", number = 5, repeats = 5) lmFit_Step <- train(Crime ~ ., data = s_Data, "lmStepAIC", scope = list(lower = Crime~1, upper = Crime~.), direction = "backward",trControl=ct_data)
##Step: AIC=503.93
##.outcome ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob #
#Fitting a new model with the above 8 variables
#
mod_data = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = s_Data) summary(mod_data)
cv.lm(c_data,form.lm = formula(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1),m=3,dots=FALSE,plotit = TRUE,printit = TRUE)

##
##Residual standard error: 195.5 on 38 degrees of freedom ##Multiple R-squared: 0.7888, Adjusted R-squared: 0.7444 ##F-statistic: 17.74 on 8 and 38 DF, p-value: 1.159e-10
#We got an Adjusted R-SQuared value = 0.7444 using the selected 8 variables using # Backward StepWise regression and Cross Validation
# Now let's use cross-validation twith only 47 data points, to see how good is the model # 47-fold cross-validation
#
SStot <- sum((c_data$Crime - mean(c_data$Crime))^2) totsse <- 0
for(i in 1:nrow(s_Data)) {
  mod_data_i = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = s_Data[-i,]) pred_i <- predict(mod_data_i,newdata=s_Data[i,])
  totsse <- totsse + ((pred_i - c_data[i,16])^2)
  
}
R2_mod <- 1 - totsse/SStot R2_mod
## 0.6676
# Notice that in the model above, the p-value for M.F is above 0.1. # Will keep in this model
mod_data1 = lm(Crime ~ U1+Prob+U2+M+Ed+Ineq+Po1, data = s_Data) summary(mod_data1)
cv.lm(s_Data,form.lm = formula(Crime ~ U1+Prob+U2+M+Ed+Ineq+Po1),m=3,dots=FALSE,plotit = TRUE,printit = TRUE)
#Residual standard error: 199.8 on 39 degrees of freedom #Multiple R-squared: 0.7738, Adjusted R-squared: 0.7332 #F-statistic: 19.06 on 7 and 39 DF, p-value: 8.805e-11
# Now we can take out insignificant U1

mod_data2 = lm(Crime ~ Prob+U2+M+Ed+Ineq+Po1, data = s_Data) summary(mod_data2)
cv.lm(s_Data,form.lm = formula(Crime ~ Prob+U2+M+Ed+Ineq+Po1),m=3,dots=FALSE,plotit = TRUE,printit = TRUE)
##
##Residual standard error: 200.7 on 40 degrees of freedom ##Multiple R-squared: 0.7659, Adjusted R-squared: 0.7307 ##F-statistic: 21.8 on 6 and 40 DF, p-value: 3.42e-11
# This model looks ok, perform cross-validation:
SStot <- sum((c_data$Crime - mean(c_data$Crime))^2) totsse <- 0
for(i in 1:nrow(s_Data)) {
  mod_data2_i = lm(Crime ~ Prob+U2+M+Ed+Ineq+Po1, data = s_Data[-i,]) pred_i <- predict(mod_data2_i,newdata=s_Data[i,])
  totsse <- totsse + ((pred_i - c_data[i,16])^2)
}

R3_mod <- 1 - totsse/SStot R3_mod
## 0.666
# cross-validation shows the same results
# whether we include M.F and U1 (0.668) or not (0.666). looks like M.F and U1 really aren't significant. # Since the quality is about the same,use simpler model instead
# ---------------------------- Lasso Regression -------------------------------------
library(glmnet)
#building lasso Regression Model
XP=data.matrix(s_Data[,-16])
YP=data.matrix(s_Data$Crime) lasso=cv.glmnet(x=as.matrix(s_Data[,-16]),y=as.matrix(s_Data$Crime),alpha=1,
                                             nfolds = 5,type.measure="mse",family="gaussian")
#Output the coefficients of the variables selected by lasso
coef(lasso, s=lasso$lambda.min)
#Fitting a new model with below significant 9 variables
mod_lasso1 = lm(Crime ~So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = s_Data) summary(mod_lasso1)

cv.lm(s_Data,form.lm = formula(Crime ~So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob),m=3,dots=FALSE,plotit = TRUE,printit = TRUE)
##
##Residual standard error: 204.5 on 37 degrees of freedom ##Multiple R-squared: 0.775, Adjusted R-squared: 0.72 ##F-statistic: 14.2 on 9 and 37 DF, p-value: 1.54e-09
#there is a slightly lower Adjusted R-SQuared value = 0.72 using the selected 9 variables using # Lasso regression and Cross Validation
# Perform cross validation
SStot <- sum((c_data$Crime - mean(c_data$Crime))^2) totsse <- 0
for(i in 1:nrow(s_Data)) {
  mod_lasso1_i = lm(Crime ~ So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = s_Data[-i,]) pred_i <- predict(mod_lasso1_i,newdata=s_Data[i,])
  totsse <- totsse + ((pred_i - c_data[i,16])^2)
  
}
LR3_mod <- 1 - totsse/SStot LR3_mod
## 0.666
# based on above observation, three of the variables (So, M.F, and NW) # are not significant. if we remove them,
#It's exactly the same model we got above
# stepwise regression model!
# Please refer above R3_mod value
# ---------------------------- Elastic Net Regression-------------------------------------
#We vary alpha in steps of 0.1 from 0 to 1 and calculate the resultant R-Squared values
R2=c()
for (i in 0:10) {
  mod_elastic = cv.glmnet(x=as.matrix(s_Data[,-16]),y=as.matrix(s_Data$Crime), alpha=i/10,nfolds = 5,type.measure="mse",family="gaussian")
  #will use The deviance(dev.ratio ) - the % of deviance explained,
  R2 = cbind(R2,mod_elastic$glmnet.fit$dev.ratio[which(mod_elastic$glmnet.fit$lambda == mod_elastic$lambda.min)])
}

R2
##[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
##[1,] 0.7062797 0.7535141 0.7386241 0.7402678 0.7603113 0.7734391 0.7886831 0.763631 0.7966378
##[,10] [,11]
##[1,] 0.7569592 0.7726187
#Best value of alpha
alpha_best = (which.max(R2)-1)/10 alpha_best
## 0.8
# best value of alpha may not lie between 0 and 1
#model using this alpha value.
E_net=cv.glmnet(x=as.matrix(s_Data[,-16]),y=as.matrix(s_Data$Crime),alpha=alpha_best, nfolds = 5,type.measure="mse",family="gaussian")
#Output the coefficients of the variables selected by Elastic Net
coef(E_net, s=E_net$lambda.min)
# The Elastic Net selects 13 variables compared to 10 in Lasso and 8 in Step Wise. # compare this new model with the Lasso and Step Wise models

mod_Elastic_net = lm(Crime ~So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = s_Data)
summary(mod_Elastic_net)
cv.lm(s_Data,form.lm = formula(Crime ~So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob),m=3,dots=FALSE,plotit = TRUE,printit = TRUE)
##
##Residual standard error: 204 on 33 degrees of freedom ##Multiple R-squared: 0.8005, Adjusted R-squared: 0.7219 ##F-statistic: 10.19 on 13 and 33 DF, p-value: 4.088e-08
# The R-SQuared value is similar using Elastic Net and 13 variables. So 3 more variables does not make any difference
# Perform Cross Validation
SStot <- sum((c_data$Crime - mean(c_data$Crime))^2) totsse <- 0
for(i in 1:nrow(s_Data)) {
  
  mod_Enet_i = lm(Crime ~ So+M+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = s_Data[-i,])
  pred_i <- predict(mod_Enet_i,newdata=s_Data[i,])
  totsse <- totsse + ((pred_i - c_data[i,16])^2) }
ER5_mod <- 1 - totsse/SStot ER5_mod
## 0.574
# Elastic Net on the above data set is performing far worse than lasso and Stepwise regression #