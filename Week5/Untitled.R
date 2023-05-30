#Import and Read Library
require(ggthemes)
library(tidyverse) 
library(magrittr) 
library(TTR) 
library(tidyr) 
library(dplyr) 
library(lubridate) 
library(ggplot2) 
library(plotly) 
library(fpp2)    
library(forecast) 
library(caTools) 
library(reshape2) 
library(psych)  
require(graphics) 
require(Matrix) 
library(corrplot) 
library(mltools) 
library(fBasics) 
library(caret)

# Read Data from usCrime.txt
data_c = read.csv("uscrime.txt",sep = "") 
str(data_c) 

#Data Validation
is.null(data_c)
print(summary(data_c))


# Visualizations via Box plots  
mData <- melt(data_c) 
p <- ggplot(mData, aes(factor(variable), value))  
p + geom_boxplot() + facet_wrap(~variable, scale="free")+theme_economist()+scale_colour_economist() 


#Visulations of Density plot.safs
dens <- data_c %>% 
  gather() %>%                          
  ggplot(aes(value)) +                  
  facet_wrap(~ key, scales = "free") + 
  geom_density()+theme_economist()+scale_colour_economist() 
dens


#Build Linear Model-1
model_1 <- lm(Crime ~ .  ,data_c) 
print(summary(model_1)) 



# Use the first linear regression model to make the prediction using the given test datapoints
# Given unscale Test Data
test<-data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5, 
                 LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120, 
                 U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0) 
print(predict(model_1,test)) 

# Crime rate for model 1 - 155.4349 
# This predication is low given the test predictors are well within all the other predictors
# we expect the value between min and maximum response but this value is less than the minium crime #342.

# Cause : However The P-value, F-stat and R-Square of the entire model looked good , predictis used in this model are not statistically 
# Signigicant. and Thus result in overfitting issue.


#Build Linear Model 2 after removing few Predictors
model_2 <- lm(Crime ~ M+Ed+Po1+U2+Ineq+Prob ,data_c) 
print(summary(model_2)) 
print(predict(model_2,test))

# removing some predictors actually help improve the model’s adjusted R-squared.
# Model 2 value of 1304.245 is much better and reasonable as it is between min and max value
# Improved p-Value , F-Stat and R-Square value proves the predictors are statistically Significant

# Diagnostics
# Compute the analysis of variance

R_AOV <- aov(model_2,data = data_c)
summary(R_AOV)
plot(R_AOV,1)

#Normality of residual Check
plot(R_AOV,2)

#histogram of residuals 
ggplot(model_2, aes(x=.resid))+geom_histogram(binwidth = 30)+ geom_histogram(bins=30) 

#Observation :  As per above plot , we have constant vaiance of residuals and it is good
# QQ plot shows that residuals appreaded to be normal in the core but not at theouter area
# Histogram shows few outliers ( may or may not need corrective actions)
