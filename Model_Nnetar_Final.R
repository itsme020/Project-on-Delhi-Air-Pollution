Delhi <- read_excel("C:/Users/User/Downloads/Delhi air pollution(github)/delhi.xlsx")
##Load Library

library(padr)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)
library(forecast)
library(imputeTS)
library(DMwR)

###Data Analyze
summary(Delhi)
str(Delhi)
plot(Delhi)

##################Find Null Value###################33
Delhi$pm25 <- as.numeric(Delhi$pm25)
sum(is.na(Delhi$pm25))
str(Delhi)
plot(Delhi)
plot(Delhi$pm25,type = "l")

##Counting NA Values
sum(is.na(Delhi$pm25))

##########################find missing value in date column###########

library(padr)

Delhi1 <- pad(as.data.frame(Delhi$date))
colnames(Delhi1) <- 'date'  
Newdata <- full_join(Delhi1,Delhi)
sum(is.na(Newdata$pm25))
str(Newdata)
plotNA.distribution(Newdata$pm25)

####################Convert the data to time series#####################################
Newdata$pm25 <- ts(Newdata$pm25,start = c(2018,01),end=c(2018, 2617), frequency=365*24)
str(Newdata$pm25)
plot(Newdata$pm25)


######################################Imputation##################
library(imputeTS)
library(ggplot2)
Newdata$ma<-na_seasplit(Newdata$pm25,algorithm = "ma",find_frequency=TRUE)
plot(Newdata$ma)
#Newdata$interpolation<- na_seasplit(Newdata$pm25,algorithm = "interpolation",find_frequency=TRUE)

plot(Newdata$pm25)
str(Newdata$ma)
View(Newdata)

######  Testing For Data whether it is stationery Or Not
library(aTSA)
library(tseries)
stationary.test(Newdata$ma, method = "pp") # pp test
stationary.test(Newdata$ma,method = "adf")  #adf test
trend.test(Newdata$ma)

## Visualization  ##
library(ggplot2)




## With Missing Values ##
ggplot(data = Newdata,aes(x= date,y= pm25)) + 
  geom_line(colour='blue') +
  theme(panel.background = element_blank())+
  ylim(0,750)+
  ggtitle("Missing Values")

### With TIME SERIES MOVING AVERAGE
ggplot(data = Newdata,aes(x= date,y= ma)) + 
  geom_line(colour='red') +
  geom_line(colour="Blue",data = Newdata,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,750)+
  ggtitle("MOVING AVERAGE Values")

View(Newdata)
str(Newdata)

### splitting
train<- Newdata[1:2094,]
test<- Newdata[2095:2617,]
str(train)

train<- ts(train$ma)  
test<- ts(test$ma)
library(dplyr)

##Model building


#####################   Forecast Using Neural Network     ######################################

#Model_1#
set.seed(123)
fit <- nnetar(train)
accuracy(fit)                        ### Train RMSE = 31.99
fcast<- forecast(fit,h = 523)
pred_fit<- data.frame(predict(fcast,n.ahead = 523))
plot(forecast(fit,h = 72))
accuracy(pred_fit$Point.Forecast,test) ### Test RMSE = 94.29
View(pred_fit)

##  Run on Whole Data-Set
set.seed(123)
fit_3 <- nnetar(Newdata$ma)
accuracy(fit_3)                     ### Train RMSE = 35.48
fcast_3<- forecast(fit_3,h = 523)
pred_fit_3<- data.frame(predict(fcast_3,n.ahead = 523))
plot(forecast(fit_3,h = 523))
accuracy(fit_3$x,fit_3$fitted)



############################## Extra Models  ##########################################

# Model_2 #
set.seed(123)
fit_4<- nnetar(train,p = 24,size = 12)    
accuracy(fit_4)                             #### Train RMSE = 35.87
fcast_4<- forecast(fit_4,h = 523)
pred_fit_4<- data.frame(predict(fcast_4,n.ahead = 523))
plot(fcast_4)
accuracy(as.vector(fcast_4$mean),test)      ####  Test RMSE = 110.89
accuracy(pred_fit_4$Point.Forecast,test)



# Model_3 #
set.seed(123)
fit_5 <- nnetar(train,p = 24,size = 13)      
accuracy(fit_5)                           ### Train RMSE = 35.07
fcast_5<- forecast(fit_5,h = 523)
pred_fit_5<- data.frame(predict(fcast_5,n.ahead = 523))
plot(fcast_5)
accuracy(pred_fit_5$Point.Forecast,test)  ### Test RMSE = 106.78
