Delhi <- read_excel("C:/Users/User/Downloads/Delhi air pollution(github)/delhi.xlsx")

library(padr)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)

library(forecast)
library(imputeTS)
library(DMwR)


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
View(Delhi1)
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

#Newdata$interpolation<- na_seasplit(Newdata$pm25,algorithm = "interpolation",find_frequency=TRUE)

plot(Newdata$pm25)
str(Newdata$ma)
View(Newdata)


######  Testing For Data whether it is stationery Or Not
library(aTSA)
library(tseries)
stationary.test(Newdata$ma, method = "pp") # pp test
stationary.test(Newdata$ma,method = "kpss")  #Kwiatkowski-Phillips-Schmidt-Shin test
trend.test(Newdata$ma)

##Decomposing
delhi_decomp<- stl(Newdata$ma,s.window = "periodic")
plot(delhi_decomp)

## Visualization  ##
library(ggplot2)


## With INTERPOLATIOn values

ggplot(data = Newdata,aes(x= date,y= interpolation)) + 
  geom_line(colour='red') +
  geom_line(colour='blue',data = Newdata,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,750)+
  ggtitle("Interpolation Values")


### With TIME SERIES MOVING AVERAGE
ggplot(data = Newdata,aes(x= date,y= ma)) + 
  geom_line(colour='red') +
  geom_line(colour="Blue",data = Newdata,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,750)+
  ggtitle("MOVING AVERAGE Values")

View(Newdata)
Newdata<- Newdata[,-c(2)]

##Splitting Into Train n Test in Time Series
train<- Newdata[1:2094,]
test<- Newdata[2095:2617,]
str(train)

train<- ts(train$ma)
test<- ts(test$ma)

library(dplyr)
library(smooth)
library(fpp)

##Using Holts Winter Method
#considering data has only level parameter
# alpha = 0.2
hw_a<- HoltWinters(train,alpha = 0.2, beta = F,gamma = F)
accuracy(hw_a$fitted,train)                         #### Train RMSE = 88.93
hwa_pred<- data.frame(predict(hw_a,n.ahead = 523))
plot(forecast(hw_a,h =523))
accuracy(hwa_pred$fit,test)                         #### Test RMSE = 94.44
hwa_mape<- MAPE(hwa_pred$fit,test)*100
hwa_mape

## Considering it has level and trend
# alpha = 0.15 and beta = 0.2
hw_ab<- HoltWinters(train,alpha = 0.2 , beta = 0.1,gamma = F)
accuracy(hw_ab$fitted,train)                       #### Train RMSE = 101.54
hwab_pred<- data.frame(predict(hw_ab,n.ahead = 523))
plot(forecast(hw_ab,h = 523))
accuracy(hwab_pred$fit,test)                        #### Test RMSE = 637.03
hwab_mape<- MAPE(hwab_pred$fit,test)*100
hwab_mape

## Without Optimum Value
# with only level Parameter
hw_na<- HoltWinters(train,beta = F, gamma = F)
accuracy(hw_na$fitted,train)                       #### Train RMSE = 55.18
hwna_pred<- data.frame(predict(hw_na,n.ahead = 523))
plot(forecast(hw_na,h = 523))
accuracy(hwna_pred$fit,test)                       #### Test RMSE = 162.14
hwna_mape<- MAPE(hwna_pred$fit,test)*100
hwna_mape

#with level n trend Parameter
hw_nab<- HoltWinters(train,gamma = F)
accuracy(hw_nab$fitted,train)                     ### Train RMSE = 55.32
hwnab_pred<- data.frame(predict(hw_nab,n.ahead = 523))
plot(forecast(hw_nab,h = 523))
accuracy(hwnab_pred$fit,test)                    #### Test RMSE = 222.58
hwnab_mape<- MAPE(hwnab_pred$fit,test)*100
hwnab_mape

## Creating Table
df_mape<- data.frame(Model = c("hwa_mape","hwab_mape","hwna_mape","hwnab_mape"),Values = c(hwa_mape,hwab_mape,hwna_mape,hwnab_mape))
View(df_mape)

##########################################################################################

##### RUN ON WHOLE DATASET  #####

new_model<- HoltWinters(Newdata$ma)
accuracy(new_model$fitted,Newdata$ma)      ### RMSE = 56.15
plot(forecast(new_model,h= 523))
forecast_new<- data.frame(predict(new_model,n.ahead = 523))
View(forecast_new)
