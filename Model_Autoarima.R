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
stationary.test(Newdata$ma,method = "adf")  #adf test
trend.test(Newdata$ma)

## Visualization  ##
library(ggplot2)

### With TIME SERIES MOVING AVERAGE
ggplot(data = Newdata,aes(x= date,y= ma)) + 
  geom_line(colour='red') +
  geom_line(colour="Blue",data = Newdata,aes(x= date,y= pm25))+
  theme(panel.background = element_blank())+
  ylim(0,750)+
  ggtitle("MOVING AVERAGE Values")

View(Newdata)
str(Newdata)



library(forecast)


### splitting
train<- Newdata[1:2094,]
test<- Newdata[2095:2617,]
str(train)

train<- ts(train$ma)
test<- ts(test$ma)
library(dplyr)


##################### Model Building  ######################################

## AUTO-ARIMA ##
library(forecast)
install.packages("forecast")

model_aa<- auto.arima(train,method = "ML")
accuracy(model_aa)                                    ####  Train RMSE = 50.33
pred_aa <- data.frame(forecast(model_aa,h = 523))
fcast_aa<- forecast(model_aa,h= 523)
plot(fcast_aa)
accuracy(as.vector(fcast_aa$mean),test)               #### Test RMSE = 91.16



##### Run Model On Whole Dataset
model_aa_wd<- auto.arima(Newdata$ma,method = "ML")
accuracy(model_aa_wd)                                 #### Train RMSE = 52.78
