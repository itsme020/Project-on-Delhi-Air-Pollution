Delhi <- read_excel("C:/Users/User/Downloads/Delhi.xlsx")



library(padr)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)

library(forecast)
library(imputeTS)
library(DMwR)
Delhi<-delhi

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


## Splitting
train<- Newdata$ma[1:2094]
test<- Newdata$ma[2095:2617]

## Model Building
h_a<- holt(train,h = 523)
autoplot(h_a)
h_a$model
accuracy(h_a,test)##55.22

# identify optimal alpha parameter
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(train, beta = beta[i], h = 72)
  RMSE[i] <- accuracy(fit)[1,2]
 }

# convert to a data frame and idenitify min alpha value
beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))

# plot RMSE vs. alpha
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, aes(beta, RMSE), size = 2, color = "blue")

# new model with optimal beta
holt.a.opt <- holt(train, h = 523, beta = 0.0001)


accuracy(holt.a.opt)                                  ## Train RMSE = 55.17

fcast_holt<- forecast(holt.a.opt,h =523)
autoplot(holt.a.opt)
accuracy(as.vector(fcast_holt$mean),test)             ## Test RMSE = 141.45


######### RUN ON WHOLE DATA SET   #################

holts_wd<- holt(Newdata$ma, h = 523,beta = 0.0001)
accuracy(holts_wd)                                    ## RMSE = 53.61

# accuracy of first model
accuracy(holt.a.opt, test)
autoplot(holt.a.opt)
