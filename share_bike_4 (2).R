#######    ****  PROJECT: BIKE SHARING PREDICTION MODE  **** #############
###   

### PACKAGES THAT WILL BE USED 

install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("pander")

install.packages("relaimpo")
install.packages("RColorBrewer")

library(MASS)
library(PASWR2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(ggplot2)
library(relaimpo)
library(RColorBrewer)
library(caret)

################################
### two independent samples 

hour_file = read.csv("G:\\DA Project\\hour.csv", header = TRUE)
head(hour_file)
names(hour_file)


day_file= read.csv("G:\\DA Project\\day.csv",header = TRUE)
head(day_file)

##############################

#YASH
###### CSV files summary #####
nrow(day_file)
ncol(day_file)
dim(day_file)
typeof(day_file)

nrow(hour_file)
ncol(hour_file)
dim(hour_file)
typeof(hour_file)

############################################

##########################################################3

sum(day_file$holiday==1)



null_check = is.null(hour_file)
na_check = is.na(day_file)
null_check_hr = is.null(day_file)
na_check_hr = is.na(hour_file)

null_check
null_check_hr

# Scatter plot, Visualize the linear relationship between the predictor and response
scatter.smooth(x=day_file$temp, y=day_file$cnt, main="cnt ~ temperature")
scatter.smooth(x=hour_file$temp, y=hour_file$cnt, main="cnt ~ temperature")


day_file$dteday <- as.Date(day_file$dteday)
day_file$season <- as.factor(day_file$season)
day_file$yr <- as.factor(day_file$yr)
day_file$mnth <- as.factor(day_file$mnth)
day_file$holiday <- as.factor(day_file$holiday)
day_file$weekday <- as.factor(day_file$weekday)
day_file$workingday <- as.factor(day_file$workingday)
day_file$weathersit <- as.factor(day_file$weathersit)
#day_file$cnt <- as.factor(day_file$cnt)
hour_file$dteday <- as.Date(hour_file$dteday)
hour_file$season <- as.factor(hour_file$season)
hour_file$yr <- as.factor(hour_file$yr)
hour_file$hr <- as.factor(hour_file$hr)
hour_file$mnth <- as.factor(hour_file$mnth)
hour_file$holiday <- as.factor(hour_file$holiday)
hour_file$weekday <- as.factor(hour_file$weekday)
hour_file$workingday <- as.factor(hour_file$workingday)
hour_file$weathersit <- as.factor(hour_file$weathersit)



####   *****    Exploratory  Data  Analysis *****  ######################
summary(day_file)
summary(hour_file)

### Visualization done on the year wise session 

str(day_file)
str(hour_file)


#### Boxplot of rental bikes per year#####

ggplot(day_file,aes(yr,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per year") +
  scale_x_discrete(labels = c("2011","2012"))

ggplot(hour_file,aes(yr,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per year According to hours") +
  scale_x_discrete(labels = c("2011","2012"))


### analyzing the bikes increased in 2012 from 2011


### boxplot of rental bikes based per season#####
#

col <- brewer.pal(4,"Set3")
ggplot(day_file,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Spring","Summer","Fall","Winter"))

col <- brewer.pal(4,"Set3")
ggplot(hour_file,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Spring","Summer","Fall","Winter"))


### the number of bikes rental are increased in summer and fall times

########

col <- brewer.pal(12,"Set3")
ggplot(day_file,aes(mnth,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per month")

col <- brewer.pal(12,"Set3")
ggplot(hour_file,aes(mnth,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per month")

#People use Shared bikes less often than other seasons in the spring,possibly beacause of the local spring climate.


### Box plot to check the respnse about yes or no#####
ggplot(day_file,aes(holiday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by holiday") +
  scale_x_discrete(labels = c("no","yes"))

ggplot(hour_file,aes(holiday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by holiday") +
  scale_x_discrete(labels = c("no","yes"))

#####################################################3
col <- brewer.pal(7,"Set3")
ggplot(day_file,aes(weekday,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weekday")

col <- brewer.pal(7,"Set3")
ggplot(hour_file,aes(weekday,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weekday")


#####
## analysed that weekdays are not affecting the count variable so that we can drop that variable


#### analyzing the working days 
ggplot(day_file,aes(workingday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by workingday") +
  scale_x_discrete(labels = c("no","yes"))

ggplot(hour_file,aes(workingday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by workingday") +
  scale_x_discrete(labels = c("no","yes"))

##############################################333
##########


###finding the missing value 
sum(is.na(day_file))
sum(is.na(hour_file))

##### To check the exploratory data analysis by normal distribution curve ###### 
## of the data 
## responsible variable  count is distributed 
## here it is normally distributed 
## median here is normally distributed 
## plotting tyhe frquency of rental with total bikes 

#day
h <- hist(day_file$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue' )

xfit <- seq(min(day_file$cnt),max(day_file$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(day_file$cnt),sd=sd(day_file$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(day_file$cnt)
lines(xfit,yfit, col='red', lwd= 3)

#hour
h <- hist(hour_file$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'red' )

xfit <- seq(min(hour_file$cnt),max(hour_file$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(hour_file$cnt),sd=sd(hour_file$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(hour_file$cnt)
lines(xfit,yfit, col='blue', lwd= 3)

### plotting the box plot for the working day and holiday 
##the medium value is increased for working dayy 
## this means that maximun bike rides are over the working days


boxplot(day_file$cnt ~ day_file$holiday,
        data = day_file,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(hour_file$cnt ~ hour_file$holiday,
        data = hour_file,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("red", "red1", "red2", "red3")) 


## Time series analysis
data_ts <-msts(hour_file[,'cnt'], seasonal.periods=c(7))
train_ts <- head(data_ts, round(length(data_ts) * 0.9))
test_ts <- tail(data_ts, round(length(data_ts) * 0.1))
plot(train_ts, xlab="Weeks", ylab="Bike riders")

data_ts_ <-msts(day_file[,'cnt'], seasonal.periods=c(7))
train_ts_ <- head(data_ts_, round(length(data_ts_) * 0.9))
test_ts_ <- tail(data_ts_, round(length(data_ts_) * 0.1))
plot(train_ts_, xlab="Weeks", ylab="Bike riders")

## Decomposition
#hour
plot(decompose(train_ts, type='add'), xlab="Weeks")
#day
plot(decompose(train_ts_, type='add'), xlab="Weeks")

### 
## correct  

###########################################################################

#SHUBHAM


##Importing the csv in the R for the regression and EDA
##imported Hours csv file and keep it into 

### two independent samples 

hour_file = read.csv("G:\\DA Project\\hour.csv", header = TRUE)
head(hour_file)
names(hour_file)
nrow(hour_file)

day_file = read.csv("G:\\DA Project\\day.csv", header = TRUE)
head(day_file)
names(day_file)
nrow(day_file)


###################################3
#####   HOUR   #####
######################################################
#hold-out
#Backward-elimination
#hour-file
#Sample the Data by 70 percent training and remaining percent of test data
select.data = sample(1:nrow(hour_file),0.75*nrow(hour_file))
#train and test data
train.hour_file =hour_file[select.data,]
test.hour_file=hour_file[-select.data,]
nrow(train.hour_file)
nrow(test.hour_file)
head(train.hour_file)
modelhr<-lm(cnt~yr+weathersit+season+temp+hum+workingday+mnth+holiday,data=train.hour_file)
summary(modelhr)
modelhr<-lm(cnt~yr+weathersit+season+temp+hum+mnth+holiday,data=train.hour_file)
summary(modelhr)
modelhr<-lm(cnt~yr+weathersit+season+temp+hum+holiday,data=train.hour_file)
summary(modelhr)
###########################################################



####################################################
#####v   DAY   ######
####################################################
#hold-out
#Backward-elimination
#day-file
#Sample the Data by 70 percent training and remaining percent of test data
select.data = sample(1:nrow(day_file),0.75*nrow(day_file))
#train and test data
train.day_file =day_file[select.data,]
test.day_file=day_file[-select.data,]
nrow(train.day_file)
nrow(test.day_file)
head(train.day_file)
#Backward MLR
modelhr_day<-lm(cnt~yr+weathersit+season+temp+workingday+holiday,data=train.day_file)
summary(modelhr_day)



#######################################
#AIC-BIC-hour_file
base_model<-lm(cnt~hum,data=train.hour_file)
summary(base_model)
full_model<-lm(cnt~yr+weathersit+season+temp+hum+workingday+mnth+holiday+weekday+yr+hr+windspeed,data=train.hour_file)
summary(full_model)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="forward",trace=T)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="both",trace=T)
myaic=step(full_model,direction="backward",trace=T)

summary(myaic)
nrow(base_model)
##############################################

#######################################
#AIC-BIC-day_file
base_model<-lm(cnt~temp,data=train.day_file)
summary(base_model)
full_model<-lm(cnt~yr+weathersit+season+temp+hum+workingday+mnth+holiday,data=train.day_file)
summary(full_model)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="forward",trace=T)
myaic<-step(base_model,scope=list(upper=full_model,lower=~1),direction="both",trace=T)
myaic=step(full_model,direction="backward",trace=T)
summary(myaic)
nrow(base_model)
############################################################33






#########################################
#n-fold-hour

###############
library(tidyverse)
set.seed(123)
training.samples <- hour_file$cnt %>%
createDataPartition(p = 0.75, list = FALSE)
train.data  <- hour_file[training.samples, ]
set.seed(123)
train.control <- trainControl(method = "cv", 
                              number = 10)
head(train.control)
test.data <- hour_file[-training.samples, ]

model <- train(cnt ~ yr+weathersit+season+temp+weekday+hum+hr+holiday+windspeed, data = hour_file, method = "lm",trControl = train.control)
print(model)
summary(model)

#########################################
#n-fold-day
#day
library(tidyverse)
set.seed(123)
training.samples <- day_file$cnt %>%
createDataPartition(p = 0.75, list = FALSE)
train.data  <- day_file[training.samples, ]
set.seed(123)
train.control <- trainControl(method = "cv", 
                              number = 10)
head(train.control)
test.data <- day_file[-training.samples, ]

model <- train(cnt ~ yr+weathersit+season+temp+mnth+holiday+weekday, data = day_file, method = "lm",trControl = train.control)
print(model)
summary(model)


#According to our analysis when we add year variable it impacts a lot on our model.
#The day file AIC BIC and MLR using N-fold is compared
#AIC BIC of day.csv gives best model

############################################
#PERFORMING RESIDUAL ANALYSIS
############################################

#STANDARD RESIDUAL VS PREDICTED VALUES

residual=rstandard(myaic)
plot(fitted(myaic),residual,main="Predicted values V/s the Standard Residual")
abline(a=0,b=0,col='red')

#STANDARD RESIDUAL VS  X-VARIABLES
plot(full_model$yr+weathersit+season+temp+hum+workingday+mnth+holiday,residual,main = "X-VARIABLE VS RESIDUAL ANALYSIS")
abline(a=0,b=0,col='red')

#EXAMINE RESIDUALS ARE NORMAL OR NOT
qqnorm(residual)
qqline(residual,col=2)

#USING SHAPIRO WILK NORMALITY TEST
shapiro.test(residual)
