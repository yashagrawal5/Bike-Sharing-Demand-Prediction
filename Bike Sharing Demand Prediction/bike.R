# Reading the csv files
bike_share_train <- read.csv("bike_train.csv", header=T)
bike_share_test <- read.csv("test.csv", header=T)

str(bike_share_train)
#head(bike_share_train)

# Ignore the casual, registered fields as this sum is equal to count field
bike_share_train <- bike_share_train[,-c(10,11)]

# Converting integer to factor on training set
bike_share_train$season <- as.factor(bike_share_train$season)
bike_share_train$holiday <- as.factor(bike_share_train$holiday)
bike_share_train$workingday <- as.factor(bike_share_train$workingday)
bike_share_train$weather <- as.factor(bike_share_train$weather)


# Converting int to factor on test set
bike_share_test$season <- as.factor(bike_share_test$season)
bike_share_test$holiday <- as.factor(bike_share_test$holiday)
bike_share_test$workingday <- as.factor(bike_share_test$workingday)
bike_share_test$weather <- as.factor(bike_share_test$weather)


#Deriving day, hour from datetime field Train & Test
library(lubridate,quietly = TRUE)
bike_share_train$datetime <- ymd_hms(bike_share_train$datetime)
bike_share_train$hour <- hour(bike_share_train$date)
bike_share_train$day <- wday(bike_share_train$date)
bike_share_train$month <- month(bike_share_train$date, label=T)
str(bike_share_train) 
bike_share_train[,11:13]<-lapply(bike_share_train[,11:13], factor) #converting multiple derived variables into factors


bike_share_test$datetime <- ymd_hms(bike_share_test$datetime)
bike_share_test$hour <- hour(bike_share_test$date)
bike_share_test$day <- wday(bike_share_test$date)
bike_share_test$month <- month(bike_share_test$date, label=T)
str(bike_share_test)
names(bike_share_test)
bike_share_test[,10:12]<-lapply(bike_share_test[,10:12], factor) #converting derived variables into factors



# Removing datetime field 
bike_share_train <- bike_share_train[,-1]

# Understand the distribution of numerical variables and generate a frequency table for numeric variables.  
# we'll test and plot a histogram for each numerical variables and analyze the distribution.
hist(bike_share_train$count)
hist(as.numeric(bike_share_train$season))
hist(as.numeric(bike_share_train$weather))
hist(bike_share_train$humidity)
hist(as.numeric(bike_share_train$holiday))
hist(as.numeric(bike_share_train$workingday))
hist(as.numeric(bike_share_train$temp))
hist(as.numeric(bike_share_train$atemp))
hist(as.numeric(bike_share_train$windspeed))

#Exploratory Data Analysis
library(sqldf, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# Get the average count of bikes rent by season, hour
season_summary_by_hour <- sqldf('select season, hour, avg(count) as count from bike_share_train group by season, hour')

# From this plot it shows, 
# There are more rental in morning(from 7-9th hour) and evening(16-19th hour)
# People rent bikes more in Fall, and much less in Spring
p1<-ggplot(bike_share_train, aes(x=hour, y=count, color=season))+
  geom_point(data = season_summary_by_hour, aes(group = season))+
  geom_line(data = season_summary_by_hour, aes(group = season))+
  ggtitle("Bikes Rent By Season")+ theme_minimal()+
  scale_colour_hue('Season',breaks = levels(bike_share_train$season), 
                   labels=c('spring', 'summer', 'fall', 'winter'))

p1
# Get the average count of bikes rent by weather, hour
weather_summary_by_hour <- sqldf('select weather, hour, avg(count) as count from bike_share_train group by weather, hour')

# From this plot it shows, 
# People rent bikes more when weather is good
# We see bike rent only at 18th hour when weather is very bad
p2<-ggplot(bike_share_train, aes(x=hour, y=count, color=weather))+
  geom_point(data = weather_summary_by_hour, aes(group = weather))+
  geom_line(data = weather_summary_by_hour, aes(group = weather))+
  ggtitle("Bikes Rent By Weather")+ scale_colour_hue('Weather',breaks = levels(bike_share_train$weather), 
                                                     labels=c('Good', 'Normal', 'Bad', 'Very Bad'))

p2
# Get the average count of bikes rent by day, hour
day_summary_by_hour <- sqldf('select day, hour, avg(count) as count from bike_share_train group by day, hour')

# From this plot it shows, 
# There are more bikes rent on weekdays during morining and evening
# There are more bikes rent on weekends during daytime
p3<-ggplot(bike_share_train, aes(x=hour, y=count, color=day))+
  geom_point(data = day_summary_by_hour, aes(group = day))+
  geom_line(data = day_summary_by_hour, aes(group = day))+
  ggtitle("Bikes Rent By Weekday")+ scale_colour_hue('Weekday',breaks = levels(bike_share_train$day),
                                                     labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
p3
#Correlation Analysis
# Correlation plot between fields
# It shows that temp, atemp has much correlation
bike_share_train_subset <- bike_share_train[,5:9]
bike_share_train_subset$humidity <- as.numeric(bike_share_train_subset$humidity)
bike_share_train_subset$count <- as.numeric(bike_share_train_subset$count)

#Correlation Calculation using function
train_cor <- cor(bike_share_train_subset)
library(corrplot, quietly = TRUE)
corrplot(train_cor, method = 'color', addCoef.col="black")

#Hypothesis Testing (using multivariate analysis)
boxplot(bike_share_train$count~bike_share_train$hour,xlab="hour", ylab="count of users")
# Splitting the Train dataset
library(caTools, quietly = TRUE)
set.seed(123)
split <- sample.split(bike_share_train$count, SplitRatio = 0.75)
training_set <- subset(bike_share_train, split == TRUE)
validation_set <- subset(bike_share_train, split == FALSE)

# Applying Linear Regression model
lmBikeRent <- lm(count~., data = training_set)
summary(lmBikeRent)

#Draw histogram. Here is showes right skews 
hist(bike_share_train$count)

#Stepwise Model Selection
# Now performs stepwise model selection by AIC with both directions(Forward, Backward)
library(MASS,quietly = TRUE)
library(car,quietly = TRUE)
lmBikeRentAIC<-stepAIC(lmBikeRent, direction="both")
#---lmBikeRentVIF<-vif(lmBikeRent)
summary(lmBikeRentAIC)

#Prediction on Validation set
# Apply prediction on validation set
lm_predict_validation <- predict(lmBikeRentAIC, newdata = validation_set)


# Let's check the summary of predicted count values
cat("\n")
print("summary of predicted count values")
summary(lm_predict_validation)

# summary of actual count values
print("summary of actual count values")
summary(validation_set$count)


#Log Transformation
# Since we got negative predicted values, let's do log transformation and run regression model again
lmBikeRentLog <- lm(log(count)~., data = training_set)

# Now performs stepwise model selection on log model
lmBikeRentLogAIC <- stepAIC(lmBikeRentLog, direction="both")

lm_predict_validation_log <- predict(lmBikeRentLogAIC,newdata=validation_set)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_validation_nonlog <- exp(lm_predict_validation_log)

# Let's check the summary of predicted count values, it shows there are no negative values
print("summary of predicted count values after log transformation")
summary(lm_predict_validation_nonlog)

summary(lmBikeRentLog)
