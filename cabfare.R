rm(list = ls())
setwd("C:\\Users\\Ramya\\Desktop\\cabfare")
# Loading Datasets
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
test = read.csv("test.csv")
str(train)
str(test)
summary(train)
summary(test)
#Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount))
#Fare amount has negative values and also cannot be 0. So we will remove these fields.
train[which(train$fare_amount < 1 ),]
nrow(train[which(train$fare_amount < 1 ),])
train = train[-which(train$fare_amount < 1 ),]
#Passenger_count
for (i in seq(4,11,by=1)){
  print(paste('passenger_count above ' ,i,nrow(train[which(train$passenger_count > i ),])))
}   
#so 20 observations of passenger_count is consistenly above from 6,7,8,9,10 passenger_counts, let's check them.
train[which(train$passenger_count > 6 ),]
# Also we need to see if there are any passenger_count==0
train[which(train$passenger_count <1 ),]
nrow(train[which(train$passenger_count <1 ),])
#We will remove these 58 observations and 20 observation which are above 6 value because a cab cannot hold these number of passengers.
train = train[-which(train$passenger_count < 1 ),]
train = train[-which(train$passenger_count > 6),]
#Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges
print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(train[which(train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(train[which(train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))
# There's only one outlier which is in variable pickup_latitude.So we will remove it with nan.
# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
# there are values which are equal to 0. we will remove them.
train = train[-which(train$pickup_latitude > 90),]
train = train[-which(train$pickup_longitude == 0),]
train = train[-which(train$dropoff_longitude == 0),]
#checking the na values according to each column
colSums(is.na(train))
train=na.omit(train)
colSums(is.na(train))
#Convert pickup_datetime from factor to date time
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$pickup_weekday = as.factor(format(train$pickup_date,"%u"))# Monday = 1
train$pickup_mnth = as.factor(format(train$pickup_date,"%m"))
train$pickup_yr = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$pickup_hour = as.factor(format(pickup_time,"%H"))
#Add same features to test set
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$pickup_weekday = as.factor(format(test$pickup_date,"%u"))# Monday = 1
test$pickup_mnth = as.factor(format(test$pickup_date,"%m"))
test$pickup_yr = as.factor(format(test$pickup_date,"%Y"))
pickup_time = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$pickup_hour = as.factor(format(pickup_time,"%H"))
train = subset(train,select = -c(pickup_datetime,pickup_date))
test = subset(test,select = -c(pickup_datetime,pickup_date))
#convert factor levels to numeric
train$pickup_mnth= as.numeric(factor(train$pickup_mnth))
train$pickup_weekday= as.numeric(factor(train$pickup_weekday))
train$pickup_yr= as.numeric(factor(train$pickup_yr))
train$pickup_hour= as.numeric(factor(train$pickup_hour))
test$pickup_mnth= as.numeric(factor(test$pickup_mnth))
test$pickup_weekday= as.numeric(factor(test$pickup_weekday))
test$pickup_yr= as.numeric(factor(test$pickup_yr))
test$pickup_hour= as.numeric(factor(test$pickup_hour))
train=na.omit(train)
#Let us calculate the distance using the haversine formula and will store it in a new variable distance
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}
#Using haversine formula to calculate distance fr both train and test
train$dist = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)
test$dist = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)
# We will remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
nrow(train[which(train$distance ==0 ),])
nrow(test[which(test$distance==0 ),])
nrow(train[which(train$distance >130 ),]) # considering the distance 130 as max and considering rest as outlier.
nrow(test[which(test$distance >130 ),])
#Normalisation
print('dist')
train[,'dist'] = (train[,'dist'] - min(train[,'dist']))/
  (max(train[,'dist'] - min(train[,'dist'])))
#splitting the data
install.packages("caret")
set.seed(1000)
trainindex = sample(1:nrow(train), 0.80 * nrow(train))
train_data = train[trainindex,]
test_data = train[-trainindex,]
#Applying the Multiple Linear Regression model
install.packages('caTools')
regressor = lm(fare_amount ~.,data=train_data)
summary(regressor)
plot(regressor$fitted.values,rstandard(regressor),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")
y_pred = predict(regressor, newdata =test_data)
install.packages("DMwR")
library(DMwR)
regr.eval(test_data[,1],y_pred)
#Decision Tree
install.packages("rpart")
library(rpart)
Dec = rpart(fare_amount ~ ., data = train_data, method = "anova")
summary(Dec)
#Predict for new test cases
y_pred = predict(Dec, test_data[,2:7])
install.packages("DMwR")
library(DMwR)
regr.eval(test_data[,1],y_pred)
#Random Forest
install.packages("randomForest")
library(randomForest)
rf_model = randomForest(fare_amount ~.,data=train_data)
summary(rf_model)
rf_predictions = predict(rf_model,test_data[,2:7])
regr.eval(test_data[,1],rf_predictions)
#XGBoosting
install.packages('xgboost')
library(xgboost)
train_data_matrix = as.matrix(sapply(train_data[-1],as.numeric))
test_data_data_matrix = as.matrix(sapply(test_data[-1],as.numeric))
xgboost_model = xgboost(data = train_data_matrix,label = train_data$fare_amount,nrounds = 15,verbose = FALSE)
summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)
install.packages("DMwR")
library(DMwR)
regr.eval(test_data[,1],xgb_predictions)
write.csv(rf_predictions,"predictedfareR.csv",row.names =FALSE)
