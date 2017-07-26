library(caret)
library(data.table)
require(xgboost)


#Getting Data
train_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\comptn\\Black Friday\\train.csv")
test_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\comptn\\Black Friday\\test.csv")

Purchase<-train_data$Purchase
col<-ncol(train_data)-1
mod_data<-train_data[,1:col]

mod_data<-rbind(mod_data,test_data)
Index1 <- nrow(train_data)

mod_data$Age<-as.integer(mod_data$Age)
mod_data$Gender<-as.integer(mod_data$Gender)
mod_data$Product_ID<-as.integer(mod_data$Product_ID)
mod_data$City_Category<-as.integer(mod_data$City_Category)
mod_data$Stay_In_Current_City_Years<-as.integer(mod_data$Stay_In_Current_City_Years)

mod_data[is.na(mod_data)]<-0
mod_train<-mod_data[1:Index1,]
mod_test<-mod_data[(Index1+1):nrow(mod_data),]
mod_train<-cbind(mod_train,Purchase)

gridSearch <- expand.grid(nrounds = 100,
                          max_depth =4,
                          eta = 0.3)

fitControl<-trainControl(method="cv",
                         number=5)


xg_boost <- train(Purchase ~ ., data = mod_train,
                  method = "xgbTree",
                  tuneGrid=gridSearch,
                  trControl = fitControl,
                  verbose = TRUE)


Purchase<-predict(xg_boost,mod_test)
prediction<-test_data[,1:2]
prediction<-as.data.frame(cbind(prediction,Purchase))

#Writing csv file
write.csv(prediction,file="C:\\Users\\ASHISH\\Downloads\\comptn\\Black Friday\\first_1.csv")
