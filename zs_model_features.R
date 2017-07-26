library(caret)
library(doParallel)
library(data.table)
require(xgboost)
registerDoParallel(cores=2)

#Getting Data
train_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\zs\\ZSTrainingDataset.csv")
test_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\zs\\ZSEvaluationDataset.csv")

#Processing Datasets
Result<-train_data$Result
mod_data<-rbind(train_data,test_data)


#Date Modification
mod_data$Date.of.First.Contact<-as.Date(mod_data$Date.of.First.Contact,"%d-%m-%Y")
mod_data$Date.of.Giving.Proposal<-as.Date(mod_data$Date.of.Giving.Proposal,"%d-%m-%Y")
mod_data$Date.of.Closing<-as.Date(mod_data$Date.of.Closing,"%d-%m-%Y")

#Building Features

#Creating dummy
mod_data$Opportunity.Number<-NULL
dummy_var<-dummyVars(Result~.,data=mod_data)
mod_data<-predict(dummy_var,newdata=mod_data)


index<-nrow(train_data)
mod_train<-as.data.frame(mod_data[1:index,])
mod_test<-as.data.frame(mod_data[(index+1):nrow(mod_data),])
mod_train<-cbind(mod_train,Result)


fun <- function(data,lev = levels(data$obs),model = NULL){
  pred <- as.numeric(data$pred)-1
  obs <- as.numeric(data$obs)-1
  pr <- sum(pred & obs)/sum(pred)
  re <- sum(pred & obs)/sum(obs)
  F1 <- 2*pr*re/(pr+re)
  out <- c(pr,re,F1)
  names(out)<- c("p","r","F1")
  out
}
fitControl <- trainControl(method = "cv", number = 5,
                           summaryFunction =fun,
                           adaptive = list(min = 5,
                                           alpha = 0.05,
                                           method = "gls",
                                           complete = FALSE))
gridSearch <- expand.grid(nrounds = 100*c(1:5),
                          max_depth = c(1:3),
                          eta = c(0.1,0.05))
boost1 <- train(Result ~ ., data = mod_train,
                method = "xgbTree",
                tuneGrid = gridSearch,
                trControl = fitControl,
                verbose = TRUE,
                metric = "F1",
                maximize = TRUE)

xgboost_impvar<-varImp(boost1,scale=FALSE)

pred<-predict(boost1,mod_test)
prediction<-as.data.frame(mod_test[mod_test$Opportunity.Number])
prediction<-as.data.frame(cbind(test_data$Opportunity.Number,pred))
names(prediction)<-c("Opportunity Number","Result")

#Logical to Boolean
fun<-function(y)
{
  if(y==2)
    return(1)
  else
    return(0)
}
prediction$Result<-sapply(prediction$Result,fun)

#Writing csv file
write.csv(prediction,file="C:\\Users\\ASHISH\\Downloads\\zs\\first.csv")