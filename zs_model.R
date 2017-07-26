library(caret)
library(doParallel)
library(data.table)
registerDoParallel(cores=2)

#Getting Data
train_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\zs\\ZSTrainingDataset.csv")
test_data<-read.csv("C:\\Users\\ASHISH\\Downloads\\zs\\ZSEvaluationDataset.csv")

#Processing Datasets
Result<-train_data$Result
mod_data<-rbind(test_data,train_data)
mod_data$Opportunity.Number<-NULL

#Date Modification
mod_data$Date.of.First.Contact<-as.Date(mod_data$Date.of.First.Contact,"%d-%m-%Y")
mod_data$Date.of.Giving.Proposal<-as.Date(mod_data$Date.of.Giving.Proposal,"%d-%m-%Y")
mod_data$Date.of.Closing<-as.Date(mod_data$Date.of.Closing,"%d-%m-%Y")

#Creating dummy
dummy_var<-dummyVars(Result~.,data=mod_data)
mod_data<-predict(dummy_var,newdata=mod_data)


index<-nrow(train_data)
mod_train<-as.data.frame(mod_data[1:index,])
mod_test<-as.data.frame(mod_data[(index+1):nrow(mod_data),])
mod_train<-cbind(mod_train,Result)


#Model tuning parameters
fitcontrol<- trainControl(method="cv",number=3)
tune_Grid <- expand.grid(nrounds=150,max_depth=3,eta=0.3)

#Model fitting
xgboost<-train(Result~.,data=mod_train,method="xgbTree",trControl=fitcontrol,verbose=FALSE,tuneGrid=tune_Grid)
pred<-predict(xgboost,mod_test)
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
