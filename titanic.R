library(randomForest)
library(rpart)

#Getting Data
train<-read.csv("C:\\Users\\ASHISH\\Downloads\\comptn\\titanic\\train.csv")
test<-read.csv("C:\\Users\\ASHISH\\Downloads\\comptn\\titanic\\test.csv")

#Data processing
data<-train[,-2]
Survived<-train[,2]
data<-rbind(data,test)
Index<-nrow(train)

#Feature Engineering
Title<-t(t(strapplyc(as.character(data$Name), ",([^.]+)")))            #Creating new column Title
Title<- data.frame(matrix(unlist(Title)))
names(Title)<-"Title"
data<-cbind(data,Title)
rm(Title)
data$Fare[which(is.na(data$Fare))]=median(data$Fare,na.rm=TRUE)        #Putting the median value for NA in Fare
data$Family_size<-data$SibSp+data$Parch+1                              #Adding new column Family_size ; +1 for himself/herself
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Family_size,
                       data = data[!is.na(data$Age),], method = "anova")
data$Age[is.na(data$Age)] <- predict(predicted_age, data[is.na(data$Age),])#Predicting age for NA value(sum(is.na(data$Age))>0 then following will be done)
train<-data[1:nrow(train),]
test<-data[Index+1:nrow(test),]
train<-cbind(train,Survived)

#Applying Random Forest
forest<-randomForest(as.factor(Survived)~Pclass + Sex+Age + SibSp + Parch + Fare + Embarked + Title + Family_size,
                         data=train,
                         ntree=1000,
                         importance=TRUE)
prediction<-predict(forest,test)
solution<-data.frame(PassengerId=test$PassengerId,Survived=prediction)

#Writing CSV file
write.csv(solution,file="C:\\Users\\ASHISH\\Downloads\\comptn\\titanic\\Random_forest_1000.csv",row.names=FALSE)