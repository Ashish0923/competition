library(caret)
library(pROC)

grid <- expand.grid(nrounds = seq(1, 201, by = 25),max_depth = 1:6,eta = (1:4)/10)

set.seed(1)
reg1 <- SLC14_1(200)
reg2 <- SLC14_1(200)

set.seed(2)
mod1 <- train(y ~ ., data = reg1,method = "xgbTree",tuneGrid = grid,trControl = trainControl(method = "cv"))
postResample(predict(mod1, reg2), reg2$y)


set.seed(3)
class1 <- twoClassSim(200)
class2 <- twoClassSim(200)

set.seed(4)
mod2 <- train(Class ~ ., data = class1,method = "xgbTree",tuneGrid = grid,trControl = trainControl(method = "cv"))
confusionMatrix(predict(mod2, class2), class2$Class)

set.seed(4)
mod3 <- train(Class ~ ., data = class1, 
              method = modelInfo,
              tuneGrid = grid,
              metric = "ROC",
              trControl = trainControl(method = "cv", 
                                       summaryFunction = twoClassSummary,
                                       classProbs = TRUE))
probs <- predict(mod3, class2, type = "prob")
roc(class2$Class, probs[, "Class1"], levels = rev(levels(class2$Class)))