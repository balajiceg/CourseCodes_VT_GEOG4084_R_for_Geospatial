#17092019
irisdata <- read.csv('iris.csv', header=TRUE) 

#split data
install.packages('caret')
install.packages('e1071')
library(caret)
library(e1071)

sample <- createDataPartition(irisdata$species, p=0.80, list=FALSE)
iris_train<-irisdata[sample,]
##this is a cool way of seperating
iris_test<-irisdata[-sample,]

control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'
fit.cart <- train(species~., data=iris_train, method='rpart',
                  trControl=control, metric=metric)
iris_prediction <- predict(fit.cart, iris_test)
confusionMatrix(iris_prediction, iris_test$species)

plot(fit.cart$finalModel)
text(fit.cart$finalModel)
