install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
data(concrete)

install.packages("labeling")
library(labeling)

data(concrete)
set.seed(1000)


set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


list <- grep("IL_",names(training),value=TRUE)
list <- c(list,"diagnosis")
list


           
test22 <- training[,list]

#PCA
preproc <- preProcess(training[,list], method="pca",thresh = 0.80)
trainPC <- predict (preproc,training[,list])
modelfit <-  train(diagnosis ~ .,method="glm",data=trainPC)

testPC <- predict(preproc,testing[,list])
confusionMatrix(testing$diagnosis,predict(modelfit,testPC))

#Multi covariate without PCA
modfitall <- train(diagnosis ~ .,data=training[,list],method="glm")
confusionMatrix(testing$diagnosis,predict(modfitall,testing[,list]))




