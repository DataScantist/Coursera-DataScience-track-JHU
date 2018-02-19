#question 1, classification trees
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- subset(segmentationOriginal,Case=="Train")
testing<- subset(segmentationOriginal,Case=="Test")
set.seed(125)

modFit  <- train(Class~.,method="rpart",data=training)
modFit
print(modFit$finalModel)
table(training$Class)

library(rattle)
fancyRpartPlot(modFit$finalModel)


#question 3, Classification trees(2)
dataset_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/data/olive_data.zip"
download.file(dataset_url,"olive_data.zip")
unzip("olive_data.zip", exdir = "olivedata")
load("olivedata/olive.rda")

olive = olive[,-1]
mean_olive = as.data.frame(t(colMeans(olive)))

    #with caret package
modFit2 <- train(Area~.,method="rpart",data=olive)
predict(modFit2,newdata=mean_olive)

    #with tree command. Result close but not exactly identical to modFit2 with caret package
library(tree)
modFit3 <- tree(Area~.,data=olive)
predict(modFit3,newdata=mean_olive)


#Question 4, glm and missclassifcation rate
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)

    #fitting with a logistic regression
modFit4 <- train(chd~ age + alcohol + obesity + tobacco + typea + ldl,method="glm",family="binomial",data=trainSA)

    #missclasification rate
        #creating the function
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
        #computing predicted values
predTrain <- predict(modFit4,newdata=trainSA)
predTest <- predict(modFit4,newdata=testSA)
        #executing the function
missClass(trainSA$chd,predTrain)
missClass(testSA$chd,predTest)

#Exercice 5, 
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

    #with Caret
modFit5 <- train(y~.,method="rf",data=vowel.train)
    #directly with randomForest, and with a measure of importance of the variables
library(randomForest)
modFit6 <- randomForest(y~.,data=vowel.train,importance=TRUE)
ls(modFit6)
print(modFit6$importance)

