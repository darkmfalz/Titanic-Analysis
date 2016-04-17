setwd("C:/Everything Else/Blackhat/R/Titanic")
rawdata = read.csv("train.csv")
testdata = read.csv("test.csv")

#Clean the gosh-diggity-darn data
traindata = subset(rawdata, Age!="NA")

#Note to self, include automatic stepwise regression ability

#1. Logistic Analysis
glm.fit = glm(Survived~Pclass+Sex+Age+Fare, family = binomial, data = traindata)
summary(glm.fit)
#Generate confusion matrix
glm.probs = predict(glm.fit, traindata, type="response")
glm.pred = rep("0", NROW(traindata))
glm.pred[glm.probs > .5] = "1"
table(glm.pred, traindata[["Survived"]])

#Removing fare, because it has such a high p-value
glm.fit = glm(Survived~Pclass+Sex+Age, family = binomial, data = mydata)
summary(glm.fit)
#Generate confusion matrix
glm.probs = predict(glm.fit, traindata, type="response")
glm.pred = rep("0", NROW(traindata))
glm.pred[glm.probs > .5] = "1"
table(glm.pred, traindata[["Survived"]])

#2. Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Survived~Pclass+Sex+Age+Fare, data = traindata)
lda.fit
#Generate confusion matrix
lda.pred = predict(lda.fit, traindata)
lda.class = lda.pred$class
table(lda.class, traindata[["Survived"]])

#3. Quadratic Discriminant Analysis
qda.fit = qda(Survived~Pclass+Sex+Age+Fare, data = traindata)
qda.fit
#Generate confusion matrix
qda.pred = predict(lda.fit, traindata)
qda.class = qda.pred$class
table(qda.class, traindata[["Survived"]])