setwd("C:/Everything Else/Blackhat/R/Titanic")
rawdata = read.csv("train.csv")
testdata = read.csv("test.csv")

#Clean the gosh-diggity-darn data
traindata = subset(rawdata, Age!="NA")

#k-Fold Cross-Validation Functions
logCV <- function(formula, tempdata, k){
	set.seed(17)
	folds <- split(tempdata, sample(rep(1:k, NROW(tempdata)/k)))
	err <- 0
	for(i in 1:k){
		#Make the dataset for the current logistic regression
		int <- ifelse(i != 1, 1, 2)
		currdata <- data.frame(folds[[int]])
		for(j in 1:k){
			if((j != i) && (j != int)){
				currdata <- rbind(currdata, data.frame( folds[[j]] ))
			}
		}
		#Fit the model to the data
		glm.fit = glm(formula, family = binomial, data = currdata)
		#Get the confusion matrix
		glm.probs = predict(glm.fit, data.frame( folds[[i]] ), type="response")
		glm.pred = rep("0", NROW( data.frame(folds[[i]]) ))
		glm.pred[glm.probs > .5] = "1"
		currtable <- table(glm.pred, data.frame( folds[[i]] )[["Survived"]])
		currerr <- currtable[1,2] + currtable[2,1]
		err <- err + currerr
	}
	err = err / k
	return(err)
}

#Note to self, include automatic stepwise regression ability

#1. Logistic Analysis
glm.fit = glm(Survived~Pclass+Sex+Age+Fare, family = binomial, data = traindata)
summary(glm.fit)
#Generate confusion matrix
glm.probs = predict(glm.fit, traindata, type="response")
glm.pred = rep("0", NROW(traindata))
glm.pred[glm.probs > .5] = "1"
table(glm.pred, traindata[["Survived"]])
#Some k-fold cross-validation stuff

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