setwd("C:/Everything Else/Blackhat/R/Titanic")
rawdata = read.csv("train.csv")
testdata = read.csv("test.csv")

#Clean the gosh-diggity-darn data
traindata = subset(rawdata, Age!="NA")

#k-Fold Cross-Validation Functions
logCV <- function(formula, tempdata, k){
	#set.seed(17)
	folds <- split(tempdata, sample(rep(1:k, NROW(tempdata)/k)))
	err <- vector(mode = "numeric", length = k)
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
		err[i] <- currerr*NROW(tempdata)/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}

ldaCV <- function(formula, tempdata, k){
	#set.seed(17)
	folds <- split(tempdata, sample(rep(1:k, NROW(tempdata)/k)))
	err <- vector(mode = "numeric", length = k)
	for(i in 1:k){
		#Make the dataset for the current logistic regression
		int <- ifelse(i != 1, 1, 2)
		currdata <- data.frame(folds[[int]])
		for(j in 1:k){
			if((j != i) && (j != int)){
				currdata <- rbind(currdata, data.frame( folds[[j]] ))
			}
		}
		library(MASS)
		lda.fit = lda(formula, data = currdata)
		#Generate confusion matrix
		lda.pred = predict(lda.fit, data.frame(folds[[i]]))
		lda.class = lda.pred$class
		currtable <- table(lda.class, data.frame( folds[[i]] )[["Survived"]])
		currerr <- currtable[1,2] + currtable[2,1]
		err[i] <- currerr*NROW(tempdata)/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}

qdaCV <- function(formula, tempdata, k){
	#set.seed(17)
	folds <- split(tempdata, sample(rep(1:k, NROW(tempdata)/k)))
	err <- vector(mode = "numeric", length = k)
	for(i in 1:k){
		#Make the dataset for the current logistic regression
		int <- ifelse(i != 1, 1, 2)
		currdata <- data.frame(folds[[int]])
		for(j in 1:k){
			if((j != i) && (j != int)){
				currdata <- rbind(currdata, data.frame( folds[[j]] ))
			}
		}
		library(MASS)
		qda.fit = qda(formula, data = currdata)
		#Generate confusion matrix
		qda.pred = predict(qda.fit, data.frame(folds[[i]]))
		qda.class = qda.pred$class
		currtable <- table(qda.class, data.frame( folds[[i]] )[["Survived"]])
		currerr <- currtable[1,2] + currtable[2,1]
		err[i] <- currerr*NROW(tempdata)/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
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
logCV(Survived~Pclass+Sex+Age+Fare, traindata, 10)

#Removing fare, because it has such a high p-value
glm.fit = glm(Survived~Pclass+Sex+Age, family = binomial, data = mydata)
summary(glm.fit)
#Generate confusion matrix
glm.probs = predict(glm.fit, traindata, type="response")
glm.pred = rep("0", NROW(traindata))
glm.pred[glm.probs > .5] = "1"
table(glm.pred, traindata[["Survived"]])
#Some k-fold cross-validation stuff
logCV(Survived~Pclass+Sex+Age, traindata, 10)

#2. Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Survived~Pclass+Sex+Age, data = traindata)
lda.fit
#Generate confusion matrix
lda.pred = predict(lda.fit, traindata)
lda.class = lda.pred$class
table(lda.class, traindata[["Survived"]])
#Some k-fold cross-validation stuff
ldaCV(Survived~Pclass+Sex+Age, traindata, 10)

#3. Quadratic Discriminant Analysis
qda.fit = qda(Survived~Pclass+Sex+Age, data = traindata)
qda.fit
#Generate confusion matrix
qda.pred = predict(lda.fit, traindata)
qda.class = qda.pred$class
table(qda.class, traindata[["Survived"]])
#Some k-fold cross-validation stuff
qdaCV(Survived~Pclass+Sex+Age, traindata, 10)