setwd("C:/Everything Else/Blackhat/R/Titanic")
rawdata = read.csv("train.csv")
testdata = read.csv("test.csv")

#Clean the gosh-diggity-darn data
tdata = subset(rawdata, Age!="NA")

#Note to self, include automatic stepwise regression ability

#1. Logistic Analysis
logFit <- function(formula, traindata){
	glm.fit = glm(formula, family = binomial, data = traindata)
	return(glm.fit)
}

logConfusion <- function(fit){
	#Generate confusion matrix
	glm.probs = predict(glm.fit, traindata, type="response")
	glm.pred = rep("0", NROW(traindata))
	glm.pred[glm.probs > .5] = "1"
	return(table(glm.pred, traindata[["Survived"]]))
}

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
		err[i] <- currerr/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}

#2. Linear Discriminant Analysis
ldaFit <- function(formula, traindata){
	library(MASS)
	lda.fit = lda(formula, data = traindata)
	return(lda.fit)
}

ldaConfusion <- function(fit){
	#Generate confusion matrix
	lda.pred = predict(lda.fit, traindata)
	lda.class = lda.pred$class
	return(table(lda.class, traindata[["Survived"]]))
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
		err[i] <- currerr/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}

#3. Quadratic Discriminant Analysis
qdaFit <- function(formula, traindata){
	qda.fit = qda(formula, data = traindata)
	return(qda.fit)
}

qdaConfusion <- function(fit){
	#Generate confusion matrix
	qda.pred = predict(qda.fit, traindata)
	qda.class = qda.pred$class
	return(table(qda.class, traindata[["Survived"]]))	
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
		err[i] <- currerr/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}