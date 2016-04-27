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

logConfusion <- function(fit, threshold){
	#Generate confusion matrix
	glm.probs = predict(glm.fit, traindata, type="response")
	glm.pred = rep("0", NROW(traindata))
	glm.pred[glm.probs > threshold] = "1"
	return(table(glm.pred, traindata[["Survived"]]))
}

logCV <- function(formula, tempdata, k, threshold = 0.5){
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
		glm.pred[glm.probs > threshold] = "1"
		currtable <- table(glm.pred, data.frame( folds[[i]] )[["Survived"]])
		currtable
		currerr <- currtable[1,2] + currtable[2,1]
		err[i] <- currerr/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}

logThreshold <- function(formula, tempdata, k){
	tries <- 20
	threshold <- 0.5
	for(i in 1:20){
		val <- 0
		valDeltaUpper <- 0
		valDeltaLower <- 0
		for(j in 1:10){
			val <- val + logCV(formula, tempdata, k, threshold)[1]
			valDeltaUpper <- valDeltaUpper + logCV(formula, tempdata, k, threshold + 0.01)[1]
			valDeltaLower <- valDeltaLower + logCV(formula, tempdata, k, threshold - 0.01)[1]
		}
		val <- val / 10
		valDeltaUpper <- valDeltaUpper / 10
		valDeltaLower <- valDeltaLower / 10
		print(threshold)
		print(val)
		threshold <- threshold - ((valDeltaUpper - valDeltaLower)/0.02)/(((valDeltaUpper - val)/0.01 - (val - valDeltaLower)/0.01)/0.01)
	}
	return(threshold)
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

#4. K-Nearest Neighbors

#Alter variables inside method
knnFit(tempdata, k){

	folds <- split(tempdata, NROW(tempdata)/2)
	train.X = cbind(data.frame(folds[[1]])[["Sex"]], data.frame(folds[[1]])[["Pclass"]], data.frame(folds[[1]])[["Age"]], data.frame(folds[[1]])[["SibSp"]])
	test.X = cbind(data.frame(folds[[2]])[["Sex"]], data.frame(folds[[2]])[["Pclass"]], data.frame(folds[[2]])[["Age"]], data.frame(folds[[2]])[["SibSp"]])
	train.Survived = data.frame(folds[[1]])[["Survived"]]
	knn.pred = knn(train.X, test.X, train.Survived, k)
	table(knn.pred, data.frame(folds[[2]])[["Survived"]])

}

#5. Boosting Classification Trees
boostCV <- function(tempdata, k, threshold = 0){
	library(gbm)
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
		boost.fit = gbm(Survived~., data = currdata, distribution="bernoulli", n.trees=5000, interaction.depth=4)
		#Get the confusion matrix
		boost.probs = predict(boost.fit, data.frame( folds[[i]] ), n.trees=5000)
		boost.pred = rep("0", NROW( data.frame(folds[[i]]) ))
		boost.pred[boost.probs > threshold] = "1"
		currtable <- table(boost.pred, data.frame( folds[[i]] )[["Survived"]])
		currtable
		currerr <- currtable[1,2] + currtable[2,1]
		err[i] <- currerr/NROW(data.frame(folds[[i]]))
	}
	summary <- vector(mode = "numeric", length = 2)
	summary[1] <- mean(err)
	summary[2] <- var(err)
	return(summary)
}
