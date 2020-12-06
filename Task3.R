library(tidyverse)
library(caret)
library(dplyr)
library(matlib)

vectorProd <- function(X, Y) {
  if(length(X) != length(Y)) {
    stop("Dimensions do not match");
  }
  
  sum <- 0
  
  for(i in 1:length(X)) {
    sum <- sum + (X[i] * Y[i])
  }
  
  sum
}

matrixMultiply <- function(X, Y) {
  if(dim(X)[2] != dim(Y)[1]) {
    stop("Dimensions do not match");
  }
  
  finalDimRows = dim(X)[1]
  finalDimCols = dim(Y)[2]
  
  result <- matrix(nrow=finalDimRows, ncol=finalDimCols)
  
  for(i in 1:finalDimRows)
  {
    for(j in 1:finalDimCols)
    {
      result[i, j] = vectorProd(c(X[i,]), c(Y[,j]))
    }
  }
  
  result
}

vectorLength <- function(X) {
  sum <- 0
  for(value in X) {
    sum <- sum + value^2
  }
  
  sqrt(sum)
}

transpose <- function(X) {
  dimX <- dim(X)
  initialRows <- dimX[1]
  initialCols <- dimX[2]
  
  allValues <- c(X)

  matrix(allValues, nrow = initialCols, ncol = initialRows, byrow = 1)
}

identityMatrix <- function(length) {
  result <- matrix(0, nrow=length, ncol=length)
  
  for(i in 1:length) {
    result[i, i] = 1
  }
  
  result
}

## mean should be a vector/matrix of R points in column notation
## sigma is the covariance matrix between all R dimensions
## x is the point which should be determined through this function
multivariateGaussian <- function(mean, sigma, x) {
  invSigma <- inv(sigma)
  xMinusMean <- x-mean
  pointMeanSigmapointMean <- t(xMinusMean) %*% invSigma %*% xMinusMean
  (1/((2*pi)^(length(mean)/2)*det(sigma)^(1/2)))*exp(-(1/2)*pointMeanSigmapointMean)
}

# Missing: Multivariate gaussian
data(iris)
set.seed(101)
head(iris)
rows <- sample(nrow(iris))
max <- 10
packages <- NULL
packageSize = nrow(iris)/max

#Put data in max different packages
for(i in 1:max) {
  packageMin <- floor(packageSize * (i-1)) + 1
  packageMax <- floor(packageSize * i)
  packageIndices <- rows[packageMin:packageMax]
  packages <- append(packages, list(iris[packageIndices, ]))
}

errorValues <- list()

for(i in 1:max) {
  test <- packages[i][[1]]
  training <- packages[-i]
  training <- bind_rows(training, .id = "fromPackageId")
  
  classesOfIris <- unique(training$Species)
  covarianceMatrices <- list()
  centerPoints <- list()
  
  for(irisClass in classesOfIris) {
    trainingForThisClass <- subset(training, Species==irisClass)
    
    #make covariance matrix & center point for each class
    trainingForThisClass <- trainingForThisClass %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
    covarianceMatrices <- append(covarianceMatrices, list(cov(trainingForThisClass)))
    centerPoints <- append(centerPoints, list(apply(trainingForThisClass, MARGIN=2, FUN = mean)))
  }
  
  #make multivariate gaussian for each
  testInput <- test %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  inputCopy <- testInput
  testResult <- test %>% select(Species)
  
  testInput$Class1 <- apply(inputCopy, 1, function (oneRow){
    oneRow <- as.vector(unlist(oneRow))
    centerPoint <- as.vector(unlist(centerPoints[1], use.names=FALSE))
    multivariateGaussian(centerPoint, covarianceMatrices[[1]], oneRow)
  })
  testInput$Class2 <- apply(inputCopy, 1, function (oneRow){
    oneRow <- as.vector(unlist(oneRow))
    centerPoint <- as.vector(unlist(centerPoints[2], use.names=FALSE))
    multivariateGaussian(centerPoint, covarianceMatrices[[2]], oneRow)
  })
  testInput$Class3 <- apply(inputCopy, 1, function (oneRow){
    oneRow <- as.vector(unlist(oneRow))
    centerPoint <- as.vector(unlist(centerPoints[3], use.names=FALSE))
    multivariateGaussian(centerPoint, covarianceMatrices[[3]], oneRow)
  })
  
  testInput$result <- apply(testInput, 1, IrisClasses = classesOfIris, function(oneRow, IrisClasses){
    if(oneRow["Class1"] > oneRow["Class2"] && oneRow["Class1"] > oneRow["Class3"]) {
      IrisClasses[1]
    } else if(oneRow["Class2"] > oneRow["Class1"] && oneRow["Class2"] > oneRow["Class3"]) {
      IrisClasses[2]
    } else {
      IrisClasses[3]
    }
  })
  testInput$real <- testResult$Species
  total <- nrow(testInput)
  correct <- nrow(subset(testInput, result==real))
  errorValues <- c(errorValues, correct/total)
  print(testInput)
  print(correct/total)
}

print(mean(unlist(errorValues)))
