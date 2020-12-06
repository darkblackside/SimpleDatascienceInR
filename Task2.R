expectationValue <- function(X) {
  sum <- 0
  
  for(number in X) {
    sum <- sum + (number*(1/length(X)))
  }
  
  sum
}

covariancePopulation <- function(X, Y) {
  eX <- expectationValue(X)
  eY <- expectationValue(Y)
  expectationValue(X*Y)-eX*eY
}

variancePopulation <- function(X) {
  expectationValue(X^2)-expectationValue(X)^2
}

standardDeviationPopulation <- function(X) {
  sqrt(variancePopulation(X))
}

correlationPopulation <- function(X, Y) {
  cov(X, Y)/(standardDeviationPopulation(X)*standardDeviationPopulation(Y))
}

class1Values <- c(1, 1, 2, 2, 3, 3.5)
class2Values <- c(4, 5, 6, 5, 6.1, 5.4, 1)
class3Values <- c(10, 20, 22, 24)

xValues = seq(0, 40, by = .1)
gaussian1 <- dnorm(xValues, expectationValue(class1Values), variancePopulation(class1Values))
gaussian2 <- dnorm(xValues, expectationValue(class2Values), variancePopulation(class2Values))
gaussian3 <- dnorm(xValues, expectationValue(class3Values), variancePopulation(class3Values))

plot(xValues, gaussian1,type="l",col="red")
lines(xValues, gaussian2,col="green")
lines(xValues, gaussian3,col="blue")

#cluster new:
xValue = 9.4
points(xValue, 0)

valueForGaussian1 <-dnorm(xValue, expectationValue(class1Values), variancePopulation(class1Values))
valueForGaussian2 <-dnorm(xValue, expectationValue(class2Values), variancePopulation(class2Values))
valueForGaussian3 <-dnorm(xValue, expectationValue(class3Values), variancePopulation(class3Values))

if(valueForGaussian1 > valueForGaussian2 && valueForGaussian1 > valueForGaussian3) {
  print("Klasse: 1")
  points(xValue, valueForGaussian1, col="red")
}
if(valueForGaussian2 > valueForGaussian1 && valueForGaussian2 > valueForGaussian3) {
  print("Klasse: 2")
  points(xValue, valueForGaussian2, col="green")
}
if(valueForGaussian3 > valueForGaussian1 && valueForGaussian3 > valueForGaussian2) {
  print("Klasse: 3")
  points(xValue, valueForGaussian3, col="blue")
}