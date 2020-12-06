source("../Task3.R", chdir = TRUE)
library(testthat)
library(geometry)

test_that("vector multiplication (dot product)", {
  randomNumbers1 <- rpois(n = 25, lambda = 10)
  randomNumbers2 <- rpois(n = 25, lambda = 10)
  
  result1 <- vectorProd(randomNumbers1, randomNumbers2)

  expect_equal(result1, dot(randomNumbers1, randomNumbers2))
})

test_that("transpose", {
  randomNumbers1 <- rpois(n = 24, lambda = 10)
  randomNumbers2 <- rpois(n = 15, lambda = 10)
  
  matrixA <- matrix(randomNumbers1, nrow=6, ncol=4);
  matrixB <- matrix(randomNumbers1, nrow=4, ncol=6);
  matrixC <- matrix(randomNumbers2, nrow=5, ncol=3);
  matrixD <- matrix(randomNumbers2, nrow=3, ncol=5);
  
  expect_equal(transpose(matrixA), t(matrixA))
  expect_equal(transpose(matrixB), t(matrixB))
  expect_equal(transpose(matrixC), t(matrixC))
  expect_equal(transpose(matrixD), t(matrixD))
})

test_that("matrix multiplication", {
  randomNumbers1 <- rpois(n = 24, lambda = 10)
  randomNumbers2 <- rpois(n = 24, lambda = 10)
  
  matrixA <- matrix(randomNumbers1, nrow=6, ncol=4);
  matrixB <- matrix(randomNumbers1, nrow=4, ncol=6);
  matrixC <- matrix(randomNumbers2, nrow=6, ncol=4);
  matrixD <- matrix(randomNumbers2, nrow=4, ncol=6);
  
  result1 <- matrixMultiply(matrixA, matrixB)
  expect_equal(result1, matrixA %*% matrixB)
  
  result2 <- matrixMultiply(matrixA, matrixD)
  expect_equal(result2, matrixA %*% matrixD)
  
  result3 <- matrixMultiply(matrixB, matrixA)
  expect_equal(result3, matrixB %*% matrixA)
  
  result4 <- matrixMultiply(matrixB, matrixC)
  expect_equal(result4, matrixB %*% matrixC)
  
  result5 <- matrixMultiply(matrixC, matrixB)
  expect_equal(result5, matrixC %*% matrixB)
  
  result6 <- matrixMultiply(matrixC, matrixD)
  expect_equal(result6, matrixC %*% matrixD)
  
  result7 <- matrixMultiply(matrixD, matrixA)
  expect_equal(result7, matrixD %*% matrixA)
  
  result8 <- matrixMultiply(matrixD, matrixC)
  expect_equal(result8, matrixD %*% matrixC)
})

test_that("vector length", {
  #transpose(x)*x = x*x = ||x||^2 for vectors
  
  randomNumbers <- rpois(n = 50, lambda = 10)
  
  expect_equal(vectorProd(randomNumbers, randomNumbers), vectorLength(randomNumbers)^2)
})

test_that("identity matrix", {
  for(i in 1:100) {
    identityM <- identityMatrix(i)
    expect_equal(identityM, diag(i))
  }
})

test_that("identity matrix", {
  for(i in 1:100) {
    randomNumbers <- matrix(rpois(n = 50, lambda = 10), ncol=1)
    identityMatrix <- identityMatrix(50)
    expect_equal(randomNumbers, matrixMultiply(identityMatrix, randomNumbers))
  }
})