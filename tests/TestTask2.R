source("../Task2.R", chdir = TRUE)
library(testthat)

test_that("expectation value", {
  for(i in 1:10) {
    randomNumbers1 <- rpois(n = 50, lambda = 10)
    randomNumbers2 <- rpois(n = 50, lambda = 10)
    
    expRand1 <- expectationValue(randomNumbers1)
    meanRand1 <- mean(randomNumbers1)
    
    expect_equal(expRand1, meanRand1)
  }
})

test_that("covariance", {
  randomNumbers1 <- rpois(n = 50, lambda = 10)
  randomNumbers2 <- rpois(n = 50, lambda = 10)
  
  covSelf <- covariancePopulation(randomNumbers1, randomNumbers2)
  covFramework <- cov(randomNumbers1, randomNumbers2)
  
  expect_equal(covSelf, ((49)/50) * covFramework)
})

test_that("standard deviation", {
  randomNumbers1 <- rpois(n = 50, lambda = 10)
  
  stdSelf <- standardDeviationPopulation(randomNumbers1)
  stdFramework <- sd(randomNumbers1)
  
  expect_equal(stdSelf, sqrt((49)/50) * stdFramework)
})

test_that("correlation", {
  randomNumbers1 <- rpois(n = 50, lambda = 10)
  randomNumbers2 <- rpois(n = 50, lambda = 10)
  
  corrSelf <- correlationPopulation(randomNumbers1, randomNumbers2)
  corrFramework <- cor(randomNumbers1, randomNumbers2)

  expect_equal(corrSelf, ((50)/49) * corrFramework)
})