data(mtcars)
attach(mtcars)

m1 <- lm(wt ~ mpg + cyl,data=mtcars)
m2 <- mlr(model.matrix(wt ~ mpg + cyl), as.vector(wt))
sm1 <- summary(m1)

test_that("coefficients are accurate", {
  expect_equal(m2,sm1$coefficients)
})

match_vec <- function(x,y){
  m <- 0
  for(i in 1:c(length(y)-length(x)+1)) {
    if(identical(x,y[i: c(length(x)+i-1)])==TRUE){
      m<- m+1
    }
  }
  return(m)
}

test_that("time of appearance are correct", {
  expect_equal(kmp(1,rep(1,10)),match_vec(1,rep(1,10)))
})

test_that("time of appearance are correct", {
  expect_equal(kmp(c(1,2),rep(c(1,2),10)),match_vec(c(1,2),rep(c(1,2),10)))
})

