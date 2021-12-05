data(mtcars)
attach(mtcars)

m1 <- lm(wt ~ mpg + cyl, data = mtcars)
m2 <- mlr(model.matrix(wt ~ mpg + cyl), as.vector(wt))
sm1 <- summary(m1)

test_that("coefficients are accurate", {
  expect_equal(m2, sm1$coefficients)
})


SeqSS = data.frame(anova(lm(wt ~ mpg)))
Ftest1 <- Ftest(mpg, wt)
m5<-summary(lm(wt~mpg))
Ftest2 <-
  cbind(SeqSS["mpg", "Sum.Sq"], SeqSS["mpg", "Mean.Sq"], SeqSS["mpg", "F.value"],
        SeqSS["mpg", "Pr..F."], m5$r.squared, SeqSS["Residuals", "Sum.Sq"], SeqSS["Residuals", "Mean.Sq"])

test_that("Ftest are the same", {
  expect_equal(Ftest1[1:7], Ftest2[1:7])
})

match_vec <- function(x, y) {
  m <- 0
  for (i in 1:c(length(y) - length(x) + 1)) {
    if (identical(x, y[i:c(length(x) + i - 1)]) == TRUE) {
      m <- m + 1
    }
  }
  return(m)
}

test_that("time of appearance are correct", {
  expect_equal(kmp(1, rep(1, 10)), match_vec(1, rep(1, 10)))
})

test_that("time of appearance are correct", {
  expect_equal(kmp(c(1, 2), rep(c(1, 2), 1000)), match_vec(c(1, 2), rep(c(1, 2), 1000)))
})

detach(mtcars)
