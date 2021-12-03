#' Fit a multiple linear regression
#' @param x a vector
#' @param y a vector
#' @return  the parameter estimates from a multiple regression model
#' @examples
#' import data
#' Ftest(X,Y)
#' @export

Ftest <- function(X, Y) {
  Xbar = mean(X)
  Ybar = mean(Y)
  n = length(Y)
  SSXY = sum((Y - Ybar) * (X - Xbar))
  SSX = sum((X-Xbar)^2)
  beta1hat = SSXY / SSX #beta1
  #as.numeric(coef(m)["estriol"]) #beta1 from lm
  beta0hat = Ybar - beta1hat * Xbar #beta0
  Yhat = beta0hat+beta1hat*X
  SSE = sum((Y - Yhat) ^ 2)
  SSR = sum((Yhat - Ybar) ^ 2)
  MSE = SSE / (n - 2)
  MSR = SSR / 1 (Fstat = MSR / MSE)
  p-value = 1 - pf(q = Fstat, df1 = 1, df2 = n - 2) ## P-value
  Fstat = MSR / MSE ## F statistic
  1 - pf(q = Fstat, df1 = 1, df2 = n - 2) ## P-value
  R_squared = SSR / (SSR + SSE) ## R-squared
  result <- cbind('Df Sum Sq'=SSR, 'Mean Sq'=SSR, 'F value'=Fstat, 'Pr(>F)'=p-value, 'R-squared'=R_squared)
  return(result)
  }
