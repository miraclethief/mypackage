#' Fit a multiple linear regression
#' @param x design matrix
#' @param y outcome
#' @return  the parameter estimates from a multiple regression model
#' @examples
#' data(mtcars)
#' attach(mtcars)
#' mlr(model.matrix(wt ~ mpg + cyl), as.vector(wt))
#' @export

mlr <- function(X, Y) {
  #input X(design matrix), Y(outcome)
  if (is.matrix(X) && is.vector(Y)) {
    n = nrow(X)
    p = ncol(X)
    # estimate betahat
    betahat = solve(t(X) %*% X) %*% t(X) %*% Y
    #estimate var(betahat)
    Yhat = X %*% betahat
    # residual
    epsilonhat = Y - Yhat
    # estimated sigma^2
    sigma_squared = t(epsilonhat) %*% epsilonhat / (n - p)
    # variance of betahat
    var_betahat = diag(solve(t(X) %*% X)) * c(sigma_squared)
    # standard error of betahat
    se_betahat = sqrt(var_betahat)
    # Inference: t statistic for H0: beta=0
    t_statistic = c(betahat / se_betahat)
    # Inference: corresponding p-value for H0: beta=0
    p_value = c(2 * (1 - pt(
      q = abs(t_statistic), df = n - p
    )))
    (
      result = cbind(
        'Estimate' = c(betahat),
        'Std. Error' = se_betahat,
        't value' = t_statistic,
        'Pr(>|t|)' = p_value
      )
    )
    return(result)
  } else{
    warning("wrong data type")
  }
}

#' Fit a multiple linear regression
#' @param x a vector
#' @param y a vector
#' @return  the parameter estimates from a multiple regression model
#' @examples
#' data(mtcars)
#' attach(mtcars)
#' Ftest(mpg,wt)
#' @export

Ftest <- function(X, Y) {
  Xbar = mean(X)
  Ybar = mean(Y)
  n = length(Y)
  SSXY = sum((Y - Ybar) * (X - Xbar))
  SSX = sum((X - Xbar) ^ 2)
  beta1hat = SSXY / SSX #beta1
  beta0hat = Ybar - beta1hat * Xbar #beta0
  Yhat = beta0hat + beta1hat * X
  SSE = sum((Y - Yhat) ^ 2)
  SSR = sum((Yhat - Ybar) ^ 2)
  MSE = SSE / (n - 2)
  MSR = SSR / 1
  Fstat = MSR / MSE # F statistic
  pvalue = 1 - pf(q = Fstat, df1 = 1, df2 = n - 2) # P-value
  R_squared = SSR / (SSR + SSE) # R-squared
  result =
    cbind(
      'Df' = 1,
      'Sum Sq' = SSR,
      'SSR' = SSR,
      'F value' = Fstat,
      'Pr(>F)' = pvalue,
      'R-squared' = R_squared,
      'SSE' = SSE,
      'MSE' = MSE
    )
  return(result)
}


#' Motivated by HW1, realize KMP algorithm to find times of appearance of x in y, especially for quite long vectors matching
#' @param x a vector
#' @param y a vector
#' @return times of appearance of \code{x} in \code{y}
#' @examples
#' kmp(c(1,2),c(1,2,1,2,3))
#' kmp(c(1,1),c(1,1,1,1,1))
#' @export

kmp <- function(x, y) {
  if ((is.vector(x) == FALSE) || (is.vector(y) == FALSE)) {
    warning('x or y is not a vector')
  } else{
    j = 0
    lenx = length(x)
    N = numeric(lenx)
    if (lenx != 1) {
      for (i in 2:lenx) {
        if (x[i] == x[j + 1]) {
          N[i] = j + 1
          j = j + 1
        } else{
          if (j > 0) {
            j = N[j]
          }
        }
      }
    }
    k = 0
    m = 0
    n = 1
    leny = length(y)

        while (n <= leny) {
      if (m == lenx) {
        k = k + 1
        m = N[m]
      }
      if (x[m + 1] == y[n]) {
        m = m + 1
        n = n + 1
      } else if (m == 0) {
        n = n + 1
      } else{
        m = N[m]
      }
    }
    if (m == lenx) {
      k = k + 1
    }
    return(k)
  }
}

