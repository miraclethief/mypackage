#' Fit a multiple linear regression
#' @param x design matrix
#' @param y outcome
#' @return  the parameter estimates from a multiple regression model
#' @examples
#' import data
#' mlr(X,Y)
#' @export

mlr <- function(X, Y){
  #input X(design matrix), Y(outcome)
  if (is.matrix(X) == FALSE || is.vector(Y) == FALSE) {
    warning("X is not a matrix or Y is not a vector")
  } else{
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
      Results = cbind(
        Estimate = c(betahat),
        Std_Err = se_betahat,
        t_statistic = t_statistic,
        p_value = p_value
      )
    )
    return(Results)
  }
}
