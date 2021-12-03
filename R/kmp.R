#' match times of appearance of x in y
#' @param x a vector
#' @param y a vector
#' @return match the times of appearance of \code{x} in \code{y}
#' @examples
#' kmp(c(1,2),c(1,2,1,2,3))
#' kmp(c(1,1),c(1,1,1,1,1))
#' @export


kmp <- function(x, y) {
  if ((is.vector(x) == FALSE) || (is.vector(y) == FALSE)) {
    warning('x or y is not a vector')
  } else{
    j <- 0
    lenx <- length(x)
    N <- numeric(lenx)
    if (lenx != 1) {
      for (i in 2:lenx) {
        if (x[i] == x[j + 1]) {
          N[i] <- j + 1
          j <- j + 1
        } else{
          if (j > 0) {
            j <- N[j]
          }
        }
      }
    }
    k <- 0
    m <- 0
    n <- 1
    leny <- length(y)

    while (n <= leny) {
      if (m == lenx) {
        k <- k + 1
        m <- N[m]
      }
      if (x[m + 1] == y[n]) {
        m <- m + 1
        n <- n + 1
      } else if (m == 0) {
        n <- n + 1
      } else{
        m <- N[m]
      }
    }
    if (m == lenx) {
      k <- k + 1
    }
    return(k)
  }
}
