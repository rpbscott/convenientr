#' A function to make an n x p matrix of randomly generated integers with
#' minimal effort by the user.
#' 
#' This function takes in the dimensions of the desired matrix and outputs
#' a matrix whose elements are randomly generated integers. The 
#' dimensions are the only required input, but users can also control the
#' parameters of the discrete uniform used to generate the matrix. 
#' @param n the number of rows for the matrix
#' @param p the number of columns for the matrix
#' @param a the lower bound of the discrete uniform distribution used to 
#' simulate the matrix elements. Defaults to 0.
#' @param b the upper bound of the discrete uniform distribution used to 
#' simulate the matrix elements. Defaults to 2*p*n
#' @return a matrix if size n x p.


qmatrix <- function(n, p, a = 0, b = 2*p*n) {
  if (n*p > (b-a)) {
    elements <- sample(a:b, n*p, replace = TRUE)
  } else {
    elements <- sample(a:b, n*p, replace = FALSE)
  }
  result <- matrix(elements, n, p)
  return(result)
}

