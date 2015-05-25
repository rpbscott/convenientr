#' A function to make a p x 1 vector of randomly generated integers with
#' minimal effort by the user.
#' 
#' This function takes in the length p of the desired vector and outputs
#' a vector whose elements are randomly generated integers. The 
#' length is the only required input, but users can also control the
#' parameters of the discrete uniform used to generate the vector 
#' @param p the number of elements of the vector
#' @param a the lower bound of the discrete uniform distribution used to 
#' simulate the matrix elements. Defaults to 0.
#' @param b the upper bound of the discrete uniform distribution used to 
#' simulate the matrix elements. Defaults to 2*p*n
#' @return a vector if length p x 1.

qvec <- function(p, a = 0, b = 2*p) {
  if (p > (b-a)) {
    result <- sample(a:b, p, replace = TRUE)
  } else {
    result <- sample(a:b, p, replace = FALSE)
  }
  return(result)
}

