#' A function to count the number of unique values in a vector, matrix,
#' or dataframe.
#' 
#' This function takes in either a vector, matrix, or dataframe and outputs
#' the number of unique values either as a 1x1 vector (if input is vector) or
#' as a px1 vector (if input is a matrix or dataframewith p columns).
#' @param x a vector, matrix, or dataframe.
#' @return a 1x1 vector (if input is vector) or
#' as a px1 vector (if input is a matrix or dataframewith p columns).

num_unique_values <- function(x) {
  if (is.vector(x)) {
    return(length(unique(x)))
  } else {
    p <- dim(x)[2]
    result <- rep(NA, p)
    for (j in 1:p) {
      result[j] <- length(unique(x[,j]))
    }
    return(result)
  }
}