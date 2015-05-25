#' A function that sets any existing object's NA values to a user specified
#' value (p).
#' 
#' This function takes a variety of inputs and sets any of the input's
#' NA values to a user-specified value (p).
#' @param x a vector, matrix, data frame, a non-hierarchical list, 
#' or a list of no more than 2 hierarchy levels (i.e. a list of lists)
#' @param p a value with which to replace the NA values. Defaults to 0.
#' @return an object of the same type as x with any NA values set to p.
#' @examples
#' fillna(matrix(c(NA, 7, NA, 6), 2, 2))
#' fillna(c(NA, 6))
#' fillna(list(NA, 3), -99)
#' fillna(list(NA, 3, list(4, NA)), -99)
#' 
#' mtcars[1,3] <- NA
#' fillna(mtcars, 0)

fillna <- function(x, p = 0) {
  if (is.list(x) & sum(sapply(x, is.list)) > 0) {
    # set any of list's NA elements to p
    x[is.na(x)] <- p
    # set any of sublists' NA elements to p
    for (i in length(x)) {
      if (is.list(x[[i]])) {
        x[[i]][is.na(x[[i]])] <- p
      }
    }
  } else {
    # for non-list objects, set NA elements to p
    x[is.na(x)] <- p
  }
  
  return(x)
}