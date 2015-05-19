#' A function that sets any existing object's NA values to 0.
#' 
#' This function takes a variety of inputs and sets any of the input's
#' NA values to 0.
#' @param x a vector, matrix, data frame, a non-hierarchical list, 
#' or a list of no more than 2 hierarchy levels (i.e. a list of lists)
#' @return an object of the same type as x with any NA values set to 0.
#' @examples
#' na2zero(matrix(c(NA, 7, NA, 6), 2, 2))
#' na2zero(c(NA, 6))
#' na2zero(list(NA, 3))
#' na2zero(list(NA, 3, list(4, NA)))
#' 
#' mtcars[1,3] <- NA
#' na2zero(mtcars)

na2zero <- function(x) {
  if (is.list(x) & sum(sapply(y, is.list)) > 0) {
    # set any of list's NA elements to 0
    x[is.na(x)] <- 0 
    # set any of sublists' NA elements to 0
    for (i in length(x)) {
      if (is.list(x[[i]])) {
        x[[i]][is.na(x[[i]])] <- 0
      }
    }
  } else {
    x[is.na(x)] <- 0 
  }
  
  return(x)
}