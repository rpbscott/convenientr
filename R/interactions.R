#' A function that returns all pairwise interations between the input's 
#' columns
#' 
#' This function takes in a matrix or dataframe and returns a data structure
#' of the same type that includes the original columns concatenated with all
#' pairwise interactions.
#' @param x a matrix or dataframe with at least two columns
#' @return an object of the same type as x with {p choose 2} additional
#' columns representing the pairwise interactions that are concatenated to the 
#' original object x.
#' @examples
#' interactions(mtcars)
interactions <- function(x) {
  # check dimensions
  if (dim(x)[2] < 2) {
    stop("In order to calculate pairwise interactions between columns, you 
         need x to have 2 or more columns. You provided a vector or a list.")
  } else {
    inter <- list()
    p <- dim(x)[2]
    for (j in 1:(p-1)) {
      inter[[j]] <- as.data.frame(x[,j]*x[,(j + 1):p])
      names(inter[[j]]) <- stringi::stri_c("inter_",
                                           names(x)[j], 
                                           '_',
                                           names(x)[(j+1):p], 
                                           sep = "")
    }
  # combine interactions and original data structure
  inter_matrix <- do.call(cbind, inter)
  result <- cbind(x, inter_matrix)
    
  # with sparse input, interactions are likely to be very sparse
  # send warnings if any interactions return a vector of constants
  if (sum(convenientr::num_unique_values(result) == 1)) {
      warning(cat("One of the columns of returned object is constant (has",
              "only 1 unique value.)"))
    }
  return(result)
  }
}

