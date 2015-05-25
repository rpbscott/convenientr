#' A function to print the run time of its input function call and save the 
#' results
#' 
#' This function takes a function call, prints the time taken to run the
#' function call, and returns the result of the function call.
#' @param FUN call to a function
#' @return the result of the inputted function call while printing the 
#' elapsed time taken to call function.
#' @examples
#' my_matrix <- matrix(rnorm(10000), 100, 100)
#' eigen_results <- time_it(eigen(my_matrix))


time_it <- function(FUN) {
  t1 <- proc.time()
  result <- FUN
  t2 <- proc.time()
  cat('function ran in ', t2[3] - t1[3], 'seconds')
  return(result)
}
