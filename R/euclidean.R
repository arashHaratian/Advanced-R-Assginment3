#' Euclidean Algorithm
#'
#' This function calculates the greatest common divisor for the two given integer numbers
#' (or numeric double value with no decimals).
#' 
#' By consensus we considerate 0 to be the greatest common divisor of `x=0`, `y=0`
#' 
#' @param x is an integer or a numeric double value with no decimals 
#' @param y is an integer or a numeric double value with no decimals 
#'
#' @return Function returns a numeric double value without decimals which corresponds to the greatest common divisor
#' @section References: For more information you can read: 
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean Algorithm}
#' @export
#'
#' @examples
#' euclidean(120,360) # = 120
#' euclidean(79,3) # = 1
#' euclidean(-8,36) # = 4
#' euclidean(85,0) # = 85
#' euclidean(-5L,20) # = 5
euclidean <- function(x,y){
  # We first check whether the arguments are integer

  stopifnot(is.vector(x=x,mode="numeric"), length(x)==1, x %% 1 == 0, 
            is.vector(x=y,mode="numeric"), length(y)==1, y %% 1 == 0)

  # We sort the vector to get the smallest number
  ordered_vector = sort(c(x, y), decreasing = FALSE)
  if(all(ordered_vector==c(0,0))){
    return(0)
  } else if(0 %in% ordered_vector){
    return(abs(ordered_vector[!ordered_vector==0]))
  }

  if ((ordered_vector[2] %% ordered_vector[1]) == 0) {
    return(abs(ordered_vector[1]))
  } else{
    for (i in 2:ordered_vector[1]) {
      # We check if the divisors of the smallest number are also divisor of the greatest,
      # starting with the greatest divisor
      if ((ordered_vector[1] %% i) == 0) {
        if ((ordered_vector[2] %% (ordered_vector[1] / i)) == 0) {
          return(abs(ordered_vector[1] / i))
        }
      }
    }
    return("There are no common divisors")
  }
}