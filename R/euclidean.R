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
  
  # We take the absolute values and sort the vector to get the smallest number
  ordered_vector = sort(c(abs(x), abs(y)), decreasing = FALSE)
  
  # We check if one of the numbers is 0
  if(all(ordered_vector==c(0,0))){
    return(0)
  } else if(0 %in% ordered_vector){
    return(ordered_vector[!ordered_vector==0])
  }
  
  # We apply the algorithm
  r0 <- ordered_vector[2]
  r1 <- ordered_vector[1]
  r2 <- 1  # Initializing r2
  while(r2 != 0){
    r2 <- r0 %% r1
    r0 <- r1
    r1 <- r2
  }
  return(r0)
}