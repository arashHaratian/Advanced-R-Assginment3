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