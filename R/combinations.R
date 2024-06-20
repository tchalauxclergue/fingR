#' Combination generator
#'
#' Generate combinations of elements in a vector
#'
#' @param x A character vector containing elements to combine.
#'
#' @return A character vector containing combinations of elements in 'x' separated by "-".
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
combinations <- function(x){
  n <- length(x)
  combs <- character(n * (n - 1) / 2) # Preallocate the 'combs' vector
  k <- 1
  for(i in 1:(n-1)){ # Iterate through each element in 'x' from the first to the second last
    for(j in (i+1):n){ # Iterate through each element in 'x' from the element next to 'i' till the last
      combs[k] <- paste(x[i], x[j], sep = "-") # Combine element i and element j with "-" separator
      k <- k+1
    }
  }
  return(combs)
}

