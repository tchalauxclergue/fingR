#' Combination generator
#'
#' Return the list of given type/class unique combinations.
#'
#' @param x a character vector of types/class.
#'
#' @return a character vector of combination.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
combinations <- function(x){
  combs <- c()
  for(i in 1:(length(x)-1)){ # from the 1st level to the last-1
    for(j in (i+1):length(x)){ # from the 2nd level to the last
      combs <- append(combs, paste(x[i], x[j], sep = "-")) # paste level i and level j (i.e A + B -> "A-B")
    }
  }
  return(combs)
}
