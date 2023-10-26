#' Encompassed sample prediction
#'
#' Calculate the number of elements from s1 that lies within the s2 range.
#'
#' @param s1,s2 Two vectors containing numerical values
#'
#' @author Thomas Chalaux-Clergue
#'
#' @returns Numerical percentage of coverage.
ESP <- function(s1, s2){
  n <- 0
  for(i in s1){
    if(i >= min(s2) & max(s2) >= i){
      n <- n+1
    }
  }
  esp <- n/length(s1)*100
  return(esp)
}
