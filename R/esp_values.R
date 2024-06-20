#' Encompassed sample predictions
#'
#' Calculate the number of elements from prediction (pred) that lies within the observed (obs) range.
#'
#' @param obs,pred Two vectors containing numerical values.
#' @param count Boolean. Return percentage when FALSE (default) and count when TRUE.
#' @param digits Integer indicating the number of decimal places (round) or significant digits (signif) to be used. For round, negative values are allowed (see base::Round for more details).
#'
#' @author Thomas Chalaux-Clergue
#' @export
esp.values <- function(pred, obs, count = FALSE, digits = 0){
  n <- 0
  for(i in pred){
    if(i >= min(obs) & max(obs) >= i){
      n <- n+1
    }
  }
  if(count == FALSE){
    esp <- round(n/length(pred)*100, digits)
  }else{
    esp <- n
  }
  return(esp)
}
