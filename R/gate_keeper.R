#' Verify and correct fully negative column
#'
#' Change fully negative column to fully positive.
#'
#' @param data A data.frame.
#' @param tracers A vector listing them columns.
#' @param inverter A boolean, if TRUE will invert the sign of designated columns
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
gate.keeper <- function(data, tracers, inverter = FALSE){

  names <- c()

  for(c in tracers){
    if(max(data[[c]]) < 0 ){
      data[[c]] <- -data[[c]]
      names <- append(names, c)
    }

    if(isTRUE(inverter)){
      data[[c]] <- -data[[c]]
    }
  }


  if(isTRUE(inverter)){
    resu <- data
  }else{
    resu <- list(data, names)
  }
  return(resu)
}
