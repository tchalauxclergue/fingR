#' Log transformation
#'
#' Convert data to log, while being careful to negatives values.
#' If some values are negative performs a shift of all values to the minimum value increased by a certain percentage,  10% (.1 - default).
#'
#' @param data a (non-empty) numeric vector of data .
#' @param shift a numeric value of the size of shift, .1 (default)
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
to.log <- function(data, shift = .1){

  #correction of negative value to avoid NAs
  if(min(data, na.rm = T) < 0){ # if at least one value is negative
    if(max(data, na.rm = T) < 0){ # if all the data is negative
      data <- -data
    }else{ # if only some data are below 0 do a shift in data (+ 10%)
      data <- data - (min(data, na.rm = T) * (1 + shift))
    }
  }
  return(log(data)) #return the log of the data
}
