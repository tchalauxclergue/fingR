#' Significance level
#'
#' Return the significant figures level of a numeric or the highest level of a vector of numerics.
#'
#' @param x a numeric or a complex numeric vector.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
lvl.signif<- function(x){
  lvl <- 0
  for(val in x){ #for every values in the vector
    val <- unlist(strsplit(as.character(val), split = "[.]")) #split the string at the "." then "0.02" become "0" "02"
    if(!is.na(val[2]) && nchar(val[2]) > lvl){ #if the number of chracter in the string after the dot is bigger than the previous registered significant figures level
      lvl <- as.numeric(nchar(val[2])) # change the significance figures level by the new level
    }
  }
  return(lvl)
}
