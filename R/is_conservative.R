#' Is conservative
#'
#' Create vector(s) of discrimant properties according to their results
#'
#' @param data A data.frame containing all range test results.
#' @param property A character listing all the properties that will be evaluated.
#' @param test.format A character vector of format strings to recognise test ("test") and criteria ("crit") label position.
#' @param crit.pos A numeric value that indicates where criteria is wrote in test columns labels.
#' @param sep.format A character vector of format strings to recognise the separator to split test labels.
#' @param note A character string to add a note at the start of test label (not set - default).
#'
#' @return data.frame. The results of the different test.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
#'
is.conservative <- function(data, property, test.format = "RT", crit.pos = 2, sep.format = "_", note){
  tests <- colnames(data)[grepl(test.format, colnames(data))] #get test column names

  resu <- list()
  for(test in tests){
    resu[[unlist(strsplit(test, split = sep.format))[crit.pos]]] <- data[which(data[[test]] == "TRUE"), property]
  }
  if(!missing(note)){
    names(resu) <- paste0(note, ".", names(resu))
  }
  return(resu)
}
