#' Is discriminant
#'
#' Create vector(s) of discrimant properties according to their results
#'
#' @param data A data.frame containing all range test results.
#' @param property A character listing all the properties that will be evaluated.
#' @param test.format A character vector of format strings to recognise test ("test") and criteria ("crit") label position.
#' @param test.pos A numeric value that indicates where criteria is wrote in test columns labels.
#' @param sep.format A character vector of format strings to recognise the separator to split test labels.
#' @param p.level  p-value threshold to reject null hypothesis (H0) (0.05 - default).
#' @param note A character string to add a note at the start of test label (not set - default).
#'
#' @return data.frame. The results of the different test.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
#'
is.discriminant <- function(data, property, test.format, test.pos, sep.format, p.level = 0.05, note){

  # test if the data.frame comes from fingR::discriminant.test
  if( setequal(colnames(data), c("Property", "n.similar.groups", "respect.KW.shape.assumption", "Kruskal.Wallis_p.value", "Kruskal.Wallis_signif")) ){
    property = "Property"
    test.format = "Kruskal.Wallis_p.value"
    test.pos = 1
    sep.format = "_"
  }else if( setequal(colnames(data), c("Property", "n.diff.groups", "Kolmogorov.Smirnov_discriminant")) ){
    property = "Property"
    test.format = "Kolmogorov.Smirnov_discriminant"
    test.pos = 1
    sep.format = "_"
  }

  resu <- list()
  for(test in test.format){ # if several tests
    if(is.numeric(data[[test]])){ # For numerical statistics
      resu[[unlist(strsplit(test, split = sep.format))[test.pos]]] <- data[[property]][which(data[[test]] <= p.level)]
    }
    if(data[[test]][1] == "TRUE" || data[[test]][1] == "FALSE"){ # For boolean statistics (TRUE, FALSE)
      resu[[unlist(strsplit(test, split = sep.format))[test.pos]]] <- data[[property]][data[[test]] == "TRUE"]
    }
  }

  if(!missing(note)){
    names(resu) <- paste0(note, ".", names(resu))
  }
  return(resu)
}
