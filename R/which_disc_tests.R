#' Discriminant test label
#'
#' Return columns names for discriminant tests result data frame base on the setted list of tests.
#' This function is called by discriminant_power(). Not intended for user use.
#'
#' @param tests a non-empty character vector of tests names: Kolmogorov-Smirnov (KW - default) or Kolmogorov-Smirnov ("KS").
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
which.disc.tests <- function(tests){

  performed.tests <- c()

  if("KW" %in% tests || "all" %in% tests){
    performed.tests <- c(performed.tests, "KW_p.value", "KW_signif")
  }
  if("KS" %in% tests || "all" %in% tests){
    performed.tests <- c(performed.tests, "KS_p.value", "KS_signif")
  }

  return(performed.tests)
}
