#' Discriminant power assessment
#'
#' Performs discrimination tests on data for fingerprinting studies.
#' Tests results highly depends on the defined sources classes.
#'
#' @param data A data.frame containing all the data values.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and mixture information.
#' @param mixture A character string corresponding to way "mixture" (default) are named.
#' @param properties A vector listing all the properties that will be evaluated.
#' @param test A character vector that indicates the test that will be performed: Kolmogorov-Smirnov ("KS") or Kruskal-Wallis ("KW").
#' @param p.level p-value threshold to reject null hypothesis (H0) (0.05 - default).
#' @param min.discriminant A numeric value of the lowest number of sources couples that need to be statistically different to consider the property as discriminant (1 - default). Only for Kolmogorov-Smirnov ("KS").
#' @param save.discrim.tests Logical. If TRUE (default) save the data.frame of all sources pairs tests.
#' @param save.dir Connection open for writing the test results data.frame, "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#' @return data.frame. The results of the different test.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
discriminant.test <- function(data, class, mixture, properties, test, p.level = .05, min.discriminant = 1, save.discrim.tests = TRUE, save.dir, note, fileEncoding = "latin1"){

  require(dplyr)
  require(utils)

  # remove mixture's values from data
  if(!missing(mixture)){
    data <- dplyr::filter(data, .data[[class]] != mixture)
  }

  # Run Kolmogorov-Smirnov test
  result.KS <- fingR::discrim.tests(data, class, test = "KS", properties = properties)
  if(!missing(save.dir) & isTRUE(save.discrim.tests)){ # save KS two-sample tests results
    file.name <- "Discriminant_pairs"
    if(!missing(note)){file.name <- paste(file.name, note, sep="_")} # user note
    utils::write.csv(result.KS, file = paste0(save.dir, paste0(file.name, ".csv")), na = "", row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
  }

  if(test == "KS"){
    results.df <- fingR::discriminant.power(data = result.KS, test.context = "KS", min.discriminant = min.discriminant, p.level = p.level) # use KS to verify KW samples distribution assumption

  }else if(test == "KW"){  # Kruskal-Wallis route
    signif.KS <- fingR::discriminant.power(data = result.KS, test.context = "KW.KS", min.discriminant = min.discriminant, p.level = p.level) # use KS to verify KW samples distribution assumption
    result.KW <- fingR::discrim.tests(data, class, test = "KW", properties = properties) # runs Kruskal-Wallis test

    results.df <- dplyr::left_join(signif.KS, result.KW, by = c("Property")) # join tests results
  }

  #saving discriminant power results
  if(!missing(save.dir)){
    file.name <- "Discriminant_tests"
    if(!missing(note)){ file.name <- paste(file.name, test, note, sep = "_") } # user note
    utils::write.csv(results.df, file = paste0(save.dir, paste0(file.name, ".csv")), na = "", row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
  }

  return(results.df)
}
