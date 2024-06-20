#' Range tests
#'
#' Performs range tests on properties to assess conservative behaviour for sediment fingerprinting studies.
#'
#' @param data A data.frame containing all the data values.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and mixture information.
#' @param mixture A character string corresponding to way "mixture" (default) are named.
#' @param properties A vector listing all the properties that will be evaluated.
#' @param alternative A character that indicates if range test should be apply on each sample (single) or should calculates the criterion on all samples (population).
#' @param sample.id description
#' @param criteria A vector or character that indicates the range tests criteria to be performed among: minimum-maximum ("MM"), minimum-maximum +/- measurement error ("MMe"), "whiskers", "hinge", "mean", mean +/- standard deviation ("mean.sd"), "median".
#' @param MM.error A numeric value indicating how much measurement error should be added to source value. No measurement error = 0.0 and 10 percent = 0.1.
#' @param hinge.range A vector listing tested percentages of population within the hinge, 70 -> c(70) (default).
#' @param CI.lvl A vector listing tested confidance interval level, 95 = c(95) (default).
#' @param shift A numeric value of the size of shift to correct the minimum value to do log transformation, .1 (default)
#' @param save.each TRUE (default) to save each sample range test.
#' @param save.dir Connection open for writing the test results data.frame. If "" save the file at working directory. If not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#' @return a data.frame with tests results.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
range.tests <- function(data, class, mixture = "target", properties, alternative = "single", sample.id, criteria = "all", MM.error = c(.1), hinge.range = c(70), CI.lvl = c(95), shift = .1, save.each = TRUE, save.dir, note, fileEncoding = "latin1"){

  require(dplyr)

  criteria <- c(criteria) #if the user only set one test

  if(alternative == "single"){
    s.id <- data[, c(class, sample.id)] %>% dplyr::filter(.data[[class]] == mixture) %>% dplyr::select(all_of(sample.id))
    results.RTs <- list()
  }

  results.df <- data.frame(matrix(ncol = 0, nrow = length(properties))) #empty data frame (number of rows = number of properties)

  for(prop in properties){ #for each property
    clean.prop <- data[complete.cases(data[[prop]]), c(class, prop)] #delete rows without data - avoid data with NA values
    n.nas <- as.numeric(length(data[[prop]]) - length(clean.prop[[prop]])) # save the number of NA samples

    data.S <- dplyr::filter(clean.prop, .data[[class]] != mixture) #sources data
    data.T <- dplyr::filter(clean.prop, .data[[class]] == mixture) #mixtures data

    resu <- fingR::RT.player(data.s = data.S, data.t = data.T[[prop]], criteria = criteria, MM.error = MM.error, hinge.range = hinge.range, CI.CLs = CI.lvl, alternative = alternative, class = class, shift = shift)

    if(alternative == "single"){
      sapply(resu, "[[", 2)
      results.df <- rbind(results.df, c(prop, #property name
                                        nrow(data.S), #number of source samples
                                        nrow(data.T), #number of mixture/target samples
                                        n.nas, #number of NA
                                        unname(sapply(resu, "[[", 2)))) # range tests results

      # each sample range test results
      results.RT <- data.frame(s.id, prop, nrow(data.S), unname(sapply(resu, "[[", 1))) #create data frame with each sample range test result
      colnames(results.RT) <- c(sample.id, "Property", "n_source", paste0("RT_", names(resu))) #correct col names
      if(save.each == TRUE){
        if(!missing(save.dir)){
          file.name <- paste("RT_samples", prop, alternative, sep = "_")
          if(!missing(note)){ # if the user add a note
            file.name <- paste(file.name, note, sep = "_") # if a note is added to the file name
          }
          write.csv(results.RT, file = paste0(save.dir, paste0(file.name, ".csv")), na = "", row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
        }
      }
      results.RTs[[prop]] <- results.RT #list of all range tests per sample
    }
  }
  colnames(results.df) <- c("Property", "n_source", "n_mixture", "NAs", paste("RT", names(resu), alternative, sep="_")) #correct the col names of the results data frame

  results.df[, c("n_source", "n_mixture", "NAs")] <- sapply(results.df[, c("n_source", "n_mixture", "NAs")], as.numeric) #correct the format for sources and mixture number and p-value to be numeric

  #saving data
  if(!missing(save.dir)){
    file.name <- paste("Range_test", alternative, sep = "_")
    if(!missing(note)){ # if the user add a note
      file.name <- paste(file.name, note, sep = "_") # if a note is added to the file name
    }
    write.csv(results.df, file = paste0(save.dir, paste0(file.name, ".csv")), na = "", row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
  }

  if(alternative == "single"){
    to.return <- list(results.df, results.RTs)
    names(to.return) <- c("results.df", "results.RT")
  }else if(alternative == "population"){
    to.return <- results.df
  }

  return(to.return)
}
