#' A Wilk's lambda stepwise tracer selection
#'
#' Run a discriminant function analysis (DFA) forward stepwise selection based on Wilk's criterion to select tracers that maximise the difference and minimise the redoundancy of information among selected tracers.
#'
#' @param data a data.frame containing all the data values.
#' @param class a character string corresponding to the column that contains the sources (source 1, source 2, etc) and target information.
#' @param tracers a vector listing all the tracers that will be evaluated.
#' @param target a character string corresponding to way "Target" are named, not set default.
#' @param save.dir connection open for writing the test results data.frame, "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note a character string to add a note at the end of the file name (not set - default).
#'
#' @return a vector of selected tracers
#'
#' @author Thomas Chalaux-Clergue and Rémi Bizeul
#'
#' @export
stepwise.selection <- function(data, class, tracers, target, save.dir, note){

  require(klaR)
  require(dplyr)

  # create the results data.frame
  results.df <- data.frame(matrix(ncol = 0, nrow = length(tracers)))
  results.df$tracers <- tracers

  # clean the data
  # remove target's values from data
  if(!missing(target)){
    data <- dplyr::filter(data, .data[[class]] != target)
  }
  # make a smaller dataset with only specified tracers
  data <- data %>% dplyr::select(all_of(class), all_of(tracers))



  # TEST - stepwise selection based on Wilk's lambda
  formule <- as.formula(paste(class, paste(tracers, collapse = " + "), sep = " ~ ")) # set the formula
  resu.stepw <- klaR::greedy.wilks(formule, data, niveau = 0.1) # run Wilk's lambda stepwise selection

  # RESULTS
  results.df <- left_join(results.df, resu.stepw$results,  by = c("tracers" = "vars")) # adding stepwise selection results to result data frame
  results.df$Stepwise_selection <- ifelse(tracers %in% resu.stepw$results$vars, TRUE, FALSE) # say if the vars was selected

  #saving data
  if(!missing(save.dir)){
    if(!missing(note)){ # if the user add a note
      file.name <- paste("Stepwise_selection", note, sep = "_") # if a note is added to the file name
    }else{
      file.name <- "Stepwise_selection" # if the user didn't add a note
    }
    write.csv(results.df, file = paste0(save.dir, paste0(file.name, ".csv"))) # if no note is added to the file name
  }

  return(resu.stepw$results$vars)
}
