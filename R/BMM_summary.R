#' Summarize the results of BMM predictions
#' 
#' @param pred A data frame containing the predicted values from a mixing model.
#' @param sample.id A character specifying the column name in 'pred' containing sample identifiers.
#' @param source A character specifying the column name in 'pred' containing source identifiers.
#' @param value A character specifying the column name in 'pred' containing predicted values.
#' @param save.dir Connection open for writing the test results data.frame. If "" save the file at working directory.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#' 
#' @returns A data frame containing summary statistics for the predicted values.
#' 
#' @author Thomas Chalaux-Clergue
#' 
#' @export
BMM.summary <- function(pred, sample.id, source, value, save.dir, note, fileEncoding = "latin1"){

  library(dplyr)

  if(missing(sample.id)){ sample.id <- colnames(pred)[1] }
  if(missing(source)){ source <- colnames(pred)[2] }
  if(missing(value)){ value <- colnames(pred)[3] }

  pred.stats <- pred %>%
    group_by(.data[[sample.id]], .data[[source]]) %>%
    dplyr::summarise("Mean" = round(mean(.data[[value]]), 3),
                     "SD" = round(stats::sd(.data[[value]]), 3),
                     "Q2.5" = round(stats::quantile(.data[[value]], probs = 0.025),3),
                     "Q5" = round(stats::quantile(.data[[value]], probs = 0.05),3),
                     "Q25" = round(stats::quantile(.data[[value]], probs = 0.25),3),
                     "Q50" = round(stats::quantile(.data[[value]], probs = 0.50),3),
                     "Q75" = round(stats::quantile(.data[[value]], probs = 0.75),3),
                     "Q95" = round(stats::quantile(.data[[value]], probs = 0.95),3),
                     "Q97.5" = round(stats::quantile(.data[[value]], probs = 0.975),3)) %>%
    ungroup() %>%
    as.data.frame()

  # save results
  if(!missing(save.dir)){
    file.name <- "BMM_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(x = pred.stats, file = paste0(save.dir, file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
  }

  return(pred.stats)
}
