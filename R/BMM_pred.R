#' Predicted Source Contributions Extraction for Bayesian Mixing Models

#' @param data A data frame containing the predicted source contribution from a Bayesian Mixing model.
#' @param stats A character or a character vector specifying the summary statistics to extract for each source among: "Median" and/or "Mean".
#' @param sample.id A character specifying the column name in 'data' containing sample identifiers.
#' @param source A character specifying the column name in 'data' containing source identifiers.
#' @param save.dir Connection open for writing the test results data.frame. If "" save the file at working directory.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).

#' @returns A data frame containing the predicted source contributions for each sample.
#' 
#' @author Thomas Chalaux-Clergue
#' 
#' @export
BMM.pred <- function(data, stats, sample.id, source, save.dir, note, fileEncoding = "latin1"){

  require(tidyr)

  # Handle missing arguments
  if(missing(sample.id)){ sample.id <- colnames(data)[1] }
  if(missing(source)){ source <- colnames(data)[2] }

  # Rename 'Median' to 'Q50' for compatibility with summary statistics file
  stats[stats %in% c("Median", "50%")] <- "Q50"
  
  # Keep necessary columns from the data
  df.stats <- data %>% dplyr::select(dplyr::all_of(c(sample.id, source, stats)))

  # Reshape the data to wide format
  df.stats <- tidyr::pivot_wider(df.stats, names_from = source, values_from = dplyr::all_of(stats)) %>%
    as.data.frame()

  # For median calculatation only: Rename columns based on summary statistics and source names
  stats[stats %in% c("Q50")] <- "Median"
  
  colnames(df.stats)[2:ncol(df.stats)] <- paste(stats, colnames(df.stats)[2:ncol(df.stats)], sep="_")

  # save results
  if(!missing(save.dir)){
    file.name <- "BMM_ordered_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(x = df.stats, file = paste0(save.dir, file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
  }
  return(df.stats)
}
