#' Ensure the total of contributions
#'
#' Correct contributions to reach a total of 1 or 100%.
#'
#' @param data A data.frame containing predicted contributions for each sources and sample names/id.
#' @param sample.id A character corresponding to the name of the column with sample names/id. If not set the function use the first column.
#' @param digits nteger indicating the number of decimal places (round) or significant digits (signif) to be used. For round, negative values are allowed (see base::Round for 'Details’).
#' @param save.dir Connection open for writing the results data.frame. If "" save the file at working directory.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#'
#' @return a data.frame.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
ensure.total <- function(data, sample.id, digits = 3, save.dir, note, fileEncoding = "latin1"){

  if(missing(sample.id)){ # suppose that the first column contains sample names/id
    sample.id <- colnames(data)[[1]]
  }

  div <- 0 # the divergence from 1
  total <- rowSums(data[,subset(colnames(data), colnames(data) != sample.id)])

  # while all totals are different from 1 however if a maximum of optimisation is reached then stop
  while( sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.id)])) != nrow(data) &  sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.id)])) != div ){

    div <- sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.id)]))

    total <- rowSums(data[,subset(colnames(data), colnames(data) != sample.id)]) # calculate the sum of contributions
    data <- cbind(data[sample.id], round(data[,subset(colnames(data), colnames(data) != sample.id)] * (1 + (1-total)), digits = digits) ) # correct the contribution as: contribution * (1 + (1-total))
  }

  dt.corrected <- cbind(data, total)

  if(!missing(save.dir)){
    file.name <- "corrected_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    write.csv(dt.corrected, paste(save.dir, paste0(file.name, ".csv"), sep="/"), fileEncoding = fileEncoding, row.names = FALSE)
  }

  return(dt.corrected) # return a data.frame with corrected contributions and their total
}
