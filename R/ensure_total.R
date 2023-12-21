#' Ensure the total of contributions
#'
#' Correct contributions to reach a total of 1 or 100%.
#'
#' @param data A data.frame containing predicted contributions for each sources and sample names/id.
#' @param sample.name A character corresponding to the name of the column with sample names/id. If not set the function use the first column.
#' @param digits nteger indicating the number of decimal places (round) or significant digits (signif) to be used. For round, negative values are allowed (see base::Round for 'Details’).
#'
#' @return a data.frame.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
ensure.total <- function(data, sample.name, digits = 3, path, note, fileEncoding = "latin1"){

  if(missing(sample.name)){ # suppose that the first column contains sample names/id
    sample.name <- colnames(data)[[1]]
  }

  div <- 0 # the divergence from 1
  total <- rowSums(data[,subset(colnames(data), colnames(data) != sample.name)])

  # while all totals are different from 1 however if a maximum of optimisation is reached then stop
  while( sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.name)])) != nrow(data) &  sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.name)])) != div ){

    div <- sum(rowSums(data[,subset(colnames(data), colnames(data) != sample.name)]))

    total <- rowSums(data[,subset(colnames(data), colnames(data) != sample.name)]) # calculate the sum of contributions
    data <- cbind(data[sample.name], round(data[,subset(colnames(data), colnames(data) != sample.name)] * (1 + (1-total)), digits = digits) ) # correct the contribution as: contribution * (1 + (1-total))
  }

  dt.corrected <- cbind(data, total)

  if(!missing(path)){
    file.name <- "corrected_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    write.csv(dt.corrected, paste(path, paste0(file.name, ".csv"), sep="/"), fileEncoding = fileEncoding, row.names = FALSE)
  }

  return(dt.corrected) # return a data.frame with corrected contributions and their total
}
