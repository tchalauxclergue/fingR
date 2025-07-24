#' Interval width
#'
#' Computes the width of the central 50% (W50) and 95% (W95) prediction intervals
#' for each sample and source group, based on a table of predictive draws.
#' 
#' @param prev A `data.frame` containing predictions for each sample and source group. If missing, the function reads the data from `path.to.prev`.
#' @param path.to.prev A file path (character string) pointing to a `.csv` file with prediction draws (used only if `prev` is missing).
#' @param sample_name A character string indicating the name of the column containing sample identifiers. If missing, defaults to the first column.
#' @param source.groups A character string indicating the name of the column defining the source groups or classes. If missing, defaults to the second column.
#' @param value A character string indicating the column with prediction values. If missing, defaults to the third column.
#' @param mean.cal Logical; if `TRUE`, the function also computes the average interval widths (`W50.mean`, `W95.mean`) for each source group. Defaults to `FALSE`.
#' @param save Logical; if `TRUE`, the results are saved as `.csv` files. Defaults to `FALSE`.
#' @param save.dir A character string giving the directory where the output files will be saved. If missing and `save = TRUE`, the function infers the directory from `path.to.prev`.
#' @param note Optional character string to add a suffix to the output file names. Useful to distinguish output versions. Default is not set.
#' @param fileEncoding A character string defining the file encoding for output files (if `save = TRUE`). Default is `"latin1"`.
#'
#' @return If `mean.cal = FALSE`, returns a data frame with W50 and W95 values
#' for each sample and source group. If `mean.cal = TRUE`, returns a named list with
#' two elements:
#' \describe{
#'   \item{samples}{A data frame of interval widths per sample and group.}
#'   \item{mean}{A data frame of mean interval widths per source group.}
#' }
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
interval.width <- function(prev, path.to.prev, sample_name, source.groups, value, mean.cal = FALSE, save = FALSE, save.dir, note, fileEncoding = "latin1"){

  require(dplyr)
  require(stats)
  require(utils)

  if (missing(prev)) {
    prev <- read.csv(path.to.prev)
    if (ncol(prev) == 1) { prev <- read.csv(path.to.prev, sep = ";") } #if the csv was opened with excel
  }

  if (missing(sample_name)) {sample_name <- colnames(prev)[1]} # assume that sample name/id is the first column
  if (missing(source.groups)) {source.groups <- colnames(prev)[2]} # assume that source groups/class is the 2nd column
  if (missing(value)) {value <- colnames(prev)[3]} # assume that prevision value is the 3rd column

  # calculating stats::quantiles and interval widths
  int <- prev %>%
    dplyr::group_by(.data[[sample_name]], .data[[source.groups]]) %>%
    dplyr::summarise("W50" = round(stats::quantile(x = .data[[value]], probs = 0.75),3) - round(stats::quantile(x = .data[[value]], probs = 0.25),3),
                     "W95" = round(stats::quantile(x = .data[[value]], probs = 0.975),3) - round(stats::quantile(x = .data[[value]], probs = 0.025),3)) %>%
    ungroup() %>% as.data.frame()

  # Proportion of encompassed values
  #P50 <- c()
  #P95 <- c()
  #for(id in levels(as.factor(int$sample))){
  #  for(grp in levels(as.factor(int$sources))){
  #    subprev <- dplyr::filter(prev, .data[[sample_name]] == id & .data[[source.groups]] == grp)
  #    bounds <- dplyr::filter(int, .data[[sample_name]] == id & .data[[source.groups]] == grp)
  #    P50 <- c(P50, round(sum(subprev >= bounds$Q25 & subprev <= bounds$Q75) / nrow(subprev),2))
  #    P95 <- c(P95, round(sum(subprev >= bounds$Q2.5 & subprev <= bounds$Q97.5) / nrow(subprev),2))
  #    print(id)
  #  }
  #}

  if (isTRUE(mean.cal)) {
    int.mean <- int %>%
      dplyr::group_by(.data[[source.groups]]) %>%
      dplyr::summarise("W50.mean" = round(mean(.data[["W50"]]), 3),
                       "W95.mean" = round(mean(.data[["W95"]]), 3))
    colnames(int.mean)[1] <- "Source"

    resu <- list(int, int.mean)
    names(resu) <- c("samples", "mean")
  } else {
    resu <- int
  }


  if (isTRUE(save) | !missing(save.dir)) {
    file.name <- "Interval_width"
    if (!missing(note)) {
      file.name <- paste0(file.name, note)
    }
    if (missing(save.dir)) {
      path2 <- unlist(strsplit(path.to.prev, split = "[/]"))
      save.dir <- paste0(paste(path2[1:(length(path2) - 1)], collapse = "/"), "/")
    }
    utils::write.csv(int, paste0(save.dir, paste0(file.name, ".csv")), fileEncoding = fileEncoding, row.names = FALSE)
    if (isTRUE(mean.cal)) {
      utils::write.csv(int.mean, paste0(save.dir, paste0(file.name, "_mean.csv")), fileEncoding = fileEncoding, row.names = FALSE)
    }
  }
  return(resu)
}
