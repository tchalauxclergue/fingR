#' Interval width
#'
#' @param prev description
#' @param path.to.prev description
#' @param sample_name description
#' @param source.groups description
#' @param value description
#' @param mean.cal description
#' @param save description
#' @param save.path description
#' @param note a character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
interval.width <- function(prev, path.to.prev, sample_name, source.groups, value, mean.cal = FALSE, save = FALSE, save.dir, note, fileEncoding = "latin1"){

  require(dplyr)
  require(stats)
  require(utils)

  if(missing(prev)){
    prev <- read.csv(path.to.prev)
    if(ncol(prev)==1){ prev <- read.csv(path.to.prev, sep=";") } #if the csv was opened with excel
  }

  if(missing(sample_name)){sample_name <- colnames(prev)[1]} # assume that sample name/id is the first column
  if(missing(source.groups)){source.groups <- colnames(prev)[2]} # assume that source groups/class is the 2nd column
  if(missing(value)){value <- colnames(prev)[3]} # assume that prevision value is the 3rd column

  # calculating stats::quantiles and interval widths
  int <- prev %>%
    dplyr::group_by(.data[[sample_name]], .data[[source.groups]]) %>%
    dplyr::summarise("W50"= round(stats::quantile(x = .data[[value]], probs = 0.75),3) - round(stats::quantile(x = .data[[value]], probs = 0.25),3),
                     "W95"= round(stats::quantile(x = .data[[value]], probs = 0.975),3) - round(stats::quantile(x = .data[[value]], probs = 0.025),3)) %>%
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

  if(isTRUE(mean.cal)){
    int.mean <- int %>%
      dplyr::group_by(.data[[source.groups]]) %>%
      dplyr::summarise("W50.mean" = round(mean(.data[["W50"]]), 3),
                       "W95.mean" = round(mean(.data[["W95"]]), 3))
    colnames(int.mean)[1] <- "Source"

    resu <- list(int, int.mean)
    names(resu) <- c("samples", "mean")
  }else{
    resu <- int
  }


  if(isTRUE(save)){
    file.name <- "Interval_width"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    if(missing(save.dir)){
      path2 <- unlist(strsplit(path.to.prev, split="[/]"))
      save.dir <- paste(paste(path2[1:(length(path2)-1)], collapse = "/"), "/", sep="")
    }
    utils::write.csv(int, paste(save.dir, paste(file.name, ".csv", sep=""), sep=""), fileEncoding = fileEncoding, row.names = FALSE)
    if(isTRUE(mean.cal)){
      utils::write.csv(int.mean, paste(save.dir, paste(file.name, "_mean.csv", sep=""), sep=""), fileEncoding = fileEncoding, row.names = FALSE)
    }
  }
  return(resu)
}
