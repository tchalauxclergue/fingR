#' Generate CSV for MixSIAR
#'
#' Generate mix and source CSV files compatible with MixSIAR data loading functions (i.e. *MixSIAR::load_mix_data* and *MixSIAR::load_source_data*).
#'
#' @param data A data.frame containing all the data values.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param target A character string corresponding to the way "Target" (default) are named.
#' @param tracers A vector listing the tracers names.
#' @param sample.name A character string correspond to samples id/name column.
#' @param save.dir Connection open for writing the test results data.frame. If "" save the file at working directory.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#' @param show.data A boolean, if FALSE data.frame are saved but not returned.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
data.for.MixSIAR <- function(data, class, target, tracers, sample.name, save.dir, note, fileEncoding = "latin1", show.data = FALSE){

  require(dplyr)

  # mix's data
  mix.dt <- data %>%
    dplyr::filter(.data[[class]] == target) %>%
    dplyr::select(all_of(c(sample.name, tracers)))

  # source's data
  sources <- data %>%
    dplyr::filter(.data[[class]] != target) %>%
    dplyr::select(all_of(c(class, tracers)))

  s.mean <- sources %>% # source mean
    group_by(.data[[class]]) %>%
    summarise_all(mean) %>%
    dplyr::select(-all_of(class)) %>%
    as.data.frame()

  s.sd <- sources %>% # source SD
    group_by(.data[[class]]) %>%
    summarise_all(sd) %>%
    dplyr::select(-all_of(class)) %>%
    as.data.frame()

  s.n <- sources %>% # number of sample per source
    group_by(.data[[class]]) %>%
    summarise(n = n()) %>%
    dplyr::select(-all_of(class)) %>%
    as.data.frame()

  for(i in tracers){ # rounding values according to data
    lvl <- lvl.signif(data[[i]])
    s.mean[[i]] <- round(s.mean[[i]], lvl)
    s.sd[[i]] <- round(s.sd[[i]], lvl)
  }

  source.dt <- data.frame(matrix(nrow = nrow(s.mean), ncol = 0))
  clnames <- c()
  for(i in 1:ncol(s.mean)){
    source.dt <- cbind(source.dt,
                           cbind(s.mean[,i], s.sd[,i]))

    clnames <- c(clnames,
                 paste("Mean", colnames(s.mean)[i], sep=""),
                 paste("SD", colnames(s.sd)[i], sep=""))
  }
  source.dt <- cbind(source.dt, s.n$n)

  colnames(source.dt) <- c(clnames, "n")
  row.names(source.dt) <- levels(as.factor(sources[[class]]))


  # discrimination levels / trophic levels
  discrim.dt <- matrix(data = 0, # tracers are considered conservative
                       nrow = nlevels(as.factor(sources[[class]])), # number of sources
                       ncol = length(tracers)*2, # maximum number of selected tracers
                       dimnames = list(levels(as.factor(sources[[class]])),
                                       paste0(c("Mean", "SD"), rep(tracers, each = 2))) # generate prop names according to MixSIAR format (i.e. MeanPropertyA, SDPropertyA...)
                       )
  
  
  # saving data
  mix.name <- "MixSIAR_mix"
  src.name <- "MixSIAR_sources"
  discrim.name <- "MixSIAR_discrimination"
  if(!missing(note)){
    mix.name <- paste0(mix.name,"_", note)
    src.name <- paste0(src.name,"_", note)
    discrim.name <- paste0(discrim.name,"_", note)
  }
  write.csv(mix.dt, paste0(save.dir, paste0(mix.name, ".csv")), row.names = F, fileEncoding=fileEncoding)
  write.csv(source.dt, paste0(save.dir, paste0(src.name, ".csv")), fileEncoding=fileEncoding)
  write.csv(discrim.dt, paste0(save.dir, paste0(discrim.name, ".csv")), row.names = T, fileEncoding=fileEncoding)

  if(show.data == TRUE){
    result <- list(mix.dt, source.dt, discrim.dt)
    names(result) <- c("mix", "sources", "discimination")

    return(result)
  }
}
