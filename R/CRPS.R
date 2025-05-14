#' Continuous Raking Probability Score
#'
#' Compute the CRPS values for Bayesian models.
#'
#' @param obs a data.frame with the observed source group contributions.
#' @param prev a data.frame with previsions for each sample and for each source group.
#' @param path.to.prev a connection to the previsions for each sample and for each source group.
#' @param sample.name.obs,sample.name.prev a character string of the column name with sample name in obs/prev.
#' @param prev.source.name a character string of the column name with source groups in prev.
#' @param prev.values a character string of the column name with prevision values in prev.
#' @param source.groups a vector of character with source groups names.
#' @param mean.cal a boolean. Set TRUE to calculate the CRPS mean value per sample.
#' @param save.dir connection open for writing the test results data.frame. If "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note a character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
CRPS <- function(obs, prev, path.to.prev, sample.name.obs, sample.name.prev, prev.source.name, prev.values, source.groups, mean.cal = FALSE, save.dir, note, fileEncoding = "latin1"){

  require(dplyr)
  require(scoringRules)

  if(missing(prev)){
    prev <- read.csv(path.to.prev)
    if(ncol(prev)==1){ prev <- read.csv(path.to.prev, sep=";") } #if the csv was opened with excel
  }
  
  if(missing(sample.name.obs)){ sample.name.obs <- colnames(obs)[1] } # if no sample name was given it is assumed that it is the first column
  if(missing(sample.name.prev)){ sample.name.prev <- colnames(prev)[1] } # if no sample name was given it is assumed that it is the first column
  if(missing(prev.source.name)){ prev.source.name <- colnames(prev)[2] }
  if(missing(prev.values)){ prev.values <- colnames(prev)[3] }

  if(missing(source.groups)){ # if no source groups names were given it is assumed that they are all the other obs columns names
    source.groups <- colnames(obs)[2:ncol(obs)]
  }

  # correct proportions
  if(max(obs[,source.groups]) > 1 & !(max(prev[[prev.values]]) > 1)){
    obs[,source.groups] <- obs[,source.groups] / 100
  }
  
  #for each sample
  CRPS.df <- as.data.frame(obs[[sample.name.obs]])
  for(group in source.groups){
    CRPS <- c()
    for(name in obs[[sample.name.obs]]){
      s.prev <- prev %>%
        dplyr::filter(.data[[sample.name.prev]] == name & .data[[prev.source.name]]  == group) # select only prevision for one sample and one source group
      s.obs <- obs[which(obs[[sample.name.obs]] == name), group] # select the sample observation

      CRPS <- c(CRPS, round(scoringRules::crps_sample(y = s.obs, dat = s.prev[[prev.values]], "edf"), 4) ) # calculation of CRPS
    }
    CRPS.df <- cbind(CRPS.df, CRPS)
  }
  colnames(CRPS.df) <- c(sample.name.obs, source.groups)


  if(isTRUE(mean.cal)){
    CRPS.mean <- data.frame("Source" = source.groups, "CRPS.mean" = unname(round(colMeans(x = CRPS.df[,source.groups]),4)))

    resu <- list(CRPS.df, CRPS.mean)
    names(resu) <- c("samples", "mean")
  }else{
    resu <- CRPS.df
  }

  # saving
  if(isTRUE(save) | !missing(save.dir)){
    file.name <- "CRPS"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep = "_")
    }
    if(missing(save.dir)){
      path2 <- unlist(strsplit(path.to.prev, split="[/]"))
      save.dir <- paste(paste(path2[1:(length(path2)-1)], collapse = "/"), "/", sep="")
    }
    utils::write.csv(CRPS.df, paste0(save.dir, file.name, ".csv"), fileEncoding = fileEncoding, row.names = FALSE)
      if(isTRUE(mean.cal)){
        utils::write.csv(CRPS.mean, paste0(save.dir, file.name, "_mean.csv"), row.names = F, fileEncoding = fileEncoding)

    }
  }
  return(resu)
}
