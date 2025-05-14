#' Summary JAGS
#'
#' Generate a table with each sample summary of JAGS predictions (e.g. mean, quantiles, etc).
#'
#' @param jags.1 rjags model object, output from MixSIAR::run_model function
#' @param mix output from MixSIAR::load_mix_data
#' @param sources output from MixSIAR::load_source_data
#' @param path connection open to write the results data.frame.
#' @param note a character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#' @param save_pred a boolean to save all the MixSIAR Monte-Carlo chain predictions
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
JAGS.summary <- function(jags.1, mix, sources, path, note, fileEncoding = "latin1", save_pred = FALSE){

  require(dplyr)

  names.samples <- sort(mix$data[[mix$factors]])
  names.source <- sources$source_names

  pred.arrays <- jags.1$BUGSoutput$sims.list$p.fac1

  # arrange the arrays from jags.1 model to get the prediction
  all.preds <- data.frame()
  for(i in 1:dim(pred.arrays)[2]){ # across the samples
    preds <- data.frame()
    for(g in 1:dim(pred.arrays)[3]){ # across the source groups
      preds <- rbind(preds, cbind(data.frame("sample" = names.samples[i], "source" = names.source[g], "pred" = as.numeric(pred.arrays[, i, g]))))
    }
    all.preds <- rbind(all.preds, preds)
  }

  # Calculation of statistics
  pred.stats <- all.preds %>%
    dplyr::group_by(source, sample) %>%
    dplyr::summarise("Mean" = round(mean(pred), 3),
                     "SD" = round(stats::sd(pred), 3),
                     "Q2.5" = round(stats::quantile(pred, probs = 0.025),3),
                     "Q5" = round(stats::quantile(pred, probs = 0.05),3),
                     "Q25" = round(stats::quantile(pred, probs = 0.25),3),
                     "Q50" = round(stats::quantile(pred, probs = 0.50),3),
                     "Q75" = round(stats::quantile(pred, probs = 0.75),3),
                     "Q95" = round(stats::quantile(pred, probs = 0.95),3),
                     "Q97.5" = round(stats::quantile(pred, probs = 0.975),3)) %>%
    as.data.frame()


  if(isTRUE(save_pred)){
    file.name <- "JAGS_prevision"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(all.preds, paste(path, paste(file.name, ".csv", sep = ""), sep="/"), row.names = F)
  }

  if(!missing(path)){
    file.name <- "JAGS_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(pred.stats, paste(path, paste(file.name, ".csv", sep = ""), sep="/"), row.names = F)
  }
  return(pred.stats)
}
