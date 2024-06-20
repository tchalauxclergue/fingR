#' Evaluation of source groups prediction quality
#'
#' Calculate prediction quality statistics index to assess model prediction quality.
#'
#' Call *fingR::eval.mix* function for the calculation of prediction statistics.
#'
#' @param df.obs A data.frame with the observed source group contributions.
#' @param df.pred A data.frame with the predicted source group contributions.
#' @param by A character vector of variables to join by within the dplyr::left_join.
#' @param path Connection open for writing the test results data.frame. If "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding A character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#' @import dplyr
#' @import utils
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
eval.groups <- function(df.obs, df.pred, by, path, note, fileEncoding = "latin1"){

  require(dplyr)
  require(utils)

  # correct proportions
  if(max(df.obs[,2:ncol(df.obs)]) > 1 & !(max(df.pred[, 2:ncol(df.pred)]) > 1)){
    df.obs[,2:ncol(df.obs)] <- df.obs[,2:ncol(df.obs)] / 100
  }
  
  # look for source groups names
  groups <- levels(as.factor(colnames(df.obs)[2:ncol(df.obs)]))

  colnames(df.obs)[2:ncol(df.obs)] <- paste("Obs", colnames(df.obs)[2:ncol(df.obs)], sep="_")
  
  # Look what is the type(s)
  n <- (ncol(df.pred)-1)/length(groups) # The number of prediction type

  types <- colnames(df.pred)[2:ncol(df.pred)] # all columns
  for(grp in groups){ # remove the groups classifications names
    types <- base::gsub(grp, "", types)
  }
  for(ponct in c("[_]", "[-]", "[.]", "[[]","[]]")){
    types <- base::gsub(ponct, "", types)
  }
  types <- base::unique(types) # only keep unique values


  # joining the two data set so we are sure that the sample order is the same
  df.OP <- dplyr::left_join(df.obs, df.pred, by)

  stats.df <- c()
  group.name <- c()
  type.name <- c()

  for(tp in types){
    for(grp in groups){
      stats <- fingR::eval.mix(obs  = df.OP[[ colnames(df.OP)[grepl(grp, colnames(df.OP)) & grepl("Obs", colnames(df.OP))] ]], # the observation columns
                               pred = df.OP[[ colnames(df.OP)[grepl(grp, colnames(df.OP)) & grepl(tp,    colnames(df.OP))] ]])

      stats.df <- rbind(stats.df, stats)

      group.name <- c(group.name, grp)
      type.name <- c(type.name, tp)
    }
  }
  stats.df <- cbind(cbind("Type" = type.name, "Source" = group.name), stats.df)


  if(!missing(path)){
    file.name1 <- "ObsPred"
    file.name2 <- "stats"
    if(!missing(note)){
      file.name1 <- paste(file.name1, note, sep="_")
      file.name2 <- paste(file.name2, note, sep="_")
    }
    utils::write.csv(df.OP, paste(path, file.name1, ".csv", sep=""), row.names = F, fileEncoding = fileEncoding)
    utils::write.csv(stats.df, paste(path, file.name2, ".csv", sep=""), row.names = F, fileEncoding = fileEncoding)
  }
  return(stats.df)
}
