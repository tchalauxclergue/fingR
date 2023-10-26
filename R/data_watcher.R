#' Evaluating measurement uncertainty
#'
#' Helps to consider the use of a property according to its measurement uncertainty.
#'
#'
#' Three types of errors are watched:
#' (a) missing values,
#' (b) positive property containing negative values,
#' (c) all negative values property (which require to be changed to positive for modelling),
#' (d) considering the uncertainty make some values virtually impossible (e.g. negative values within positive values property),
#' (e) relative size of the uncertainty compare to the value.
#'
#' @param data a data.frame containing all the data values.
#' @param properties a character string corresponding to the column containing the sources (source 1, source 2, etc) and target information.
#' @param prop.uncer a vector listing all the properties that will be evaluated.
#' @param rl.lvl a numeric value indicating the relative uncertainty
#'
#' @return The list of properties with missing values, virtually impossibles and the size of the uncertainty compared to the property values.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
data.watcher <- function(data, properties, prop.uncer, rl.lvl = 5){

  nas <- c() ; all.neg <- c() ; some.neg <- c() ; wd.unceror <- c() ; rl.unceror <- c() ; rl.unceror2 <- c()
  unceror.consideration <- F

  for(i in 1:length(properties)){
    prop <- properties[i]

    if(!missing(prop.uncer)){
      uncer <- prop.uncer[i]
    }


    #### NA VALUES
    if(TRUE %in% is.na(data[[prop]])){ #look if the sum of NA is not null
      nas <- c(nas, prop)
    }

    #### NEGATIVE VALUES
    if(min(data[[prop]], na.rm = T) < 0){ #if the smallest value is negative
      if(max(data[[prop]], na.rm = T) < 0){ #if the biggest value is also negative
        # it means that all the values are negatives
        all.neg <- c(all.neg, prop)
      }else{
        # else it means that only some values are negatives
        some.neg <- c(some.neg, prop)
      }
    }

    ### WHEN UNCERTAINTY IS SET
    if(!missing(prop.uncer)){
      #### UNCERTAINTY MAKES PROPERTY TO VIRTUALLY IMPOSSIBLE
      lowest.unceror <- min(abs(data[[prop]]) - data[[uncer]], na.rm = T) # look at the lowest value of the difference between the measure and measurement uncertainty
      if(lowest.unceror < 0){
        wd.unceror <- c(wd.unceror, prop)
      }

      ### UNCERTAINTY RELATIVE ABOUNDANCE
      rl <- max(abs(round(data[[uncer]]/data[[prop]]*100)), na.rm = T) # higher relative uncertainty
      if(rl.lvl < rl){ # if the higher relative uncertainty is bellow the
        rl.num <- sum(rl.lvl < abs(round(data[[uncer]]/data[[prop]]*100)), na.rm=T) # number of relative uncertainty above the threshold

        rl.unceror <- c(rl.unceror, c(rl)) # add the higher value
        names(rl.unceror)[length(rl.unceror)] <- prop # name vector as the property name

        rl.unceror2 <- c(rl.unceror2, c(rl.num)) # number of value above the threshold
        names(rl.unceror2)[length(rl.unceror2)] <- prop

      }
    }else{
      unceror.consideration <- T
    }
  }

  rl.unceror <- sort(rl.unceror, decreasing = T) # sort in descreasing order of the higher error
  rl.unceror <- rbind(rl.unceror, rl.unceror2) # bind higher value and number of value

  #### Listing data results
  phrase <- NULL
  if(length(nas) > 0){
    phrase <- paste("Following column(s) contain(s) NA values: ", paste(nas, collapse=", "), ".", sep="")

  }
  if(length(some.neg) > 0){
    phrase <- paste(phrase,
                    paste("Following column(s) contain(s) some negative values: ", paste(some.neg, collapse=", "), ".", sep=""),
                    sep="\n")
  }
  if(length(all.neg) > 0){
    phrase <- paste(phrase,
                    paste("Following column(s) contain(s) all negative values: ", paste(all.neg, collapse=", "),
                          ". The data will be transform to positive for model calculation.", sep=""),
                    sep="\n")
  }
  if(length(wd.unceror) > 0){
    phrase <- paste(phrase,
                    paste("Following column(s) have a measurement uncertainty that makes some values to be virtually impossible: ", paste(wd.unceror, collapse=", "), ".", sep=""),
                    sep="\n")
  }
  if(length(wd.unceror) > 0){
    phrase <- paste(phrase,
                    paste("Following column(s) have a relative measurement uncertainty above ", rl.lvl, "% (up to - number): ",
                          paste(paste(colnames(rl.unceror), " (max:", as.numeric(rl.unceror[1,]), "% - n:", as.numeric(rl.unceror[2,]), ")", sep=""), collapse = ", "),
                          ".", sep=""),
                    sep="\n")
  }
  if(isTRUE(unceror.consideration)){
    phrase <- paste(phrase,
                    "No measurement uncertainties were setted. We would like to remind the importance of measurement uncertainty considering the quality of a measure and its interpretation.",
                    sep="\n")
  }
  if(nchar(phrase) == 0){
    phrase <- "No irregulaties were found."
  }

  return(cat(phrase))
}
