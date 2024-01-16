#' Whiskers criterion range test
#'
#' Performs a range test based on the whiskers criterion.
#'
#' @param target a (non-empty) numeric vector of target values.
#' @param sources a (non-empty) data.frame with information of source classes and property value.
#' @param class a character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param alternative a character that indicates if range test should be apply on each sample (single) or should calculates the criterion on all samples (population).
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.whiskers <- function(target, sources, class, alternative = "single"){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label
  lvl <- fingR::lvl.signif(sources[[property]])
  
  #source whiskers
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(min = min, max = max, Q1 = ~stats::quantile(., .25), Q3 = ~stats::quantile(., .75))))

  source.bounds["l.whisk"] <- round(round(source.bounds[4], lvl) - 1.5*(round(source.bounds[5],lvl)-round(source.bounds[4],lvl)), lvl) # lower whisker
  source.bounds["u.whisk"] <- round(round(source.bounds[5], lvl) + 1.5*(round(source.bounds[5],lvl)-round(source.bounds[4],lvl)), lvl) # upper whisker

  #get min and max of max of source groups
  whiskers.s <- source.bounds %>%
    dplyr::rowwise() %>%
    dplyr::summarise(lower = max(.data[[colnames(source.bounds)[2]]], .data[[colnames(source.bounds)[6]]]), # max(min, lower whisker)
              upper = min(.data[[colnames(source.bounds)[3]]], .data[[colnames(source.bounds)[7]]])) # min (max, upper whisker)

  bound.s <- c(min(whiskers.s$lower), max(whiskers.s$upper))

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- bound.s[1] <= target & target <= bound.s[2]
    resu[["RT.pass"]] <- !(FALSE %in% resu[["sample.RT"]])

  }else if(alternative == "population"){ # consider target as a population
    t.Q25 <- round(stats::quantile(target, .25),lvl)
    t.Q75 <- round(stats::quantile(target, .75),lvl)

    target.lower <- max(min(target), round(t.Q25 - 1.5*(t.Q75 - t.Q25)),lvl)
    target.upper <- min(max(target), round(t.Q75 + 1.5*(t.Q75 - t.Q25)),lvl)

    resu <- bound.s[1] <= target.lower & target.upper <= bound.s[2]
  }
  return(resu)
}
