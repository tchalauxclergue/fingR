#' Hinge range test
#'
#' Performs a range test based on the hinge/interstats::quantile range (IQR) criterion.
#'
#' @param target a (non-empty) numeric vector of target values.
#' @param sources a (non-empty) data.frame with information of source classes and property value.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param h.range A numeric value of the population percentage within the hinge
#' @param alternative A character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).
#'
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.hinge <- function(target, sources, class, h.range, alternative = "single"){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label
  lvl <- lvl.signif(sources[[property]])
  # set the probability associated with setted hinge range (h.range)
  prob.hinge <- c((50 - h.range/2)/100, (50 + h.range/2)/100)

  #source stats::quantiles
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(~stats::quantile(., prob.hinge[1]), ~stats::quantile(., prob.hinge[2]))))

  #get min and max of max of source groups
  bound.s <- round(c(min(source.bounds[[2]]), max(source.bounds[[3]])), lvl)

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- bound.s[1] <= target & target <= bound.s[2]
    resu[["RT.pass"]] <- !(FALSE %in% resu[["sample.RT"]])

  }else if(alternative == "population"){ # consider target as a population
    resu <- unname(bound.s[1] <= round(stats::quantile(target, prob.hinge[1]),lvl) & round(stats::quantile(target, prob.hinge[2]),lvl) <= bound.s[2])
  }
  return(resu)
}
