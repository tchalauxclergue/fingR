#' Median criterion range test
#'
#' Assess the conservativity of a property based on the comparison of the sources and target medians.
#' The property is conservative if the target median value is between the highest and lowest sources medians.
#'
#' @param target a (non-empty) numeric vector of target values.
#' @param sources a (non-empty) numeric vector of source values.
#' @param class name of the source classes columns in sources.
#' @param alternative a character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.median <- function(target, sources, class, alternative = "single"){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label
  lvl <- fingR::lvl.signif(sources[[property]])

  # sources median
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(stats::median)))

  source.bounds[2] <- round(source.bounds[2], lvl)

  #get min of min and max of max add some measurement error if set (MM.error)
  bound.s <- c(min(source.bounds[2]), max(source.bounds[2]))

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- bound.s[1] <= target & target <= bound.s[2]
    resu[["RT.pass"]] <- !(FALSE %in% resu[["sample.RT"]])

  }else if(alternative == "population"){ # consider target as a population
    resu <- bound.s[1] <= round(stats::median(target), lvl+3) & round(stats::median(target), lvl+3) <= bound.s[2]
  }
  return(resu)
}
