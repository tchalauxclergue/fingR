#' Mean criterion range test
#'
#' Assess the conservativity of a property based on the comparison of the sources and target means.
#' All data is transform to the log as comparing means underlay that distribution is assessed to be Normal.
#' The property is conservative if the mean of log of target value is between the highest and lowest means of log of sources values.
#'
#' @param target a (non-empty) numeric vector of target values.
#' @param sources a (non-empty) numeric matrix/data.frame of sources values and classes.
#' @param class name of the source classes columns in sources.
#' @param alternative a character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).
#' @param shift a numeric value of the size of shift to correct the minimum value to do log transformation, .1 (default)
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.mean <- function(target, sources, class, alternative = "single", shift = .1){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label
  lvl <- fingR::lvl.signif(sources[[property]])

  # convert to log to unsure Normal distribution
  sources[[property]] <- round(to.log(sources[[property]], shift), lvl+3)
  target <- round(to.log(target, shift), lvl+3)

  # sources mean
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(mean)))


  #get min of min and max of max add some measurement error if set (MM.error)
  bound.s <- round(c(min(source.bounds[2]), max(source.bounds[2])), lvl+3)

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- base::ifelse(bound.s[2] < target, "high", base::ifelse(target < bound.s[1], "low", TRUE))
    resu[["RT.pass"]] <- !(FALSE %in% c(bound.s[1] <= target & target <= bound.s[2])) # check if all are TRUE

  }else if(alternative == "population"){ # consider target as a population
    resu <- bound.s[1] <= round(mean(target), lvl+3) & round(mean(target), lvl+3) <= bound.s[2]
  }
  return(resu)
}
