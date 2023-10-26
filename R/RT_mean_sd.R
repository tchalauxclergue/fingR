#' Mean plus/minus one standard deviation range test
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
RT.mean.sd <- function(target, sources, class, alternative = "single", shift = .1){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label
  lvl <- lvl.signif(sources[[property]])

  # convert to log to unsure Normal distribution
  sources[[property]] <- round(to.log(sources[[property]], shift), lvl+3)
  target <- round(to.log(target, shift), lvl+3)

  # sources mean
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(mean, stats::sd)))
  source.bounds[,c(2:3)] <- round(source.bounds[,c(2:3)], lvl+3)

  #get min of min and max of max add some measurement error if set (MM.error)
  bound.s <- c(min(source.bounds[2]-source.bounds[3]), max(source.bounds[2]+source.bounds[3]))

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- bound.s[1] <= target & target <= bound.s[2]
    resu[["RT.pass"]] <- !(FALSE %in% resu[["sample.RT"]])

  }else if(alternative == "population"){ # consider target as a population
    resu <- bound.s[1] <= round(mean(target)-stats::sd(target), lvl+3) & round(mean(target)+stats::sd(target), lvl+3) <= bound.s[2]
  }
  return(resu)
}
