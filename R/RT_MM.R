#' Minimum-maximum criterion range test
#'
#' Performs a range test based on the minimum-maximum criterion. For a single target, it should be above the lowest minimum and bellow the highest maximum of source groups. For a population, the minimum target sample should be above the lowest minimum value of source groups and the maximum target sample should be bellow the highest maximum of source groups.
#'
#' @param target A (non-empty) numeric vector of target values.
#' @param sources A (non-empty) data.frame with information of source classes and property value.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param MM.error A numeric value indicating how much measurement error should be added to source value. No measurement error = 0.0 and 10 percent = 0.1.
#' @param alternative A character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.MM <- function(target, sources, class, MM.error = 0, alternative = "single"){
  require(dplyr)

  property <- dplyr::setdiff(colnames(sources), class) #get the property label

  # sources MIN and MAX
  source.bounds <- as.data.frame(sources) %>%
    dplyr::group_by(.data[[class]]) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(property), .fns = list(min, max)))

  #get min of min and max of max add some measurement error if set (MM.error)
  bound.s <- round(c(min(source.bounds[[2]]*(1-MM.error)), max(source.bounds[[3]]*(1+MM.error))), lvl.signif(sources[[property]]))

  if(alternative == "single"){ # for each target sample
    resu <- list()
    resu[["sample.RT"]] <- base::ifelse(bound.s[2] < target, "high", base::ifelse(target < bound.s[1], "low", TRUE))
    resu[["RT.pass"]] <- !(FALSE %in% c(bound.s[1] <= target & target <= bound.s[2])) # check if all are TRUE

  }else if(alternative == "population"){ # consider target as a population
    resu <- bound.s[1] <= min(target) & max(target) <= bound.s[2]
  }
  return(resu)
}
