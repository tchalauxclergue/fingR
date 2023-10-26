#' Discriminant power
#'
#' Identify the strength properties discriminant power based on two-samples tests.
#'
#'
#' Calculates how many source groups combinations exists (b) and how many of these combinations are statistically different based on the p-value (a).
#' The discriminant power can be assessed the ratio between a/b values.
#' This function is called within *fingR::discriminant.test* function.
#'
#' @param data A data.frame with all source group pairs results from tests.
#' @param test.context A character vector that indicates in which context two-samples Kolmogorov-Smirnov tests ("KS") were realised: Kolmogorov-Smirnov tests ("KS") or to test Kruskal-Wallis assumption ("KW.KS").
#' @param min.discriminant A numeric value of the minimum number of statistically different tests (1 default) for KS context.
#' @param p.level A numeric value of the p-value significance level.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
discriminant.power <- function(data, test.context, min.discriminant = 1, p.level = .05){

  results.df <- data.frame()

  test.pval <- colnames(data)[grep("p.value", colnames(data), fixed = T)] # get tests names

  properties <- c(unlist(unique(data$Property))) # vector of properties
  n.sources <- length(unlist(unique(c(data$Source_A, data$Source_B)))) # number of sources class

  test.resu <- data.frame()
  for(property in properties){
    if(test.context == "KS"){
      test.sum <- sum(data[which(data$Property == property), test.pval] < p.level) # count groups that are statistically different
      position <- test.sum >= min.discriminant

      end.colnames <- c("n.diff.groups", "Kolmogorov.Smirnov_discriminant")
    }

    if(test.context == "KW.KS"){
      test.sum <- sum(data[which(data$Property == property), test.pval] > p.level) # count groups that are statistically non-differentiable
      position <- test.sum == n.sources # all groups are non-differentiable

      end.colnames <- c("n.similar.groups", "respect.KW.shape.assumption")
    }

    test.resu <- rbind(test.resu, c(property, test.sum, position))
  }
  colnames(test.resu) <- c("Property", end.colnames)

  return(test.resu)
}
