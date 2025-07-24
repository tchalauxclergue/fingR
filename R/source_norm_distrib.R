#' Normal distributed source samples
#'
#' Generate artificial (multivariate) normal distributed source samples.
#'
#' @param data a data.frame containing all the source properties values.
#' @param class a character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param tracers a vector listing all the tracers column names.
#' @param n a numeric value, the number of samples generated.
#' @param multivar a boolean, TRUE to generate multivariate normal distribution of source properties (FALSE, default).
#'
#' @author Thomas Chalaux-Clergue
#'
#' @references Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
#'
#' @export
source.norm.distrib <- function(data, class, tracers, n = 2500, multivar = FALSE){

  require(dplyr)

  MND.sources = NULL
  MND.sources.L <- list()

  # for every class type
  for (lvl in levels(as.factor(data[[class]]))) {

    MND <- data %>%
      dplyr::filter(.data[[class]] == lvl) %>% # keep only one class
      dplyr::select(-setdiff(colnames(data), tracers)) %>% # delete every column which is not a tracer
      log()  # calculate the log of every tracer

    ### calculate Mean and SD of log-transformed data
    mu <- as.numeric(summarize_all(MND, mean))

    source.name <- lvl

    if (multivar == FALSE) { # normal distribution
      sigma <- as.matrix(summarize_all(MND, sd)) # each property standard variation

      # generate 2500 normal distributed samples for each property
      norm.d <- c()
      for (i in 1:length(colnames(sigma))) {
        norm.d <- cbind(norm.d, rnorm(n = 2500, mean = mu[i], sd = sigma[i]))
      }
      colnames(norm.d) <- colnames(sigma)

      assign(source.name, exp(norm.d))

    }else{ # if multivariate distribution
      require(MASS)

      sigma <- cov(MND) #  properties covariance
      assign(source.name, exp(mvrnorm(n = 2500, mu, sigma)))
    }
    MND.sources <- rbind(MND.sources, lvl)
  }

  mean.sources <- MND.sources[,c(1)]
  MND.sources.L <- mget(mean.sources)
  
  to.return <- list(c(mean.sources), c(MND.sources.L))
  names(to.return) <- c("levels", "values")

  return(to.return)
}
