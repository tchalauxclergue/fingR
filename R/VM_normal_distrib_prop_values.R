#' Virtual mixtures from normal distributed property values
#'
#' Generates virtual mixture property values using normally distributed source samples, based on tracer means and standard deviations per source group. This simulation is typically used for testing and validating sediment source apportionment models.
#'
#' @param data A `data.frame` containing source samples with tracer measurements and associated source group labels.
#' @param class A character string specifying the name of the column in `data` that defines source groups or classes.
#' @param tracers A character vector specifying the names of the tracer columns to be used for simulation (e.g., geochemical properties, isotopes).
#' @param n An integer defining the number of normally distributed samples to simulate per source group.
#' @param multivar Logical; if `TRUE`, the simulation accounts for the multivariate structure (covariances) of the tracers. If `FALSE`, tracers are simulated independently.
#' @param contributions A `data.frame` describing the contribution of each source group to each virtual mixture. One row per mixture, columns include a mixture ID and proportion values for each source group.
#' @param VM.name A character string giving the name of the column in `contributions` that contains the unique virtual mixture names (e.g., "Mix1", "Mix2").
#'
#' @return A named list of two `data.frame` objects:
#' \describe{
#'   \item{values}{Simulated tracer values for each virtual mixture (`Sample_name`).}
#'   \item{sd}{Associated standard deviations of simulated tracer values.}
#' }
#'
#' @author Thomas Chalaux-Clergue
#'
#' @references Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
#'
#' @export
VM.normal.distrib.prop.values <- function(data, class, tracers, n, multivar, contributions, VM.name){

  require(tibble)
  
  # create the normal distributed source samples
  MND.sources <- source.norm.distrib(data = data, class = class, tracers = tracers, n = n, multivar = multivar)
  sources <- MND.sources[[2]]


  colSdApply <- function(x, ...)apply(X = x, MARGIN = 2, FUN = sd, ...)

  MND.mix <- NULL
  MND.list <- list()
  am.mean <- NULL
  am.sd <- NULL

  for (mix in contributions[[VM.name]]) {
    # Create small table with source contributions
    MND <- contributions %>% # for one artificial mixture
      dplyr::filter(.data[[VM.name]] == mix) %>% # keep only one class
      reshape2::melt(id = 1) %>% # pivot longer the data frame
      dplyr::mutate(variable = as.factor(variable)) # change source type class into factor

    a.mix <- NULL

    for (lvl in MND$variable) { # for every source class
      # select one source group proportion
      tempo <- MND %>% # in the precedent data frame
        filter(variable == lvl) # select on source type

      MND.list <- lapply(sources[lvl], "*" , tempo$value) # apply the proportion of the source to all predicted samples properties

      a.mix <- c(a.mix, MND.list) # append the sample list of this source to am
    }

    a.mix <- Reduce("+", a.mix) # sum all the values associated to source into one result

    am.mu <- t(as.data.frame(colMeans(a.mix))) # calculate the mean of every tracer
    row.names(am.mu) <- mix # name the matrix row after lab mix name

    am.sigma <- t(as.data.frame(colSdApply(a.mix))) # calculate the standard deviation (SD) for every tracer with each column is a tracer
    row.names(am.sigma) <- mix

    am.mean <- rbind(am.mu, am.mean) # create a matrix with every tracer mean value for every lab mix
    am.sd <- rbind(am.sigma, am.sd) # same with sd
  }

  math.mix <- as.data.frame(am.mean) %>% tibble::rownames_to_column() # change format to data frame and say that lab mix are a column and not row name
  math.mix.sd <- as.data.frame(am.sd) %>% tibble::rownames_to_column() # same for sd
  colnames(math.mix)[1] <- "Sample_name"
  colnames(math.mix.sd)[1] <- "Sample_name"


  resu <- list(math.mix, math.mix.sd)
  names(resu) <- c("values", "sd")
  return(resu)
}
