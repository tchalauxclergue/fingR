#' Virtual mixtures property values generator
#'
#' Generate artificial mixture property values according to source samples properties.
#'
#'
#' Virtual mixture properties values are determined according to the multiplication of each source distribution (%) and a randomly selected value among the Normal distributed
#' samples (n = n.norm). Normal distributed samples are generated according to each source group characteristics (mean and standard deviation) within the *fingR::source.norm.distrib* function.
#'
#' @param data A data.frame containing all the source properties values, material and class.
#' @param material A character string corresponding to the column where the source samples can be differentiate from other samples.
#' @param source.name A character string corresponding to the way source samples are named in 'material': "Source" (default).
#' @param class A character string corresponding to the column that contains the sources classification groups (source A, source B...) and target.
#' @param tracers A vector listing all the tracers column names.
#' @param contributions A data.frame with the artificial mixture source groups contributions (from 0 to 1). If not set proportions will be generated according to the 'step'.
#' @param VM.name A character string corresponding to the column where artificial mixture are labelled.
#' @param Normal.distrib.samples description
#' @param multivar A boolean, TRUE to generate multivariate normal distribution of source properties (FALSE, default).
#' @param n.norm A numeric value corresponding the number of samples generated.
#' @param step A numeric value indicating the step between two contributions, between 0 and 1 (default: 0.1).
#' @param save.dir Connection open for saving artificial mixtures property values and standard deviation (SD) data.frames. If "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fieEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#' @param RETURN A boolean, when set to TRUE (default) return the artificial mixture property values. Set to FALSE for no return from the function.
#'
#' @returns A list of two data.frame: artificial mixtures *property* values and artificial mixtures properties measurement *uncertainty*.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @references Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
#'
#' @export
VM.builder <- function(data, material, source.name = "Source", class, tracers, contributions, VM.name, Normal.distrib.samples = FALSE, multivar = FALSE, n.norm = 2500, step = .1, save.dir, note, fileEncoding = "latin1", RETURN = TRUE){

  require(reshape2)
  require(tibble)
  require(dplyr)

  # select sources samples
  df.sources <- data %>%
    dplyr::filter(.data[[material]] == source.name) %>% # select rows with source as material
    as.data.frame()

  # If no contributions are given
  if(missing(contributions)){
    sources <- data %>%
      dplyr::filter(.data[[material]] == source.name) %>%
      dplyr::select(dplyr::all_of(class)) %>% unlist() %>% as.factor() %>% levels() # get source labels

    contributions <- fingR::VM.contrib.generator(n.sources = length(sources), groups = sources, step = step, save.dir = save.dir, note = note, fileEncoding = fileEncoding)
    VM.name <- colnames(contributions)[1]
  }

  # generate property values
  if(isTRUE(Normal.distrib.samples)){  # to generate normal distributed samples property value

    # correct if a tracer is fully negative
    GK <- fingR::gate.keeper(df.sources, tracers)
    df.sources <- GK[[1]]

    resu <- fingR::VM.normal.distrib.prop.values(data = df.sources, class = class, tracers = tracers, n = n.norm, multivar = multivar, contributions = contributions, VM.name = VM.name)
    math.mix <- resu[[1]]
    math.mix.sd <- resu[[2]]

    # rounding property values according to actual analysis
    for(i in tracers){
      lvl <- fingR::lvl.signif(data[[i]])
      math.mix[[i]] <- round(math.mix[[i]], lvl)
      math.mix.sd[[i]] <- round(math.mix.sd[[i]], lvl)
    }

    # return signs as given
    math.mix <- fingR::gate.keeper(math.mix, GK[[2]], inverter = TRUE)

    # order VM
    math.mix <- math.mix[order(math.mix[[1]], decreasing = FALSE),]
    math.mix.sd <- math.mix.sd[order(math.mix.sd[[1]], decreasing = FALSE),]

  }else{ # to generate VM property values that are simple multiplication
    resu <- fingR::VM.proportionate.prop.values(data = df.sources, class = class, tracers = tracers, contributions = contributions, VM.name = VM.name)
    math.mix <- resu[[1]]
    math.mix.sd <- resu[[2]]
  }

  #saving artificial mixture properties
  if(!missing(save.dir)){
    file.name1 <- "VM_properties"
    file.name2 <- "VM_properties_SD"
    if(!missing(note)){
      file.name1 <- paste(file.name1, note, sep="_")
      file.name2 <- paste(file.name2, note, sep="_")
    }

    utils::write.csv(math.mix, paste0(save.dir, paste0(file.name1, ".csv")), row.names = F, fileEncoding = fileEncoding)
    utils::write.csv(math.mix.sd, paste0(save.dir, paste0(file.name2, ".csv")), row.names = F, fileEncoding = fileEncoding)
  }

  result <- list(math.mix, math.mix.sd)
  names(result) <- c("property", "uncertainty")


  if(isTRUE(RETURN)){
    return(result)
  }
}


