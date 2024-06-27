#' VM.builder
#'
#' This function generates virtual mixtures (VM) for sediment source fingerprinting studies. It can create normal distributed samples or simple proportional mixtures of source signatures. The results include property values, uncertainty values, and combined data frames. The function allows for saving the results and adding source samples to the virtual mixtures.
#'
#' @param data A data frame containing the dataset.
#' @param material A character string specifying the name of the material column in the dataset.
#' @param source.name A character string specifying the name of the source material. Default is "Source".
#' @param class A character string specifying the column name for the class.
#' @param tracers A character vector specifying the tracer columns to be used.
#' @param uncertainty A character vector specifying the uncertainty columns, if available.
#' @param contributions A data frame specifying the contribution of each source. If missing, contributions are generated.
#' @param VM.range A numeric vector of length 2 specifying the range of contributions. 
#' @param VM.step A numeric value specifying the step size for the contributions.
#' @param VM.name A character string specifying the name for the virtual mixture column.
#' @param add.sources A logical value indicating whether to add source samples to the virtual mixtures. Default is FALSE.
#' @param Normal.distrib.samples A logical value indicating whether to generate normal distributed samples. Default is FALSE.
#' @param multivar A logical value indicating whether to use multivariate normal distribution for samples. Default is FALSE.
#' @param n.norm An integer specifying the number of normal distributed samples to generate. Default is 2500.
#' @param step A numeric value specifying the step size for normal distributed samples. Default is 0.1.
#' @param save.dir A character string specifying the directory to save the results. If missing, results are not saved.
#' @param note A character string specifying additional notes for the saved files.
#' @param fileEncoding A character string specifying the file encoding. Default is "latin1".
#' @param RETURN A logical value indicating whether to return the results. Default is TRUE.
#'
#' @return A list of three data frames:
#' \describe{
#'   \item{property}{A data frame with the property values of the virtual mixtures.}
#'   \item{uncertainty}{A data frame with the uncertainty values of the virtual mixtures.}
#'   \item{full}{A data frame with the combined property and uncertainty values of the virtual mixtures.}
#' }
#'
#' @import reshape2
#' @import tibble
#' @import dplyr
#' 
#' @author Thomas Chalaux-Clergue
#' 
#' @export
VM.builder <- function(data, material, source.name = "Source", class, tracers, uncertainty, contributions, VM.range, VM.step, VM.name, add.sources = FALSE, Normal.distrib.samples = FALSE, multivar = FALSE, n.norm = 2500, step = .1, save.dir, note, fileEncoding = "latin1", RETURN = TRUE){

  require(reshape2)
  require(tibble)
  require(dplyr)

  # select sources samples
  df.sources <- data %>%
    dplyr::filter(.data[[material]] == source.name) %>% # select rows with source as material
    as.data.frame()

  sources <- data %>%
    dplyr::filter(.data[[material]] == source.name) %>% 
    dplyr::select(dplyr::all_of(class)) %>% unlist %>% as.factor %>% levels
  
  # If no contributions are given
  if(missing(contributions)){
    contributions <- fingR::VM.contrib.generator(n.sources = length(sources), min = VM.range[1], max = VM.range[2], step = VM.step, sources.class = sources, VM.name = VM.name, fileEncoding = fileEncoding,  return = TRUE, save = FALSE)
    VM.name <- colnames(contributions)[1]
  }

  # manage contribution levels as rate and not percentage
  if(max(contributions[, sources]) > 1){
    contributions[, sources] <- contributions[, sources]/100
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

  }else{ # Generate VM property values that are simple multiplication
    resu <- fingR::VM.proportionate.prop.values(data = df.sources, class = class, tracers = tracers, contributions = contributions, VM.name = VM.name)
    math.mix <- resu[[1]]
    math.mix.sd <- resu[[2]]
  }
  
  # Add the class to virtual mixtures data.frames
  math.mix <- math.mix %>% dplyr::mutate(!!class := "Virtual Mixture") %>% dplyr::relocate(class, .after = !!all_of(VM.name))
  math.mix.sd <- math.mix.sd %>% dplyr::mutate(!!class := "Virtual Mixture") %>% dplyr::relocate(class, .after = !!all_of(VM.name))
  
  
  # Correct sd names if uncertainty labels were set
  if(!missing(uncertainty)){
    colnames(math.mix.sd)[which(!colnames(math.mix.sd) %in% c(VM.name, class))] <- uncertainty
  }else{
    colnames(math.mix.sd)[which(!colnames(math.mix.sd) %in% c(VM.name, class))] <- paste0(colnames(math.mix.sd)[which(!colnames(math.mix.sd) %in% c(VM.name, class))], "_SD")
  }
  
  # Define the suffix if no uncertainty labels were set
  if(!missing(uncertainty)){
    VM.suffix <- c("", "")
  }else{
    VM.suffix <- c("", "_SD") # uncertainties are simply label as SD
  }
  
  math.full <- dplyr::left_join(math.mix, math.mix.sd, by = dplyr::join_by(!!VM.name, !!class), suffix = VM.suffix) # join mixture values and uncertainty values
  
  # Add source samples at the end of all the data frames
  if(isTRUE(add.sources)){
    math.mix <- dplyr::rows_append(math.mix,  data %>% dplyr::filter(.data[[material]] == source.name) %>% dplyr::select(dplyr::all_of(colnames(math.mix))))
    
    if(!missing(uncertainty)){
      math.mix.sd <- dplyr::rows_append(math.mix.sd,  data %>% dplyr::filter(.data[[material]] == source.name) %>% dplyr::select(dplyr::all_of(colnames(math.mix.sd))))
      math.full <- dplyr::rows_append(math.full,  data %>% dplyr::filter(.data[[material]] == source.name) %>% dplyr::select(dplyr::all_of(colnames(math.full))))
    }else{
      math.mix.sd <- dplyr::rows_append(math.mix.sd,  data %>% dplyr::filter(.data[[material]] == source.name) %>% dplyr::select(dplyr::all_of(c(VM.name, class))))
      math.full <- dplyr::rows_append(math.full,  data %>% dplyr::filter(.data[[material]] == source.name) %>% dplyr::select(dplyr::all_of(c(VM.name, class, tracers))))
    }
  }
  
  # Saving virtual mixtures properties
  if(!missing(save.dir)){
    file.name1 <- "VM_properties"
    file.name2 <- "VM_properties_SD"
    file.name3 <- "VM_properties_full"
    if(!missing(note)){
      file.name1 <- paste(file.name1, note, sep="_")
      file.name2 <- paste(file.name2, note, sep="_")
      file.name3 <- paste(file.name3, note, sep="_")
    }
    utils::write.csv(math.mix, paste0(save.dir, paste0(file.name1, ".csv")), row.names = FALSE, fileEncoding = fileEncoding)
    utils::write.csv(math.mix.sd, paste0(save.dir, paste0(file.name2, ".csv")), row.names = FALSE, fileEncoding = fileEncoding)
    utils::write.csv(math.full, paste0(save.dir, paste0(file.name3, ".csv")), row.names = FALSE, fileEncoding = fileEncoding)
  }

  result <- list(math.mix, math.mix.sd, math.full)
  names(result) <- c("property", "uncertainty", "full")


  if(isTRUE(RETURN)){
    return(result)
  }
}
