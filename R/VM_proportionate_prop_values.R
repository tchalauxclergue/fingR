##' Virtual mixtures proportionate property values
#'
#' Generate virtual mixtures property values using source group property mean and standard deviation.
#'
#' @param data A data frame containing the dataset.
#' @param class A character string specifying the column name for the class.
#' @param tracers A character vector specifying the tracer columns to be used. Uncertainty values names should be consistent with value names. The uncertainty should be set as a prefix (i.e. uncertainty + property) or as a suffix (i.e. property + uncertainty).
#' @param contributions A data frame specifying the contribution of each source. If missing, contributions are generated.
#' @param VM.name A character string specifying the name for the virtual mixture column.
#' @param uncertainty.suffix,uncertainty.prefix A character string or vector specifying the uncertainty suffix OR prefix used to indicate uncertainty analyses (e.g. prop_SD -> "_SD" OR SD_prop -> "SD_").
#'
#' @returns A list of two data.frame: artificial mixtures *property* values and artificial mixtures properties measurement *uncertainty*.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @references Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
#'
#' @export
VM.proportionate.prop.values <- function(data, class, tracers, contributions, VM.name, uncertainty.suffix, uncertainty.prefix){
  
  require(dplyr)
  require(tidyr)
  
  groups <- levels(as.factor(data[[class]]))
  
  df.value <- data %>%
    dplyr::group_by(.data[[class]]) %>% # keep only one class
    dplyr::select(-all_of(setdiff(colnames(data),c(class, tracers)))) %>%
    dplyr::summarise(across(everything(), list(Mean = mean))) # calculate mean for each source group
  
  df.SD <- data %>%
    dplyr::select(-all_of(setdiff(colnames(data), tracers))) %>%
    dplyr::summarise(across(everything(), list(SD = sd))) # calculate mean for each source group
  
  for(var in tracers){ # round property mean value according to the initial data set
    
    var.prop <- paste0(var, "_Mean") # adapt label to df.value
    
    if(sum(startsWith(var.prop, colnames(df.value)) & endsWith(var.prop, colnames(df.value))) == 1){
      # Correct var in df.value
      df.value[,which(startsWith(var.prop, colnames(df.value)) & endsWith(var.prop, colnames(df.value)))] <- round(df.value[,which(startsWith(var.prop, colnames(df.value)) & endsWith(var.prop, colnames(df.value)))], fingR::lvl.signif(data[[var]]))
      
    }else if(sum(startsWith(var.prop, colnames(df.value)) & endsWith(var.prop, colnames(df.value))) > 1){
      # If there are multiple solution - generate an error
      stop(sprintf("The property label '%s' was mistaken with other labels : %s.\n  Please modify '%s' to avoid confusion.\n ",
                   var,
                   replacement = "", x = paste("'", colnames(df.value)[grepl(var, colnames(df.value))[1:(sum(grepl(var, colnames(df.value)))-1)]], "'", sep = "", collapse = ", "),
                   var))
    }else{ stop(sprintf("The property label '%s' was not found in the data", var)) }
    
    # Correct var in df.SD
    var.SD <- var
    if(!missing(uncertainty.suffix)){
      var.SD <- paste0("^", var, paste0("(?:", paste(uncertainty.suffix, collapse ="|\\"), ")?$")) # add uncertainty suffix to var name
    }else if(!missing(uncertainty.prefix)){
      var.SD <- paste0("^", paste0("(?:", paste(uncertainty.prefix, collapse ="|\\"), ")?$"), var) # add uncertainty prefix to vqar name
    }
    
    if(sum(grepl(var.SD, colnames(df.SD))) == 1){
      df.SD[,which(base::grepl(var.SD, colnames(df.SD)))] <- round(df.SD[,which(base::grepl(var.SD, colnames(df.SD)))], fingR::lvl.signif(data[[var]]))
    }else if(sum(grepl(var.SD, colnames(df.SD))) > 1){
      stop(sprintf("The uncertainty label '%s' (%s) was mistaken with other labels : %s.\n  Please modify '%s' to avoid confusion.\n ",
                   var,
                   if(!missing(uncertainty.suffix)){paste("+", uncertainty.suffix, collapse = ", ", sep = "")}else if(!missing(uncertainty.prefix)){paste("+", uncertainty.prefix, collapse = ", ", sep = "")}else{"no suffix/prefix were indicated"},
                   paste("'", colnames(df.SD)[grepl(var.SD, colnames(df.SD))[1:(sum(grepl(var.SD, colnames(df.SD)))-1)]], "'", sep = "", collapse = ", "), var))
      
    }else{ stop(sprintf("The uncertainty label '%s' was not found in the data", var)) }
  }
  
  dt.mean <- df.value %>%
    dplyr::select(.data[[class]], all_of(paste0(tracers, "_Mean"))) %>%
    tidyr::pivot_longer(cols = paste0(tracers, "_Mean"), names_to = "Property") %>% # manage to make it as groups property values are in columns
    tidyr::pivot_wider(names_from = class, values_from = value)
  
  dt.sd <- df.SD %>%
    dplyr::select(all_of(paste0(tracers, "_SD"))) %>%
    tidyr::pivot_longer(cols = paste0(tracers, "_SD"), names_to = "Property")
  
  dt.mean$Property <- gsub("_Mean", "", dt.mean$Property) # simplify property name by removing Mean indication
  dt.sd$Property <- gsub("_SD", "", dt.sd$Property) # simplify property name by removing sd indication
  
  
  # create value dataframe
  df.value <- NULL
  df.value <- as.data.frame(matrix(nrow = nrow(contributions), ncol = (length(tracers)+1))) # row for VM and columns for properties
  colnames(df.value) <- c(VM.name, tracers)
  df.value[[VM.name]] <- contributions[[VM.name]]
  
  # groups <- c("Forest", "Subsoil",  "Undecontaminated")
  
  for(mix in 1:nrow(contributions)){ # for each VM
    for(var in tracers){ # for each property
      val <- 0
      for(grp in groups){ # for each source group
        val <- val + (contributions[mix, grp] * dt.mean[which(dt.mean$Property == var), grp]) # multiplicate source contribution with source mean value
      }
      df.value[mix, var] <- val #round(val, fingR::lvl.signif(data[[var]]))
    }
  }
  
  for(var in tracers){ # round property mean value according to the initial data set
    # Correct var in df.value - no need of securities since it was already checked
    df.value[,which(grepl(var, colnames(df.value)))] <- round(df.value[,which(grepl(var, colnames(df.value)))], fingR::lvl.signif(data[[var]]))
  }
  
  
  
  # creat sd dataframe
  df.sd <- as.data.frame(matrix(nrow = nrow(contributions), ncol = (length(tracers)+1))) # row for VM and columns for properties
  colnames(df.sd) <- c(VM.name, tracers)
  df.sd[[VM.name]] <- contributions[[VM.name]]
  
  for(var in tracers){ # for each property
    df.sd[[var]] <- dt.sd[[which(dt.sd$Property == var), "value"]]
  }
  
  result <- list(df.value, df.sd)
  names(result) <- c("property", "uncertainty")
  
  return(result)
}

