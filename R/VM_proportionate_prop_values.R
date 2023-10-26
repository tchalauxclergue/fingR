##' Virtual mixtures proportionate property values
#'
#' Generate virtual mixtures property values using source group property mean and standard deviation.
#'
#' @param data description
#' @param class description
#' @param tracers description
#' @param contributions description
#' @param VM.name description
#'
#' @returns A list of two data.frame: artificial mixtures *property* values and artificial mixtures properties measurement *uncertainty*.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @references Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
#'
#' @export
VM.proportionate.prop.values <- function(data, class, tracers, contributions, VM.name){

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
    df.value[,which(grepl(var, colnames(df.value)))] <- round(df.value[,which(grepl(var, colnames(df.value)))], fingR::lvl.signif(data[[var]]))
    df.SD[,which(grepl(var, colnames(df.SD)))] <- round(df.SD[,which(grepl(var, colnames(df.SD)))], fingR::lvl.signif(data[[var]]))
  }

  dt.mean <- df.value %>%
    dplyr::select(.data[[class]], all_of(paste0(tracers, "_Mean"))) %>%
    pivot_longer(cols = paste0(tracers, "_Mean"), names_to = "Property") %>% # manage to make it as groups property values are in columns
    pivot_wider(names_from = class, values_from = value)

  dt.sd <- df.SD %>%
    dplyr::select(all_of(paste0(tracers, "_SD"))) %>%
    pivot_longer(cols = paste0(tracers, "_SD"), names_to = "Property")

  dt.mean$Property <- gsub("_Mean", "", dt.mean$Property) # simplify property name by removing Mean indication
  dt.sd$Property <- gsub("_SD", "", dt.sd$Property) # simplify property name by removing sd indication


  # create value dataframe
  df.value <- as.data.frame(matrix(nrow = nrow(contributions), ncol = (length(tracers)+1))) # row for VM and columns for properties
  colnames(df.value) <- c(VM.name, tracers)
  df.value[[VM.name]] <- contributions[[VM.name]]

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
    df.value[[which(grepl(var, colnames(df.value)))]] <- round(df.value[[which(grepl(var, colnames(df.value)))]], fingR::lvl.signif(data[[var]]))
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

