#' Graphic representation of CRPS
#'
#' Draw source groups observed contributions by CRPS values.
#'
#' @param obs A data.frame with observed values for each sample and each source group.
#' @param crps A data.frame with CRPS values for each sample and each source group.
#' @param by A character vector of variables to join by. Please refer to dplyr::mutate-joins vignette for more detailed explanations.
#' @param path Connection open for writing the test results data.frame, "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param width,height,units Plot size in units ("in", "cm", "mm", or "px"). If not supplied, uses the size of current graphics device.
#'
#' @author Thomas Chalaux Clergue
#'
#' @export
graph.crps <- function(obs, crps, by, path, note, units = c("cm"), width = 10, height = 10, line0 = TRUE){

  require(dplyr)
  require(ggthemes)

  OCRPS <- dplyr::left_join(obs, crps, by = by,
                            suffix = c("_OBS", "_CRPS"))

  groups <- colnames(crps)[2:ncol(crps)]

  for(grp in groups){
    plt <- ggplot() + aes(x = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("OBS", colnames(OCRPS))]]],
                   y = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("CRPS", colnames(OCRPS))]]]) +
      geom_point(pch = 1) +
      ## regression line
      geom_smooth(method = "loess", formula = y~poly(x,2), se = FALSE) +
      # scale graph
      scale_x_continuous(paste(grp, "proportion"), limits = c(0, 1), breaks = seq(0, 1, .2)) +
      scale_y_continuous("CRPS", limits = c(0, 1), breaks = seq(0, 1, .2)) +
      # theme
      theme_bw() +
      theme(legend.position = "none", axis.title = element_text(size = 11), axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 9, angle = 45, hjust = 1))

    if(isTRUE(line0)){
      plt <- plt + geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4)
    }

    file.name <- paste("contrib_crps", grp, sep = "_")
    if(!missing(note)){
      file.name <- paste(file.name, note, sep = "_")
    }
    ggsave(filename = paste(file.name, ".PDF", sep = ""), plot = plt, path = path, units = units, width = width, height = height)
  }
}
