
BMM.pred <- function(data, stats, sample.id, source, save.dir, note, fileEncoding = "latin1"){

  require(tidyr)

  if(missing(sample.id)){ sample.id <- colnames(data)[1] }
  if(missing(source)){ source <- colnames(data)[2] }

  if("Median" %in% stats){
    stats[which(stats == "Median")] <- "Q50"
  }else if("50%" %in% stats){
    stats[which(stats == "50%")] <- "Q50"
  }


  df.stats <- data %>% dplyr::select(dplyr::all_of(c(sample.id, source, stats)))

  df.stats <- tidyr::pivot_wider(df.stats, names_from = source, values_from = dplyr::all_of(stats)) %>%
    as.data.frame()


  if("Q50" %in% stats){
    stats[which(stats == "Q50")] <- "Median"
  }

  colnames(df.stats)[2:ncol(df.stats)] <- paste(stats, colnames(df.stats)[2:ncol(df.stats)], sep="_")

  # save results
  if(!missing(save.dir)){
    file.name <- "BMM_ordered_contrib"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(x = df.stats, file = paste0(save.dir, file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
  }
  return(df.stats)
}
