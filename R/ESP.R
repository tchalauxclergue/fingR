#' Encompassed sample predictions
#'
#' Calculate the number of elements from prediction (pred) that lies within the observed (obs) range for each source
#'
#' @param obs,pred Two data.frame containing numerical values.
#' @param sources,source.obs,sources.pred Character or vector of character indicating the column(s) that contains source labels. If source labels differ in obs and pred, indicate them.
#' @param count Boolean or Character. Return percentage when FALSE (default) and count when TRUE. Or "Both" if user want both.
#' @param digits Integer indicating the number of decimal places (round) or significant digits (signif) to be used. For round, negative values are allowed (see base::Round for more details).
#'
#' @author Thomas Chalaux-Clergue
#' 
#' @export
ESP <- function(obs, pred, sources, sources.obs, sources.pred, count = FALSE, digits = 0, split = "_", source.pos){

  if(!missing(sources)){
    sources.obs <- sources
    sources.pred <- sources
  }

  esp.1 <- list()
  esp.2 <- list() # if "both"
  for(i in 1:length(sources.obs)){
    if(toupper(count) == "BOTH"){
      esp.1[[sources.obs[i]]] <- esp.values(obs = obs[[sources.obs[i]]], pred = pred[[sources.pred[i]]], count = TRUE, digits)
      esp.2[[sources.obs[i]]] <- esp.values(obs = obs[[sources.obs[i]]], pred = pred[[sources.pred[i]]], count = FALSE, digits)
    }else{
      esp.1[[sources.obs[i]]] <- esp.values(obs = obs[[sources.obs[i]]], pred = pred[[sources.pred[i]]], count = count, digits)
    }
  }

  # arrange ESP values and name rows
  esp <- as.data.frame(esp.1)
  if(count == TRUE){
    rownames(esp) <- "ESP.Number"
  }else{
    rownames(esp) <- "ESP.Percentage"
  }
  
  if(toupper(count) == "BOTH"){
    esp <- rbind(esp, as.data.frame(esp.2))
    rownames(esp) <- c("ESP.Number", "ESP.Percentage")
  }
  
  esp <- as.data.frame(t(esp))
  
  srcs <- c()
  for(i in 1:nrow(esp)){
    srcs <- c(srcs, unlist(strsplit(rownames(esp)[i], split = split))[2])
  }
  
  esp <- cbind("Source" = srcs, esp)
  
  return(esp)
}
