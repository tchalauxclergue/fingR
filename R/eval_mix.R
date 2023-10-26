#' Evaluation of prediction quality
#'
#' Calculates prediction quality statistic indexes: mean error (ME), root mean squared error (RMSE), Pearson correlation coefficient (r2) and
#' Nash-Stucliff modelling efficiency coefficient (NSE).
#'
#' @param obs A numeric vector with observed values.
#' @param pred A numeric vector with predicted values
#'
#' @return A data.frame with
#'
#' @author Thomas Chalaux-Clergue and Rémi Bizeul
#'
#' @references Bennett ND, Croke BFW, Guariso G et al (2013) Characterising perfor- mance of environmental models. Environ Model Softw 40:1–20. https:// doi. org/ 10. 1016/j. envso ft. 2012. 09. 011
#'
#' @export
eval.mix <- function(obs, pred){

  require(stats)

  # Mean error (ME)
  ME <- round(mean(pred - obs, na.rm = TRUE), digits = 2)

  # Root mean squared error (RMSE)
  RMSE <- round(sqrt(mean((pred - obs)**2, na.rm = TRUE)), digits = 2)

  # Squared Pearson's correlation coefficient (r2)
  r2 <- round((stats::cor(pred, obs, method = "pearson", use  = "pairwise.complete.obs")**2), digits = 2)

  # Nash-Sutcliffe modelling efficiency coefficient (NSE)
  SSE <- sum((obs - pred)**2, na.rm = TRUE)
  SST <- sum((obs - mean(obs, na.rm = TRUE))**2, na.rm = TRUE)
  NSE <- round((1 - SSE/SST), digits = 2)

  return(data.frame("ME" = ME, "RMSE" = RMSE, "r2" = r2, "NSE" = NSE))
}
