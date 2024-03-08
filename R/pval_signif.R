#' Significance of p value
#'
#' Returns p value signif level significance level code.
#' 0.10 < p value - " "
#' 0.05 < p value < 0.10 - "."
#' 0.01 < p value < 0.05 - "*"
#' 0.001 < p value < 0.01 - "**"
#' p value < 0.001 - "***"
#'
#' @param p.value numeric value of p-value
#'
#' @return character significance code.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
#'
pval.signif <- function(p.value){
  signif <- ifelse(is.na(p.value), NA,
                   ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        ifelse(p.value < 0.1, ".", "")))))

  return(signif)
}
