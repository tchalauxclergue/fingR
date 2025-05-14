#' Run range criteria
#'
#' This function is called within *is.conservative* and was not designed for human use.
#'
#' @param data.t A numeric vector with source property value
#' @param data.s A numeric vector with source property value or if alternative is set to "independent" a matrix with sources class and property value (2 columns).
#' @param criteria Indicates the test(s) that will be performed, "all" or a combination between: "MM", "hinge", "whiskers", "CI" (only merged), "OSDM" (only merged), "mean" (only independent), or "median" (only merged).
#' @param MM.error A numeric value indicating how much measurement error should be added to source value. No measurement error = 0.0 and 10 percent = 0.1.
#' @param hinge.range A vector listing tested percentages of population within the hinge, 70 -> c(70) (default).
#' @param CI.CLs A vector listing tested confidance interval level, 95 = c(95) (default).
#' @param alternative A character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and target information.
#' @param shift A numeric value of the size of shift to correct the minimum value to do log transformation, .1 (default)
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
RT.player <- function(data.t, data.s, criteria, MM.error, hinge.range, CI.CLs, alternative = "single", class, shift = .1){

  resu <- list()

  # minimum-maximum
  if("all" %in% criteria || "MM" %in% criteria){
    resu[["MM"]] <- fingR::RT.MM(target = data.t, sources = data.s, class = class, alternative = alternative)
  }
  # minimum-maximum and measurement error
  if("all" %in% criteria || "MMe" %in% criteria){
    resu[["MMe"]] <- fingR::RT.MM(target = data.t, sources = data.s, class = class, MM.error = MM.error, alternative = alternative)
  }
  # hinge
  if("all" %in% criteria || "hinge" %in% criteria){
    if(length(hinge.range) == 1){
      resu[["hinge"]] <- fingR::RT.hinge(target = data.t, sources = data.s, class = class, h.range = hinge.range, alternative = alternative)
    }else{for(h in hinge.range){# if different hinge range were asked
        resu[[paste0("hinge.", h)]] <- fingR::RT.hinge(target = data.t, sources = data.s, class = class, h.range = h, alternative = alternative)
    }}
  }
  ## whiskers
  if("all" %in% criteria || "whiskers" %in% criteria){
    resu[["whiskers"]] <- RT.whiskers(target = data.t, sources = data.s, class = class, alternative = alternative)
  }
  ## mean
  if("all" %in% criteria || "mean" %in% criteria){
    resu[["mean"]] <- RT.mean(target = data.t, sources = data.s, class = class, shift = shift, alternative = alternative)
  }
  ## 7 - MEAN SD test only for INDEPENDENT approach
  if("all" %in% criteria || "mean.sd" %in% criteria){
    resu[["mean.sd"]] <- RT.mean.sd(target = data.t, sources = data.s, class = class, shift = shift, alternative = alternative)
  }
  ## 8 - median test only for INDEPENDENT approach
  if("all" %in% criteria || "median" %in% criteria){
    resu[["median"]] <- RT.median(target = data.t, sources = data.s, class = class, alternative = alternative)
  }
  return(resu)
}

