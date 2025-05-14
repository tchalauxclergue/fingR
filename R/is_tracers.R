#' Is tracers
#'
#' Create a list of vector(s) containing conversative and discriminant properties.
#'
#' @param cons a vector or a list of vector that contain(s) conservatives properties
#' @param discrim a vector or a list of vector that contain(s) discriminant properties
#' @param note A character string to add a note at the start of test label (not set - default).
#'
#' @return data.frame. The results of the different test.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
is.tracers <- function(cons, discrim, note){

  resu <- list()

  for(c in names(cons)){
    for(d in names(discrim)){
      resu[[paste(c, d, sep = "_")]] <- intersect(cons[[c]], discrim[[d]])
    }
  }

  if(!missing(note)){
    names(resu) <- paste0(note, ".", names(resu))
  }

  return(resu)
}
