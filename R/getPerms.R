#' Permutation calculator
#'
#' Generate all the permutations of the given data.frame.
#'
#' @param x A data.frame (ncols > 1) with numerir/character values.
#'
#' @return A data.frame with all the permutations row bound to x.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}
