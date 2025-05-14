#' BMM.gate.keeper
#'
#' Checks whether specified tracer in a dataset contain zero values. If so, returns an error message listing the affected properties and corresponding sample names.
#'
#' @param data Data frame. The dataset containing both source and target samples.
#' @param sample.id Character. The column name for sample identifiers.
#' @param tracers Character vector. The column names of the tracers used in the model.
#' 
#' @author Thomas Chalaux-Clergue
#'
#' @export 
BMM.gate.keeper <- function(data, sample.id, tracers){
  
  # List column(s) that contains zero value
  zero.vars <- tracers[base::sapply(tracers, function(var) {any(data[[var]] == 0.0, na.rm = TRUE)} )]
  
  # If some columns have zero values
  if(length(zero.vars) > 0.0){
    
    # list the location of the samples with zero values
    zero.cells <- base::sapply(zero.vars, function(var) {which(data[[var]] == 0.0)} )
    
    #Error message
    base::stop(paste0("\nThe following source sample(s) have zero value(s): ", paste0(names(zero.cells), " (", base::sapply(zero.cells, function(x){paste(paste0("'", data[x, sample.id], "'"), collapse = ", ")}), ")", collapse = ", "), ".\n",
                      "Such sample(s) must be managed to perform the log transformation before running BMM.\n"))
  }
  
  # else nothing happens
  return()
}
