#' VM.contrib.generator
#'
#' Generates a matrix of virtual mixture source contributions based on specified parameters.
#'
#' @param n.sources Integer. The number of source levels (from 2 to 4).
#' @param min Numeric. Minimum contribution value (default is 0).
#' @param max Numeric. Maximum contribution value (default is 100).
#' @param step Numeric. Step between two contribution levels (default is 5).
#' @param sources.class Character vector. Optional names for the source classes.
#' @param VM.name Character. Name of the column containing virtual mixture labels (default is "Sample_name").
#' @param save.dir Connection open for saving artificial mixtures property values and standard deviation (SD) data.frames. If "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note Character. Optional additional note to append to the file name.
#' @param fieEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written (default is "latin1").
#' @param return Logical. Whether to return the result matrix (Default is TRUE).
#' @param save Logical. Whether to save the result matrix to a file (Default is TRUE).
#' 
#' @return A matrix (or data frame) of virtual mixture contributions if \code{return} is set to TRUE.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
VM.contrib.generator <- function(n.sources, min = 0, max = 100, step = 5, sources.class, VM.name = "Sample_name", save.dir, note, fileEncoding = "latin1", return = TRUE, save = TRUE){

  # Intialise an empty matrix for contributions
  VM.contrib <- matrix(nrow = 0, ncol = n.sources)

  # Generate all possible contribution for source number 1
  S1 <- base::seq(from = min, to = max, by = step)

  # If there are 2 sources
  if(n.sources == 2){
    VM.contrib <- cbind(S1, rev(S1)) # Pair each contribution of S1 with its reverse (i.e. S2)

  # If there are 3 sources
  }else if(n.sources == 3){
    for(i in seq_along(S1)){
      S2 <- base::seq(from = min, to = max-S1[i], by = step)
      VM.contrib <- rbind(VM.contrib, cbind(S1[i], S2, rev(S2))) # Add to the matrix of contributions
    }
    
  # If there are 4 sources
  }else if(n.sources == 4){
    for(i in seq_along(S1)){
      S2 <- base::seq(from = min, to = max-S1[i], by = step)
      for(j in seq_along(S2)){
        S3 <- base::seq(from = min, to = max-S1[i]-S2[j], by = step)
        VM.contrib <- rbind(VM.contrib, cbind(S1[i], S2[j], S3, rev(S3))) # Add to the matrix of contributions
      }
    }
  }

  # Set column names for the matrix of contribution
  if(missing(sources.class)){
    colnames(VM.contrib) <- paste0("S", seq(from = 1, to = n.sources, by = 1))
  }else{
    if(length(sources.class) != n.sources){base::warning("The number of source names in 'sources.class' differ from that of 'n.sources'.")}
    colnames(VM.contrib) <- sources.class[1:n.sources]
  }

  # Generate virtual mixtures names
  VM.names <- as.data.frame(paste0("VM-", base::sprintf("%0*d", base::nchar(nrow(VM.contrib)), 1:nrow(VM.contrib)))) # generate VM names with 0 to keep the order (eg. ...VM-099, VM-100,...)

  VM.contrib <- cbind(VM.names, VM.contrib) # Combine names with the contribution matrix
  colnames(VM.contrib)[1] <- VM.name # Change column name according to 'VM.name'

  # Save data if 'save.dir' is provided and 'save' is TRUE
  if(!missing(save.dir) & isTRUE(save)){
    file.name <- "VM_contributions"
    if(!missing(note)){ # If the user adds a note
      file.name <- paste(file.name, note, sep = "_") # Append note to the file name
    }
    utils::write.csv(VM.contrib, file = paste0(save.dir, paste0(file.name, "_", nrow(VM.contrib), ".csv")), row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
  }

  if(isTRUE(return)){ return(VM.contrib) }
}
