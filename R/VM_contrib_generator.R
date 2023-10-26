#' Virtual mixtures proportion generator
#'
#' Generate proportion for artificial mixture according to the number of source groups, from 2 to 5.
#'
#'
#' Contributions are generated according to a scale range from 0 to 100% contribution for a source, with the *step* increments.
#' The other groups are determined as fractions of the remaining contribution (i.e. 1 - source A contribution) with decreasing importance of groups.
#' The value of the fraction is determined by the number of sources. The denominator is defined as: (number of sources - 1) * 2.
#' The numerator values are set according to the number groups: 3 groups - 3 and 1, 4 groups - 3, 2 and 1, 5 groups - 4, 3, 2 and 1.
#' Then, the permutation of this contribution scale are determine.
#'
#' @param n.sources A numeric value indicating the number of source groups.
#' @param groups A vector listing source groups names
#' @param step A numeric value indicating the step between two contributions, between 0 and 1 (default: 0.1).
#' @param save.dir A connection open for saving artificial mixtures property values and standard deviation (SD) data.frames. If "" save the file at working directory, if not set (default) the data.frame is not saved.
#' @param note A character string to add a note at the end of the file name (not set - default).
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded
#' as they are written, "latin1" (default).
#'
#' @return A data.frame with artificial mixture source contributions.
#'
#' @author Thomas Chalaux-Clergue
#'
#' @export
VM.contrib.generator <- function(n.sources, groups, step = .1, save.dir, note, fileEncoding = "latin1"){

  S1 <- sort(c(0.99, 0.01, seq(0, 1, by = step))) #create the sequence from 0 to 1 with the specified step
  #S1 <- S1[-c(1, length(S1))] #remove 0 and 1 because they are theorical not reachable

  frac <- (n.sources-1)*2 # a number to determine the different sources relative contributions

  if(n.sources == 2){
    S2 <- 1 - S1
    VM.prop <- data.frame(X1 = S1, X2 = S2)

  }else if(n.sources == 3){
    S2 <- (1 - S1) * 3/frac
    S3 <- (1 - S1) * 1/frac

    to.permut <- list(S1, S2, S3)

  }else if(n.sources == 4){
    S2 <- (1 - S1) * 3/frac
    S3 <- (1 - S1) * 2/frac
    S4 <- (1 - S1) * 1/frac

    to.permut <- list(S1, S2, S3, S4)

  }else if(n.sources == 5){
    S2 <- (1 - S1) * 4/frac
    S3 <- (1 - S1) * 3/frac
    S4 <- (1 - S1) * 2/frac
    S5 <- (1 - S1) * 1/frac

    to.permut <- list(S1, S2, S3, S4, S5)
  }

  if(n.sources > 2){
    permut <- as.data.frame(getPerms(to.permut)) # generate all the permutation

    # create the proportions data frame
    VM.prop <- data.frame(matrix(nrow = length(unlist(permut[1])), ncol = 0))
    for(i in 1:ncol(permut)){
      VM.prop <- cbind(VM.prop, unlist(permut[i]))
    }
  }

  # creating artifical mix names
  mix.names <- c()
  for(i in 1:nrow(VM.prop)){
    if(nchar(as.character(i)) < nchar(as.character(nrow(VM.prop)))){
      mix.names <- append(mix.names, paste("VM",
                                           paste0(paste(rep("0", nchar(as.character(nrow(VM.prop))) - nchar(as.character(i))), collapse = ""), i),
                                           sep = "-"))
    }else{
      mix.names <- append(mix.names, paste("VM", i, sep="-"))
    }
  }

  VM.prop <- cbind(mix.names, VM.prop)
  colnames(VM.prop) <- c("mix.names", groups)

  # save
  if(!missing(save.dir)){
    file.name <- paste("VM_proportions", nrow(VM.prop), sep = "_")
    if(!missing(note)){ # if the user add a note
      file.name <- paste(file.name, note, sep = "_") # if a note is added to the file name
    }
    write.csv(VM.prop, file = paste0(save.dir, paste0(file.name, ".csv")), na = "", row.names = FALSE, fileEncoding = fileEncoding) # saving results data.frame
  }
  return(VM.prop)
}
