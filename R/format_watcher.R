# verify if the data set is correctly structured for the mixing model
# which means no NA and negative values. Also if a tracer has all negative value it change the sign of this tracer.
heimdall <- function(data, properties){

  tr.na <- c() # vector of tracers with NA values
  tr.neg <- c() # vector of trancers with some negative values


  for(tracer in tracers_vector){

    #### NA VALUES
    if(sum(is.na(data[[tracer]])) > 0){ #look if the sum of NA is not null
      tr.na <- c(tr.na, tracer)
    }

    #### VEGATIVE VALUES
    if(sum(data[[tracer]] < 0) > 0){ # look if there are samples with negative values

      if(sum(data[[tracer]] < 0) != length(data[[tracer]])){ # if not all the samples are negatives
       neg.tf <- c(tr.neg, tracer) # list the tracer name

      }else{ # if all the samples are negatives
        data[[tracer]] <- -data[[tracer]]
      }

    }
  }


  #### ERROR MESSAGES

  # NA VALUES
  if(length(tr.na) > 0){
    stop(paste("tracer(s):", paste(tr.na, collapse=", "), "contain NA values. Theses rows need to be removed", sep=" "))
  }

  # NEGATIVE VALUES
  if(length(tr.neg) > 0){
    stop(paste("tracer(s):", paste(tr.neg, collapse=", "), "contain negatives values. Theses rows need to be removed", sep=" "))
  }


  return(data)
}
