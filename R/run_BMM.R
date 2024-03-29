#' Bayesian Mean Model
#'
#' Perform a Bayesian un-mixing model using the sum of squared relative error.
#' 
#'
#' @param data A data.frame containing all the data values.
#' @param class A character string corresponding to the column that contains the sources classes (source 1, source 2, etc) and mixture information.
#' @param mixture A character string corresponding to way "target" (default) are named.
#' @param sample.id  A character string corresponding to the column that contains sample name.
#' @param tracers A vector of character corresponding to the columns that contains tracers data.
#' @param uncertainty A vector of character corresponding to the columns that contains tracers uncertainties.
#' @param save.dir Connection open for writing the test results data.frame. If "" save the file at working directory. If not set (default) the data.frame is not saved.
#' @param note A character string to add note at the end of the file name (not set - default).
#' @param fileEncoding Character string, if non-empty declares the encoding to be used on a file (not a connection) so the character data can be re-encoded as they are written, "latin1" (default).
#' @param multivar A logical indicating whether to perform multivariate analysis (default: TRUE).
#' @param sd.threshold A numeric specifying the threshold for standard deviation calculation (default: 0.05).
#' @param sample.size An integer specifying the sample size (default: 100).
#' @param n.normal.targets An integer specifying the number of normal distributed target samples (default: 2500).
#' @param n.iter An integer specifying the number of iterations for model calibration (default: 2500).
#' @param lower.bound A numeric specifying the lower bound for source contribution (default: 0.001).
#' @param upper.bound A numeric specifying the upper bound for source contribution (default: 1).
#' @param constraints A numeric specifying the constraints for model calibration (default: 1).
#' @returns a data frame containing the source contributions for each target sample.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export %>% 
run.BMM <- function(data, class, mixture = "target", sample.id, tracers, uncertainty, save.dir, note, fileEncoding = "latin1",
                    multivar = TRUE, sd.threshold = .05, sample.size = 100, n.normal.targets = 2500, n.iter = 2500, lower.bound = 0.001, upper.bound = 1, constraints = 1){
  
  require(dplyr)
  require(tidyr)
  require(foreach)
  require(Rsolnp)
  
  # source dataset
  dt.source <- data %>%
    dplyr::filter(.data[[class]] != mixture) %>% # selection source sample
    dplyr::select(dplyr::all_of(c(sample.id, class, tracers)))
  
  # target dataset
  dt.target <- data %>%
    dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
    dplyr::select(dplyr::all_of(c(sample.id, tracers)))
  
  if(nrow(dt.target)==0){base::stop(paste0("The number of target samples is equal to zero. Please add target samples or correct the 'mixture' argument (currently set as: '", mixture,
                                           "').\n Type   help(run.BMM)   or  ?fingR::run.BMM   for the function documentation."))}
  
  # target's uncertainty
  if(!missing(uncertainty)){
    dt.target.uncer <- data %>%
      dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
      dplyr::select(dplyr::all_of(c(sample.id, uncertainty)))
  }
  
  # source levels labels
  sources.levels <- levels(as.factor(dt.source[[class]]))[levels(as.factor(dt.source[[class]])) != mixture] #c("S1","S2", "S3")
  
  
  ### generate sources normal distributed samples
  source.mnd <- fingR::source.norm.distrib(dt.source, class = class, tracers = tracers, n = n.normal.targets, multivar = multivar)
  
  all.contribs <- c()
  # for each target sample
  for(target in dt.target[[sample.id]]){
    one.target <- dt.target %>%
      dplyr::filter(.data[[sample.id]] == target) %>%
      dplyr::select(-all_of(sample.id))
    
    if(!missing(uncertainty)){ # if measurement uncertainty is set
      sigma <- dt.target.uncer %>%
        dplyr::filter(.data[[sample.id]] == target) %>%
        dplyr::select(-all_of(sample.id))
    }else{
      sigma <- one.target * sd.threshold # calculate a standard deviation for each tracer based based on the given threshold, defined as 0.05 %
    }
    
    # generate target normal distributed samples
    target.nd <- mapply(function(x, y){abs(rnorm(x, y, n = sample.size))}, x = one.target, y = sigma) %>% log # log transform the values
    
    # predict n normal distributed samples from tracers mean values and covariance.
    mnd.target <- MASS::mvrnorm(n = n.normal.targets, mu = colMeans(target.nd), Sigma = cov(target.nd)) %>% exp # transform the value to get the initial magnitude
    
    # model calibration
    n.sources <- length(sources.levels) # number of source levels
    lower.b <- rep(lower.bound, n.sources) # create as many lower.bound as there are sources
    upper.b <- rep(upper.bound, n.sources) # create as many upper.bound as there are sources
    
    
    # setting model functions
    eqn <- function(x){
      one.vec <- matrix(1, ncol = n.sources, nrow = 1)
      return(one.vec%*%x)
    }
    
    ineq <- function(x){ return(x) }
    
    random.fun <- function(){
      temp1 <- list()
      for(i in 1:length(source.mnd[[2]])){
        temp <- source.mnd[[2]][[i]]
        temp1[[i]] <- (temp[sample(nrow(temp), 1), ])
      }
      
      Xm <- as.matrix(data.frame(temp1)) # source mean value
      Xm.tracers <- Xm[tracers,]
      
      mnd.target <- mnd.target[sample(x = nrow(mnd.target), 1), ] # target property values
      mnd.target.tracers <- mnd.target[tracers]
      
      fn <- function(x) {
        # for linear tracers
        pred <- Xm.tracers %*% x # multiply tracers value per source by the contribution for each tracer
        #sqr_err <- sum(pred - mnd.target) # squared error
        sum( ((mnd.target.tracers - pred)/mnd.target.tracers)**2) # relative error
      }
      return(fn)
    }
    
    # Model Simulation (for more information check out ?solnp and ?foreach)
    system.time(
      sims.fine <- foreach(sims = 1:n.iter, .combine = rbind) %do% # for each iteration
        (solnp(pars = runif(n.sources), fun = random.fun(),
               eqfun = eqn, eqB = constraints,
               ineqfun = ineq, ineqLB = lower.b, ineqUB = upper.b,
               contro = list(tol = 10^-5)))$pars
    )
    # Summarize Results
    
    sims.fine <- as.data.frame(as.matrix(sims.fine))
    colnames(sims.fine) <- sources.levels # add source levels as col names
    
    all.contribs <- rbind(all.contribs,
                          sims.fine %>% # convert results to data.frame
                            tidyr::pivot_longer(cols = 1:ncol(sims.fine)) %>%
                            dplyr::mutate("mix.names" = target)) # add the current sample name
  }
  
  
  all.contribs <- all.contribs[,c(3,1,2)] # reorder columns
  colnames(all.contribs) <- c("mix.names", "source", "value")
  
  # save results
  if(!missing(save.dir)){
    file.name <- "BMM_prevision"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(x = all.contribs, file = paste0(save.dir, file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
  }
  
  return(all.contribs)
}
