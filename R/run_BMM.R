#' run.BMM
#'
#' Runs the Bayesian Mixing Model (BMM) to estimate source contributions for given target samples. Source contribution are estimated by minimising the sum of squared residuals (SSR) of the mass balance un-mixing model.
#' Manage isotopic ratio for tracers specified within \code{isotope.prop}, \code{isotope.ratio} (optional \code{isotopes.unc})
#'
#' @param data Data frame. The dataset containing both source and target samples.
#' @param class Character. The column name that specifies the class (source/target).
#' @param mixture Character. The class label for the target samples (default is \code{"target"}).
#' @param sample.id Character. The column name for sample identifiers.
#' @param tracers Character vector. The column names of the tracers used in the model.
#' @param uncertainty Character vector. Optional column names for uncertainty values of the tracers.
#' @param isotope.ratio Character vector. Optional column names for the property isotopes ratio (e.g. d13C).
#' @param isotope.prop Character vector. Optional column names for the concentration properties of the respective isotopes (e.g. TOC).
#' @param isotopes.unc Character vector. Optional column names for isotopes uncertainties (e.g. d13C_uncertainty).
#' @param save.dir Character. Optional directory path for saving the results.
#' @param note Character. Optional additional note to append to the file name.
#' @param fileEncoding Character. Encoding to be used for the saved file (default is \code{"latin1"}).
#' @param multivar Logical. Whether to use multivariate normal distribution for sources (default is \code{TRUE}).
#' @param sd.threshold Numeric. Standard deviation threshold for generating target samples (default is 0.05).
#' @param sample.size Integer. The size of the sample to generate for the target (default is 100).
#' @param n.normal.targets Integer. The number of normally distributed samples for sources (default is 2500).
#' @param n.iter Integer. The number of iterations for the model simulation (default is 2500).
#' @param lower.bound Numeric. The lower bound for the source contributions (default is 0.001).
#' @param upper.bound Numeric. The upper bound for the source contributions (default is 1).
#' @param constraints Numeric. The constraints for the equality function in the model (default is 1).
#' 
#' @return A data frame with the estimated source contributions for each target sample.
#' 
#' @import dplyr
#' @import tidyr
#' @import foreach
#' @import doParallel
#' @import Rsolnp
#' @import progress
#' 
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export 
run.BMM <- function(data, class, mixture = "target", sample.id, tracers, uncertainty, isotope.ratio, isotope.prop, isotopes.unc, save.dir, note, fileEncoding = "latin1",
                    multivar = TRUE, sd.threshold = .05, sample.size = 100, n.normal.targets = 2500, n.iter = 2500, lower.bound = 0.001, upper.bound = 1, constraints = 1){
  
  require(dplyr)
  require(tidyr)
  require(foreach)
  require(doParallel)
  require(Rsolnp)
  require(progress)
  
  # Source dataset
  dt.source <- data %>%
    dplyr::filter(.data[[class]] != mixture) %>% # selection source sample
    dplyr::select(dplyr::all_of(c(sample.id, class, tracers)))
  
  # Target dataset
  dt.target <- data %>%
    dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
    dplyr::select(dplyr::all_of(c(sample.id, tracers)))
  
  if(!missing(isotope.ratio)){
    dt.source[, isotope.ratio] <- abs(dt.source[, isotope.ratio]) # keep the absolute value of organic matter isotopes
    dt.target[, isotope.ratio] <- abs(dt.target[, isotope.ratio]) # keep the absolute value of organic matter isotopes
  }
  
  if(nrow(dt.target)==0){base::stop(paste0("The number of target samples is equal to zero. Please add target samples or correct the 'mixture' argument (currently set as: '", mixture,
                                           "').\n Type   help(run.BMM)   or  ?fingR::run.BMM   for the function documentation."))}
  
  
  # Target's uncertainty
  if(!missing(uncertainty)){
    dt.target.uncer <- data %>%
      dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
      dplyr::select(dplyr::all_of(c(sample.id, uncertainty)))
  }
  
  # Source levels labels
  sources.levels <- levels(as.factor(dt.source[[class]]))[levels(as.factor(dt.source[[class]])) != mixture] #c("S1","S2", "S3")
  
  
  ### Generate sources normal distributed samples
  source.mnd <- fingR::source.norm.distrib(dt.source, class = class, tracers = tracers, n = n.normal.targets, multivar = multivar)

  
  ## If isotopes are defined
  if(!missing(isotope.ratio)){
    tracers <- setdiff(tracers, isotope.ratio) # remove isotopes from the list of tracers
    iso <- TRUE
  }else{
    iso <- FALSE
  }
  
  if(!missing(uncertainty)){
    uncer <- TRUE
  }else{
    uncer <- FALSE
  }

  time.s <- Sys.time()
  all.contribs <- c()
  # For each target sample
  for(target in dt.target[[sample.id]]){
    time.a <- Sys.time()
    t <- as.numeric(difftime(time.a, time.s, units="secs"))
    
    pbar <- progress::progress_bar$new(format = paste0("       Unmixing '", target,
                                                                "' (", which(dt.target[[sample.id]] == target), "/", length(dt.target[[sample.id]]), " - ",
                                                                round(which(dt.target[[sample.id]] == target)/length(dt.target[[sample.id]])*100), "% - ",
                                                                sprintf("%02d:%02d:%02d", floor(t / 3600), floor((t %% 3600)/60), round(t %% 60)),
                                                                ")  -  [:bar] :current/:total iterations (:percent - :elapsedfull)"), # progress bar message and structure
                                                clear = TRUE,  # Keep the progress bar once completed
                                                total = n.iter, # The number of iterations
                                                width = 130)     # The length of the progress bar
    pbar$tick(0) # Initialise the progress bar
    
    
    one.target <- dt.target %>%
      dplyr::filter(.data[[sample.id]] == target) %>%
      dplyr::select(-all_of(sample.id))
    
    if(isTRUE(uncer)){ # If measurement uncertainty is set
      sigma <- dt.target.uncer %>%
        dplyr::filter(.data[[sample.id]] == target) %>%
        dplyr::select(-all_of(sample.id))
    }else{
      sigma <- one.target * sd.threshold # Calculate a standard deviation for each tracer based based on the given threshold, defined as 0.05 %
    }
    
    # Generate target normal distributed samples
    target.nd <- mapply(function(x, y){abs(rnorm(x, y, n = sample.size))}, x = one.target, y = sigma) %>% log # log transform the values
    
    # Predict n normal distributed samples from tracers mean values and covariance.
    mnd.target <- MASS::mvrnorm(n = n.normal.targets, mu = colMeans(target.nd), Sigma = cov(target.nd)) %>% exp # transform the value to get the initial magnitude
    
    # Model calibration
    n.sources <- length(sources.levels) # Number of source levels
    lower.b <- rep(lower.bound, n.sources) # Create as many lower.bound as there are sources
    upper.b <- rep(upper.bound, n.sources) # Create as many upper.bound as there are sources
    
    
    # Setting model functions
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
      
      Xm <- as.matrix(data.frame(temp1)) # Source mean value
      Xm.tracers <- Xm[tracers,]
      
      mnd.target <- mnd.target[sample(x = nrow(mnd.target), 1), ] # Target property values
      mnd.target.tracers <- mnd.target[tracers]
      
      if(isTRUE(iso)){
        Xm.iso.cons <- Xm[isotope.prop,]
        Xm.iso.ratio <- Xm[isotope.ratio,]
        mnd.target.iso <- mnd.target[isotope.ratio]
      }
      
      fn <- function(x) {
        # For linear tracers
        pred <- Xm.tracers %*% x # multiply tracers value per source by the contribution for each tracer
        #sqr_err <- sum(pred - mnd.target) # squared error
        residuals.lin <- (mnd.target.tracers - pred)/mnd.target.tracers # residuals
        
        # For non linear tracers
        if(isTRUE(iso)){ # if organic matter isotopes were defined
          pred.OM <- Xm.iso.cons %*% x
          pred.iso <- (Xm.iso.ratio * Xm.iso.cons) %*% x
          # sqr_err <- sum(pred.iso - mnd.isotarget)
          residuals.iso <- (mnd.target.iso - (pred.iso/pred.OM)) / mnd.target.iso # relative error for isotopes
          
          sum( rbind(residuals.lin, residuals.iso)**2 ) # sum of squared relative error
        }else{
          sum( residuals.lin**2 )# sum of squared relative error
        }
      }
      return(fn)
    }
    
    # Model Simulation (for more information check out ?solnp and ?foreach)
    #system.time(
    invisible(capture.output( 
      sims.fine <- foreach::foreach(sims = 1:n.iter, .combine = rbind) %dopar% { # for each iteration
        
        pbar$tick(1) # Update progress bar
        
        # run solnp monte carlo chain
        (solnp(pars = runif(n.sources), # predicted contributions
               fun = random.fun(),
               eqfun = eqn,
               eqB = constraints, # constraints, i.e., the total of contributions
               ineqfun = ineq,
               ineqLB = lower.b, # Lower bound of the inequality constraints
               ineqUB = upper.b, # Upper bound of the inequality constraints
               contro = list(tol = 10^-5)) # number of digits
        )$pars # Return optional parameters, here optimised contributions
      }
    ))
    #)
    
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
  
  # Resume running information
  t <- as.numeric(difftime(Sys.time(), time.s, units="secs"))
  base::message(paste0("        Unmixing completed  -  Number of unmixed mixtures: ", length(dt.target[[sample.id]]), "  -  Running time: ", sprintf("%02d:%02d:%02d", floor(t / 3600), floor((t %% 3600)/60), round(t %% 60))))
  
  
  # Save data if 'save.dir' is provided
  if(!missing(save.dir)){
    file.name <- "BMM_prevision"
    if(!missing(note)){
      file.name <- paste(file.name, note, sep="_")
    }
    utils::write.csv(x = all.contribs, file = paste0(save.dir, file.name, ".csv"), row.names = FALSE, fileEncoding = fileEncoding)
  }
  
  return(all.contribs)
}
