run.BMM <- function(data, data.target, class, mixture = "Target", sample.id, target.id, tracers, uncertainty, save.dir, note, fileEncoding = "latin1",
                    multivar = TRUE, sd.threshold = .05, sample.size = 100, n.normal.targets = 2500, n.iter = 2500, lower.bound = 0.001, upper.bound = 1, constraints = 1){

  require(dplyr)
  require(tidyr)
  require(foreach)
  require(Rsolnp)

  # source dataset
  dt.source <- data %>%
    dplyr::filter(.data[[class]] != mixture) %>% # selection source sample
    dplyr::select(dplyr::all_of(c(sample.id, class, tracers)))

  #dt.source[[class]] <- as.factor(as.character(dt.source[[class]])) # est-ce utile ?

  # target dataset
  if(!missing(data.target)){ # if an additional target dataset is set
    if(!missing(target.id)){
      dt.target <- data.target %>% # selection source sample
        dplyr::select(dplyr::all_of(c(target.id, tracers))) # if target id are different
      sample.id <- target.id
    }else{
      dt.target <- data.target %>% # selection source sample
        dplyr::select(dplyr::all_of(c(sample.id, tracers)))
    }
  }else{
    dt.target <- data %>%
      dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
      dplyr::select(dplyr::all_of(c(sample.id, tracers)))
  }

  # target's uncertainty
  if(!missing(uncertainty)){
    dt.target.uncer <- data %>%
      dplyr::filter(.data[[class]] == mixture) %>% # selection target sample
      dplyr::select(dplyr::all_of(c(sample.id, uncertainty)))
  }

  # source levels labels
  sources.levels <- levels(as.factor(dt.source[[class]]))
  sources.levels <- sources.levels[sources.levels != mixture]

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
    target.nd <- mapply(function(x, y){rnorm(x, y, n = sample.size)}, x = one.target, y = sigma) %>% log() # log transform the values

    # predict n normal distributed samples from tracers mean values and covariance.
    mnd.target <- MASS::mvrnorm(n = n.normal.targets, mu = colMeans(target.nd), Sigma = cov(target.nd)) %>% exp() # transform the value to get the initial magnitude

    # model calibration
    n.sources <- length(sources.levels) # number of source levels
    lower.b <- rep(lower.bound, n.sources) # create a many 0.001 as there are sources
    upper.b <- rep(upper.bound, n.sources) # same


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

      Xm <- as.matrix(data.frame(temp1))

      mnd.target <- mnd.target[sample(x = nrow(mnd.target), 1), ]

      fn <- function(x) {
        pred <- Xm %*% x
        sqr_err <- sum(pred - mnd.target)
        sum(((mnd.target - pred)/mnd.target)**2)
      }
      return(fn)
    }

    # Model Simulation (for more information check out ?solnp and ?foreach)
    system.time(
      sims.fine <- foreach(sims = 1:n.iter,
                           .combine = rbind) %do% # for each iteration
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
