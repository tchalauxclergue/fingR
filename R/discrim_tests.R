#' Discriminant assessement
#'
#' Run discriminant tests for every property. Test can be Kruskal-Wallis (KW) or Kolmogorov-Smirnov (KS).
#'
#' This function is called within *fingR::discriminant.test*.
#'
#' @param data A data.frame containing at least class and property values.
#' @param class A character string corresponding to the column that contains the sources (source 1, source 2, etc) and mixture information if data contains mixture samples.
#' @param mixture A character string corresponding to way "mixture" are named in class. No name is defined by default. can not be set if date does not contain mixture samples.
#' @param test A character vector that indicates the test that will be performed: Kolmogorov-Smirnov ("KS") or Kruskal-Wallis ("KW").
#' @param properties A vector listing all the properties that will be evaluated.
#'
#' @return A data.frame with test results.
#'
#' @author Thomas Chalaux-Clergue & Rémi Bizeul
#'
#' @export
discrim.tests <- function(data, class, mixture, test, properties){

  require(dplyr)
  require(NSM3)
  #require(nonpar)

  # if a mixture name is specified it mean that it should be removed from the data
  if(!missing(mixture)){
    data <- dplyr::filter(data, .data[[class]] != mixture)
  }

  results.df <- data.frame() # create the results data.frame
  source.lvls <- levels(as.factor(data[[class]])) # set the list of source type names
  source.couples <- combinations(source.lvls) # create all combinations for all levels
  resu.r <- rep(list(c()), 2) # create results list: p-value and its significance

  for(property in properties){ # perform test for every property

    # KOLMOGOROV-SMIRNOV
    if(test == "KS"){
      for(couple in source.couples){
        ## create two subsets for each type of source
        source.A <- dplyr::filter(data, .data[[class]] == unlist(strsplit(couple, split="[-]"))[1]) #source type A
        source.B <- dplyr::filter(data, .data[[class]] == unlist(strsplit(couple, split="[-]"))[2]) #source type B

        if(nrow(source.A) <= 3 || nrow(source.B) <= 3){ #if there are less 3 samples in at least of source type don't run a test, because is not statistically significant.
          resu <- "Too few samples"
          signif <- NA
        }else{ # if there are enough samples
          # KOLMOGOROV-SMIRNOV TEST
          test.resu <- ks.test(source.A[[property]], source.B[[property]], alternative = "two.sided")
          resu.r[[1]] <- round(test.resu$p.value, 8)
          resu.r[[2]] <- fingR::pval.signif(test.resu$p.value)
        }
        results.df <- rbind(results.df, unlist(c(property, unlist(strsplit(couple, "[-]")), nrow(source.A), nrow(source.B), resu.r))) # add each source couple test result
      }
    }

    # KRUSKAL-WALLIS
    if(test == "KW"){
      test.resu <- kruskal.test(x = data[[property]], g = data[[class]]) #x are values and g class levels
      resu.r[[1]] <- round(test.resu$p.value, 5)
      resu.r[[2]] <- fingR::pval.signif(test.resu$p.value)

      results.df <- rbind(results.df, unlist(c(property, resu.r))) # add each property test result
    }
  }
  # correct the column names of the results data frame
  if(test == "KS"){
    colnames(results.df) <- c("Property", "Source_A", "Source_B", "n_source_A", "n_source_B", which.disc.tests(test))
    var.to.correct <- c("n_source_A", "n_source_B", which.disc.tests(test)[1])

  }else if(test == "KW"){
    colnames(results.df) <- c("Property", which.disc.tests(test))
    var.to.correct <- which.disc.tests(test)[1]
  }

  # correct some column class
  for(var in var.to.correct){
    results.df[[var]] <- as.numeric(results.df[[var]])
  }

  return(results.df)
}
