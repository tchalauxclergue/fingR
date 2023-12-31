% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RT_mean_sd.R
\name{RT.mean.sd}
\alias{RT.mean.sd}
\title{Mean plus/minus one standard deviation range test}
\usage{
RT.mean.sd(target, sources, class, alternative = "single", shift = 0.1)
}
\arguments{
\item{target}{a (non-empty) numeric vector of target values.}

\item{sources}{a (non-empty) numeric matrix/data.frame of sources values and classes.}

\item{class}{name of the source classes columns in sources.}

\item{alternative}{a character that indicates if range test should be apply on each sample (sample) or should calculates the criterion on all samples (population).}

\item{shift}{a numeric value of the size of shift to correct the minimum value to do log transformation, .1 (default)}
}
\description{
Assess the conservativity of a property based on the comparison of the sources and target means.
All data is transform to the log as comparing means underlay that distribution is assessed to be Normal.
The property is conservative if the mean of log of target value is between the highest and lowest means of log of sources values.
}
\author{
Thomas Chalaux-Clergue & Rémi Bizeul
}
