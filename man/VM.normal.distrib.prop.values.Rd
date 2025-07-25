% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VM_normal_distrib_prop_values.R
\name{VM.normal.distrib.prop.values}
\alias{VM.normal.distrib.prop.values}
\title{Virtual mixtures from normal distributed property values}
\usage{
VM.normal.distrib.prop.values(
  data,
  class,
  tracers,
  n,
  multivar,
  contributions,
  VM.name
)
}
\arguments{
\item{data}{A `data.frame` containing source samples with tracer measurements and associated source group labels.}

\item{class}{A character string specifying the name of the column in `data` that defines source groups or classes.}

\item{tracers}{A character vector specifying the names of the tracer columns to be used for simulation (e.g., geochemical properties, isotopes).}

\item{n}{An integer defining the number of normally distributed samples to simulate per source group.}

\item{multivar}{Logical; if `TRUE`, the simulation accounts for the multivariate structure (covariances) of the tracers. If `FALSE`, tracers are simulated independently.}

\item{contributions}{A `data.frame` describing the contribution of each source group to each virtual mixture. One row per mixture, columns include a mixture ID and proportion values for each source group.}

\item{VM.name}{A character string giving the name of the column in `contributions` that contains the unique virtual mixture names (e.g., "Mix1", "Mix2").}
}
\value{
A named list of two `data.frame` objects:
\describe{
  \item{values}{Simulated tracer values for each virtual mixture (`Sample_name`).}
  \item{sd}{Associated standard deviations of simulated tracer values.}
}
}
\description{
Generates virtual mixture property values using normally distributed source samples, based on tracer means and standard deviations per source group. This simulation is typically used for testing and validating sediment source apportionment models.
}
\references{
Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate sediment fingerprinting source apportionments. Journal of Soils and Sediments, 22(4), 1315-1328.
}
\author{
Thomas Chalaux-Clergue
}
