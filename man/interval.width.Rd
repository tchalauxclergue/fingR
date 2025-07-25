% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_width.R
\name{interval.width}
\alias{interval.width}
\title{Interval width}
\usage{
interval.width(
  prev,
  path.to.prev,
  sample_name,
  source.groups,
  value,
  mean.cal = FALSE,
  save = FALSE,
  save.dir,
  note,
  fileEncoding = "latin1"
)
}
\arguments{
\item{prev}{A `data.frame` containing predictions for each sample and source group. If missing, the function reads the data from `path.to.prev`.}

\item{path.to.prev}{A file path (character string) pointing to a `.csv` file with prediction draws (used only if `prev` is missing).}

\item{sample_name}{A character string indicating the name of the column containing sample identifiers. If missing, defaults to the first column.}

\item{source.groups}{A character string indicating the name of the column defining the source groups or classes. If missing, defaults to the second column.}

\item{value}{A character string indicating the column with prediction values. If missing, defaults to the third column.}

\item{mean.cal}{Logical; if `TRUE`, the function also computes the average interval widths (`W50.mean`, `W95.mean`) for each source group. Defaults to `FALSE`.}

\item{save}{Logical; if `TRUE`, the results are saved as `.csv` files. Defaults to `FALSE`.}

\item{save.dir}{A character string giving the directory where the output files will be saved. If missing and `save = TRUE`, the function infers the directory from `path.to.prev`.}

\item{note}{Optional character string to add a suffix to the output file names. Useful to distinguish output versions. Default is not set.}

\item{fileEncoding}{A character string defining the file encoding for output files (if `save = TRUE`). Default is `"latin1"`.}
}
\value{
If `mean.cal = FALSE`, returns a data frame with W50 and W95 values
for each sample and source group. If `mean.cal = TRUE`, returns a named list with
two elements:
\describe{
  \item{samples}{A data frame of interval widths per sample and group.}
  \item{mean}{A data frame of mean interval widths per source group.}
}
}
\description{
Computes the width of the central 50% (W50) and 95% (W95) prediction intervals
for each sample and source group, based on a table of predictive draws.
}
\author{
Thomas Chalaux-Clergue
}
