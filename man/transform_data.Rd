% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_data.R
\name{transform_data}
\alias{transform_data}
\title{Transform tabular data in Stan-friendly list data}
\usage{
transform_data(sim_data, dimVals = seq(-0.5, +0.5, 0.05), groupNames = NULL)
}
\arguments{
\item{sim_data}{output from \code{simulate_data}}

\item{dimVals}{dimension values, vector length of 5}

\item{groupNames}{optional vector of group names}
}
\description{
Transform tabular data in Stan-friendly list data
}
