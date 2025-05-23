% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_grid.R
\name{tidy_grid}
\alias{tidy_grid}
\title{convert result of 'rxtracto_3D' or 'rxtractogon' to tidy long-format}
\usage{
tidy_grid(response, as_tibble = TRUE)
}
\arguments{
\item{response}{data frame returned from 'rxtracto_3D'()' or 'rxtractogon()'}

\item{as_tibble}{whether to return as tibble or datafrome}
}
\value{
a dataframe in long-format
}
\description{
\code{tidy_grid} is a function to convert result of 'rxtracto_3D' or 'rxtractogon' to "tidy" long-format
}
\examples{
MBsst_tidy <-tidy_grid(MBsst)
}
