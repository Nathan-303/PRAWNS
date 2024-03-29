% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facet_medmean_london_src.R
\name{facet_medmean_london_src}
\alias{facet_medmean_london_src}
\title{Plot the mean and median emissions with and without London next to each other}
\usage{
facet_medmean_london_src(pollutant, year, prawn_path)
}
\arguments{
\item{pollutant}{the pollutant being investigated, this should be in quotes
and correspond with its name in the data}

\item{year}{the year the data was taken, used for axis titles}

\item{prawn_path}{The filepath for the prawn CSV that is to be used.}
}
\description{
Plot the mean and median emissions with and without London next to each other
}
\examples{
side_by_sidestats(
  prawn_path="PRAWN.csv",
  pollutant="NOx",
  year=2019
  )
}
\keyword{faceted,}
\keyword{sources}
