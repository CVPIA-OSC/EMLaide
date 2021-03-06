% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxonomic-coverage-element.R
\name{add_taxonomic_coverage}
\alias{add_taxonomic_coverage}
\title{Add Taxonomic Coverage}
\usage{
add_taxonomic_coverage(taxonomic_metadata)
}
\arguments{
\item{taxonomic_metadata}{see \code{\link{create_taxonomic_coverage}}}
}
\value{
A complete list describing taxonomic coverage that can be added to the coverage list using \code{add_coverage()}
}
\description{
Formats the taxonomic coverage elements in a nested list to easily be added as a parameter to the \code{add_coverage()} function.
}
\examples{
\dontrun{
taxonomic_metadata <- list(CVPIA_common_species = c("chinook", "steelhead"), common_name = c(NA, NA), 
                           kingdom = c(NA, NA), phylum = c(NA, NA), class = c(NA, NA), 
                           order = c(NA, NA), family = c(NA, NA), genus = c(NA, NA), species = c(NA, NA), 
                           taxon_id = c(NA, NA))}
\dontrun{                           
add_taxonomic_coverage(taxonomic_metadata)
}
}
