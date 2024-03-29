% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_aggregation.R
\name{run_aggregation}
\alias{run_aggregation}
\title{Aggregate block-level population data from the decennial Census to a chosen grid definition}
\usage{
run_aggregation(
  mode = "shapefile",
  grid_path = NULL,
  grid_name = NULL,
  year = 2020,
  variables = NULL,
  area_weight = TRUE,
  states = NULL,
  output_path = getwd(),
  output_name,
  overwrite = FALSE,
  shiny = FALSE
)
}
\arguments{
\item{mode}{A character value indicating the type of grid to use for aggregation, one of \code{c("shapefile", "county", "tract")}. Defaults to using user-specified shapefile (\code{mode = "shapefile"}). \code{"county"} and \code{"tract"} options instead use U.S. Census-defined county and tract shapes, respectively.}

\item{grid_path}{A character value representing the file path to the shapefile to use. Only used when \code{mode = "shapefile"}.}

\item{grid_name}{A character value representing the name of the shapefile to use. Do not include file extension. Only used when \code{mode = "shapefile"}.}

\item{year}{A numeric value indicating the year of the desired Census data. Defaults to \code{2020}, currently the only supported decennial Census year.}

\item{variables}{A character vector indicating the Census variables to include. Use \code{\link[tidycensus:load_variables]{tidycensus::load_variables()}} to get information on Census variables. Defaults to BenMAP input variables binned by age, sex, race, and ethnicity.}

\item{area_weight}{A \code{TRUE/FALSE} value indicating whether to use an area weighting approach (block population data are allocated to grid cells based on area proportion, \code{area_weight = TRUE}) or a centroid approach (block population data are allocated to grid cells based on block centroids, \code{area_weight = FALSE}) to allocate block data to the chosen grid definition. Defaults to the area weighting approach (\code{area_weight = TRUE}). Only used when \code{mode = "county"} or \code{"tract"}.}

\item{states}{A character value or character vector of state postal abbreviations indicating which states to include. If no input is provided, all CONUS states are included.}

\item{output_path}{A character value representing the file path to the output folder. Defaults to the current working directory (\code{output_path = getwd()})}

\item{output_name}{A character value representing the name to use for output files.}

\item{overwrite}{A \code{TRUE/FALSE} value indicating whether to overwrite existing output files. Defaults to \code{FALSE}.}
}
\value{
\code{\link[=run_aggregation]{run_aggregation()}} does not return a value, but instead saves three output files: a shapefile of the selected grid with population data, a CSV with the number of people in each grid cell for each of the race-ethnicity-gender-age demographic groups included in BenMAP, and a CSV with the fraction of the total population in each of the eight race-ethnic groups that comes from each U.S. county.
}
\description{
This function allows users to aggregate block-level population data from the decennial U.S. Census to a chosen grid definition. Users may provide a grid in the form of a shapefile or use the Census's county or tract grids. Other inputs allow the user control over allocation method, Census year and variables to use, and included U.S. states.
}
\examples{
run_aggregation(mode = "county",
 year = 2020,
 states = c("MA", "RI", "CT", "ME", "NH", "VT"),
 output_name = "New England")
}
