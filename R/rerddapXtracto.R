#' rerddapXtracto: Routines to simplify data extraction using ERD's ERDDAP™ web service.
#'
#' The rerddapXtracto package subsets and extracts satellite and other oceanographic related data
#' from any ERDDAP™ server using the R package "rerddap"developed by Scott Chamberlain and the
#' wonderful people at "rOpenSci".
#'
#' The package contains three main functions:
#'
#' @section Main Functions:
#' \itemize{
#' \item \code{rxtracto} - Extracts an environmental variable along a track defined by
#'  its longtiude, latitude and time.
#' \item \code{rxtracto_3D} - Extracts an environmental variable in a
#' 3D  (longitude,latitude, time) bounding box
#' \item \code{rxtractogon} - Extracts an environmental variable in a spatial
#' polygon through time.
#'  }
#'  These functions require a call be made to  \code{rerddap:info()} for the appropriate ERDDAP™ server
#'  and datasetID.
#'#'
#'  @section Details:
#' Besides the terse help documents,  more  detail in using the
#' functions are given  in the included vignette "UsingrerddapXtracto". The datasets used
#' in the vignette are included in the "data" directory.
#'
#' @docType package
#' @name rerddapXtracto
#' @keywords internal
"_PACKAGE"
## usethis namespace: start
## usethis namespace: end
NULL
