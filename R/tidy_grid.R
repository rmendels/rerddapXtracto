#' convert result of 'rxtracto_3D' or 'rxtractogon' to tidy long-format
#'
#' \code{tidy_grid} is a function to convert result of 'rxtracto_3D' or 'rxtractogon' to "tidy" long-format
#'
#' @export
#' @param response data frame returned from 'rxtracto_3D'()' or 'rxtractogon()'
#' @return a dataframe in long-format
#'
#' @examples
#' MBsst_tidy <-tidy_grid(MBsst)


tidy_grid <- function(response){
  if (!('rxtracto3D' %in% class(response))) {
    print('given extract is not valid rxtracto_3D() output')
    print("class of the responseonse is not' 'rxtracto3D' ")
    stop('execution halted')
  }
  no_dims <- length(response) - 2
  dims <- names(response)[3 : length(response)]

  lat_name = ifelse('lat' %in% dims, 'lat', 'latitude')
  lon_name = ifelse('lon' %in% dims, 'lon', 'longitude')
  out <- list()
  for (i in seq_along(dims)) {
    if (!(is.na(response[[i + 2]][1])))
    out[[dims[i]]] <- response[[i + 2]]
  }
  vars <- names(response)[1]
  outvars <- list()
  outvars[[ vars[1] ]] <- as.vector(response[[1]])
  df <- do.call("cbind.data.frame", outvars)
  rows <- length(outvars[[1]])

  out <- out[length(out):1]
  meta <- expand.grid(out, stringsAsFactors = FALSE)
  # make data.frame
  alldf <- if (NROW(meta) > 0) cbind(meta, df) else df

  # output
  return(alldf)
}

