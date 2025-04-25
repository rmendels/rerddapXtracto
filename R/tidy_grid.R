#' convert result of 'rxtracto_3D' or 'rxtractogon' to tidy long-format
#'
#' \code{tidy_grid} is a function to convert result of 'rxtracto_3D' or 'rxtractogon' to "tidy" long-format
#'
#' @export
#' @param response data frame returned from 'rxtracto_3D'()' or 'rxtractogon()'
#' @param as_tibble whether to return as tibble or datafrome
#' @return a dataframe in long-format
#'
#' @examples
#' MBsst_tidy <-tidy_grid(MBsst)


tidy_grid <- function(response, as_tibble = TRUE) {
  if (!inherits(response, "rxtracto3D")) {
    stop("`response` must be an object of class 'rxtracto3D'")
  }

  # 1) Name of data‐array
  var_name   <- names(response)[1]

  # 2) Figure out which slots are real coordinates (not all-NA)
  coord_names <- setdiff(names(response), c(var_name, "datasetname"))
  coord_names <- coord_names[
    vapply(response[coord_names], function(x) !all(is.na(x)), logical(1))
  ]

  # 3) Pull those vectors out
  coords     <- response[coord_names]

  # 4) Cartesian product of coords
  grid_df    <- expand.grid(
    coords,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # 5) Flatten the array values
  values     <- as.vector(response[[var_name]])

  # 6) Bind them back together
  out_df     <- cbind(
    grid_df,
    stats::setNames(list(values), var_name),
    stringsAsFactors = FALSE
  )

  # 7) Keep datasetname as an attribute (not repeated in every row)
  attr(out_df, "datasetname") <- response$datasetname

  # 8) Optionally convert to tibble
  if (as_tibble) {
    if (!requireNamespace("tibble", quietly = TRUE)) {
      stop("Please install the 'tibble' package to use `as_tibble = TRUE`")
    }
    out_df <- tibble::as_tibble(out_df)
    attr(out_df, "datasetname") <- response$datasetname
  }

  out_df
}
