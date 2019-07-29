data_extract_read <- function(dataInfo, callDims, urlbase,
                              xName, yName, zName, tName, parameter,
                              erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                              verbose, cache_remove) {

  griddapCmd <- makeCmd(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                        erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                        verbose)
  #print(griddapCmd)
  # Get the data ------------------------------------------------------------

  numtries <- 10
  tryn <- 0
  goodtry <- -1
  while ((tryn <= numtries) & (goodtry == -1)) {
    tryn <- tryn + 1
    griddapExtract <- try(do.call(rerddap::griddap, griddapCmd ), silent = TRUE)
    if (!class(griddapExtract)[1] == "try-error") {
      goodtry <- 1
    } else{
      rerddap::cache_delete_all()
      # rerddap::cache_list()
      Sys.sleep(tryn * 0.5)
    }
  }
#  if (class(griddapExtract)[1] == "try-error") {
  if (goodtry == -1) {
    print('error in trying to download the subset')
    print('check your settings')
    print(griddapCmd)
    print('stopping execution  - will return what has been downloaded so far')
    #stop('check that the dataset is active in the given ERDDAP server')
    temp_extract <- -1
    return(temp_extract)
  }


  # read in the downloaded netcdf file --------------------------------------


  datafileID <- try(ncdf4::nc_open(griddapExtract$summary$filename), silent = TRUE)
  if (class(datafileID) == "try-error") {
    print('error in trying to open netcdf file')
    print('check check above for any errors')
    print('stopping execution  - will return what has been downloaded so far')
    temp_extract <- -1
    return(temp_extract)

  }

  dataX <- ncdf4::ncvar_get(datafileID, varid = xName)
  dataY <- ncdf4::ncvar_get(datafileID, varid = yName)
  if (!is.null(callDims[[3]])) {
    dataZ <- ncdf4::ncvar_get(datafileID, varid = zName)
  }

  if (!is.null(callDims[[4]])) {
    datatime <- ncdf4::ncvar_get(datafileID, varid = "time")
    datatime <- as.POSIXlt(datatime, origin = '1970-01-01', tz = "GMT")
  }

  param <- ncdf4::ncvar_get(datafileID, varid = parameter, collapse_degen = FALSE)

  ncdf4::nc_close(datafileID)


  # Readjust lat-lon coordinates --------------------------------------------

  tempCoords <- readjustCoords(param, dataX, dataY, callDims[[1]], datafileID, callDims)
  dataX <- tempCoords$dataX
  dataY <- tempCoords$dataY

  # remove netcdf file from cache
  if (cache_remove) {
    rerddap::cache_delete(griddapExtract)
  }
  # create output list ------------------------------------------------------


  temp_extract <- list(NA, NA, NA, NA, NA, NA)
  # temp_extract <-  vector("list", 6)
  temp_extract[[1]] <- tempCoords$param
  temp_extract[[2]] <- attributes(dataInfo)$datasetid
  temp_extract[[3]] <- dataX
  temp_extract[[4]] <- dataY
  if (!is.null(callDims[[3]])) {
    temp_extract[[5]] <-  dataZ
  }
  if (!is.null(callDims[[4]])) {
    temp_extract[[6]] <-  datatime
  }
  if (grepl('etopo',temp_extract[[2]])) {
    names(temp_extract) <- c('depth', "datasetname", names(callDims)[1], names(callDims)[2], names(callDims)[3], "time")

  }else{
    names(temp_extract) <- c(parameter, "datasetname", names(callDims)[1], names(callDims)[2], names(callDims)[3], "time")

  }

  return(temp_extract)
}
