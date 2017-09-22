# rerddapXtracto (Version 0.3.0)
rerddapXtracto - R package for accessing environmental data using rerddap  (**For Testing Purposes Only**)

- ** Version.  Still for testing purposes only **
- ** For real work - use at own risk **
- ** Suggest doing small tests on any extract **
- ** For rxtracto(), do say the first 3 points with "verbose = TRUE" **
- ** Make certain the URL calls make sense **
- ** In final output,  make certain data limits **
- ** and limits requested from dataset make sense **

`rerddapXtracto` is an <span style="color:blue">R</span> package developed to subset and extract satellite and other oceanographic related data from a remote <span style="color:blue">ERDDAP</span> server. The program can extract data for a moving point in time along a user-supplied set of longitude, latitude, time and depth  (new in this version) points; in a 3D bounding box; or within a polygon (through time). 

New in this version is that a track can now move in (x, y, z, t) space if appropriate for the dataset being accessed.  And two plotting functions have been added,  `plotTrack()` and `plotBox()` that make use of the `plotdap` package.  See the new [rerdapXtracto vignette](https://rmendels.github.io/UsingrerddapXtracto.nb.html).  A lot of the code has been reworked, in particular the handling of time,  and in the formation of the requests to `rerddap`.



There are three main data extraction functions in the `rerddapXtracto` package: 

- `rxtracto <- function(dataInfo, parameter = NULL, xcoord = NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xlen = 0., ylen = 0., zlen = 0., xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)`

- `rxtracto_3D <- function(dataInfo, parameter = NULL, xcoord = NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)`

- `rxtractogon <- function(dataInfo, parameter, xcoord = NULL, ycoord = NULL, zcoord = NULL, tcoord = NULL, xName = 'longitude', yName = 'latitude', zName = 'altitude', tName = 'time', urlbase = 'http://upwell.pfeg.noaa.gov/erddap', verbose = FALSE)`

and two functions for producing maps:

- `plotTrack <- function(resp, xcoord, ycoord, plotColor = 'viridis', name = NA, myFunc = NA, shape = 20, size = .5)`

- `plotBBox <- function(resp, plotColor = 'viridis', time = NA, animate = FALSE, name = NA, myFunc = NA, maxpixels = 10000)`



`rerddapXtracto` uses the `rerddap`, `ncdf4` , `parsedata`, `plotdap` and `sp` packages , and these packages (and the packages imported by these packages) must be installed first or `rerddapXtracto` will fail to install.   

```{r install,eval=FALSE}
install.packages("ncdf4") 
install.packages("parsedata") 
install.packages("sp")
```

The present version of `rerddapXtracto` is coded against a version of `rerddap` that is not yet on CRAN.  To obtain that version of `rerddap`:

```{r plotdap, eval = FALSE}
install.packages("devtools")
devtools::install_github('ropensci/rerddap')
```


To install the plotdap package from <span style="color:blue">Github</span>:

```{r plotdap, eval = FALSE}
install.packages("devtools")
devtools::install_github('ropensci/plotdap')
```

The `rerddapXtracto` package at the moment can be installed from Github using the devtools package:

```{r install,eval=FALSE}
install.packages("devtools")
devtools::install_github("rmendels/rerddapXtracto")
```



# Required legalese

“The United States Department of Commerce (DOC) GitHub project code is provided
on an ‘as is’ basis and the user assumes responsibility for its use.
DOC has relinquished control of the information and no longer has responsibility
to protect the integrity, confidentiality, or availability of the information.
Any claims against the Department of Commerce stemming from the use of its
GitHub project will be governed by all applicable Federal law. Any reference to
specific commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their endorsement,
recommendation or favoring by the Department of Commerce. The Department of
Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used
in any manner to imply endorsement of any commercial product or activity by DOC
or the United States Government.”


