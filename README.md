# rerddapXtracto
rerddapXtracto - R package for accessing environmental data using rerddap  (** For Testing Purposes Only **)

`rerddapXtracto` is an <span style="color:blue">R</span> package developed to subset and extract satellite and other oceanographic related data from a remote <span style="color:blue">ERDDAP</span> server. The program can extract data for a moving point in time along a user-supplied set of longitude, latitude and time points; in a 3D bounding box; or within a polygon (through time).  


There are three main data extraction functions in the `xtractomatic` package: 

- `rxtracto <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord=NULL, zcoord = NULL, tcoord=NULL, xlen=0., ylen=0., xName='longitude', yName='latitude', zName='altitude', tName='time', urlbase='http://upwell.pfeg.noaa.gov/erddap')`

- `rxtracto_3D <- function(dataInfo, parameter = NULL, xcoord=NULL, ycoord=NULL, zcoord = NULL, tcoord=NULL, xName='longitude', yName='latitude', zName='altitude', tName='time', urlbase='http://upwell.pfeg.noaa.gov/erddap')`

- `rxtractogon <- function(dataInfo, parameter, xcoord=NULL, ycoord=NULL, zcoord = NULL, tcoord=NULL, xName='longitude', yName='latitude', zName='altitude', tName='time', urlbase='http://upwell.pfeg.noaa.gov/erddap')`



`rerddapXtracto` uses the `rerddap`, `ncdf4` and `sp` packages , and these packages (and the packages imported by these packages) must be installed first or `rerddapXtracto` will fail to install.   

```{r install,eval=FALSE}
install.packages("rerddap", dependencies = TRUE)
install.packages("ncdf4") 
install.packages("sp")
```

The `xtractomatic` package at the moment can be installed from Github using the devtools package:

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


