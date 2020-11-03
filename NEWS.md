# rerddapXtracto 1.0.2

Vignette is now completely static

# rerddapXtracto 1.0.1

 Fixed bug in 'rxracto()' request date element of the return structure.

# rerddapXtracto 1.0.0

- Major changes in how 'rxtracto()' works.
- Now one download for all requests at the same period
- If no time,  such as bathymetry,  just one download
- Calculations are then done on the downloaded data
- Should greatly speed-up very large requests

# rerddapXtracto 0.4.8

fixed problem in 'plotBBox' when name is given for colorbar

# rerddapXtracto 0.4.7

- added option for progress bar in 'rxtracto()'

- improved vignette to more accurately reflect the use of 'cmocean' colors
  in 'plotdap'.

# rerddapXtracto 0.4.6

Changed vignette to work with latest version of 'plotdap' that now uses the colors from the 'cmocean' package

# rerddapXtracto 0.4.5

Added maptools and rgdal to Suggests to be consistent with of plotdap v0.0.4

# rerddapXtracto 0.4.4

- fixed etopo error reported by user

# rerddapXtracto 0.4.3

- fixed vignette to reflect new datasetID at IFREMER ERDDAP

# rerddapXtracto 0.4.2

- fixed vignette to reflect new datasetID at IFREMER ERDDAP
- removed 'plotdap' message when MBMS map is built

# rerddapXtracto 0.4.1

- More robust handling of http timeouts particularly in 'rxtracto()'
- Better handling of zlen if xlen and ylen vary 
- Fixed problem with data indirectly served through TDS that cross dateline
- Removed UTF-8 Symbol

# rerddapXtracto 0.4.0

- Makes extracts that cross dateline
- plot routines updated
- refactoring of code

# rerddapXtracto 0.3.5

Initial CRAN release

