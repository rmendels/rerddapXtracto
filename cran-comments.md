## Test environments
* local OS X install, R 3.6.0
* rhub check_for_cran Windows Server
* win-builder (devel and release)

## Resubmission

Problem with keywords fixed.

Timings have been reduced as much as possible.
'rxtracto()', 'rxtracto_3D()', and 'rxtractogon()' all are functions that perform subsetting
and downloading of data from remote servers.  That is why the difference in user time 
and elapsed time.  It is data downloads.  The examples in this submission are cut down
as much as can be - they essentially download one point. Also, anything that is 
extraneous to the particular 'rerddapXtracto' call has been preloaded and is in
the "data" folder,  including the results of the necessary calls to 'rerddap::info()'.


## R CMD check result

On Mac OS X I get:

Duration: 2m 43.1s
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

On rhub Windows Server I get:

   Examples with CPU or elapsed time > 5s
               user system elapsed
   rxtractogon 0.08   0.00    6.86
   rxtracto    0.21   0.04    7.10
   rxtracto_3D 0.04   0.02    6.40


On winbuilder-release I get:

1 note new submission

On winbuilder_devel I get:

1 note new submission


* This is a new release.
