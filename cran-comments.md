## Test environments
* local OS X install, R 3.6.0
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## update Version 0.4.0



Timings have been reduced as much as possible.
'rxtracto()', 'rxtracto_3D()', and 'rxtractogon()' all are functions that perform subsetting
and downloading of data from remote servers.  That is why the difference in user time 
and elapsed time.  It is data downloads.  The examples in this submission are cut down
as much as can be - they essentially download one point. Also, anything that is 
extraneous to the particular 'rerddapXtracto' call has been preloaded and is in
the "data" folder,  including the results of the necessary calls to 'rerddap::info()'.


## R CMD check result

On Mac OS X I get:

Duration: 51.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
            user system elapsed
rxtracto    0.40   0.00    6.75
rxtracto_3D 0.06   0.02    6.03
rxtractogon 0.06   0.01    6.07
** found \donttest examples: check also with --run-donttest

On r-hub Fedora:

Get Ok,  no notes,  no warnings

on winbuilder_release:

OK

on winbuilder_develop:

Ok
