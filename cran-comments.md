##  Version 1.1.0

In response to CRAN problems,  Vignette is now completely static,  willl not fail.
This is the reason for a second submission in a short time period

## Test environments
* local OS X install, R 4.0.5
* rhub check_for_cran Windows
* winbuilder (devel and release)

## R CMD check result

On Mac OS X  R 4.0.3 I get:

Duration: 1m 22.9s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
rxtracto    0.42   0.11    7.25
rxtracto_3D 0.13   0.00    6.42
rxtractogon 0.11   0.00    6.38

  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
