## update Version 0.4.8

fixed problem in 'plotBBox' when name is given for colorbar

## Test environments
* local OS X install, R 4.0.1
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## R CMD check result

On Mac OS X I get:

Duration: 2m 27.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

On rhub Windows Server I get:

   Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
   rxtracto    0.21   0.11    6.61
   rxtractogon 0.08   0.00    6.09
   rxtracto_3D 0.05   0.00    6.01
   ** found \donttest examples: check also with --run-donttest

On rhub Fedora I get:

Ok
  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
