## update Version 0.4.1

Should fix the notes and warnings in CRAN nightly builds
Some other minor improvements detailed in the NEWS

## Test environments
* local OS X install, R 3.6.1
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## R CMD check result

On Mac OS X I get:

Duration: 51.1s

0 errors v | 0 warnings v | 0 notes v

On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
            user system elapsed
rxtracto    0.51   0.09    6.72
rxtractogon 0.17   0.03    5.96
rxtracto_3D 0.08   0.05    5.93
** found \donttest examples: check also with --run-donttestOn r-hub Fedora:

On rhub Fedora I get:

Ok
  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
