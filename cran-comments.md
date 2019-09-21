## update Version 0.4.3

Fixes the warnings in the CRAN nightly builds
The DESCRIPTION file specifically says will not work with old-release

## Test environments
* local OS X install, R 3.6.1
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## R CMD check result

On Mac OS X I get:

Duration: 2m 4.4s

0 errors v | 0 warnings v | 0 notes v

On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
rxtracto    0.35   0.07    7.76
rxtracto_3D 0.08   0.02    6.18
rxtractogon 0.06   0.00    7.46
** found \donttest examples: check also with --run-donttest

On rhub Fedora I get:

Ok
  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
