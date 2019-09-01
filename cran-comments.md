## update Version 0.4.2

Should fix the notes and warnings in CRAN nightly builds

## Test environments
* local OS X install, R 3.6.1
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## R CMD check result

On Mac OS X I get:

Duration: 1m 17.8s

0 errors v | 0 warnings v | 0 notes v

On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
rxtracto    0.28   0.08    6.55
rxtracto_3D 0.10   0.01    5.87
rxtractogon 0.07   0.00    5.84
** found \donttest examples: check also with --run-donttest

On rhub Fedora I get:

Ok
  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
