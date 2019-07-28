## update Version 0.4.1

## Test environments
* local OS X install, R 3.6.1
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)






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
