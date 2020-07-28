## update Version 1.0.0

fixed problem in 'plotBBox' when name is given for colorbar

## Test environments
* local OS X install, R 4.0.2
* rhub check_for_cran Windows Server and Fedora
* win-builder (devel and release)

## R CMD check result

On Mac OS X  R 4.0.2 I get:

Duration: 1m 58.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

On Fedora-32, R 4.0.2,  I get:

Duration: 4m 19.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


On rhub Windows Server I get:

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
rxtracto    0.36   0.05    6.53
rxtracto_3D 0.11   0.02    5.78
rxtractogon 0.11   0.00    5.83


  
on winbuilder_release:

Ok

on winbuilder_develop:

Ok
