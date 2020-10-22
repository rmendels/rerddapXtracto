## update Version 1.0.1

bug fix in  return structure of  'rxtracto()' 

## Test environments
* local OS X install, R 4.0.3
* local Fedora-32 install, R 4.0.2
* rhub check_for_cran Windows
* winbuilder (devel and release)

## R CMD check result

On Mac OS X  R 4.0.3 I get:

Duration: 2m 15.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

On Fedora-32, R 4.0.2,  I get:

Duration: 3m 20.8s

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
