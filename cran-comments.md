##  Version 1.1.1

Fixed 'rxtracto()' example error

##  Version 1.1.0

Fixing present warnings and errors:

Believe existing problms are fixed. Two are from 3.6.x releases,  the DESCRIPION clearly states requires at least R4.0.0

## Test environments
* local OS X install, R 4.1.0
* rhub check_for_cran Debian
* rhub check_for_cran Ubuntu
* rhub macos-highsierra-release-cran
* rhub solaris-x86-patched-ods
* winbuilder (devel and release)

## R CMD check result

On Mac OS X  R 4.1.0 I get:

Duration: 1m 18.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

on winbuilder_release:

Installation time in seconds: 7
Check time in seconds: 119
Status: OK
R version 4.1.0 (2021-05-18)


on winbuilder_develop:

Installation time in seconds: 5
Check time in seconds: 87
Status: OK
R Under development (unstable) (2021-05-28 r80404)

rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC:
OK

rhub Debian Linux, R-release, GCC
OK

rhub macos-highsierra-release-cran
OK

rhub solaris-x86-patched-ods
OK
