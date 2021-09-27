##  Version 1.1.2

Fixed CRAN check problem on Solaris. 
Fixed Debian error on auto check
Functions will now always return something and end.

## Test environments
* local OS X install, R 4.1.1
* rhub check_for_cran Debian
* rhub check_for_cran Ubuntu
* rhub macos-highsierra-release-cran
* rhub solaris-x86-patched-ods
* winbuilder (devel and release)

## R CMD check result

On Mac OS X  R 4.1.1 I get:

Duration: 1m 20.3s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

on winbuilder_release:

Status: OK


on winbuilder_develop:

Status: OK

rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC:
OK

rhub Debian Linux, R-release, GCC
OK

rhub macos-highsierra-release-cran
OK

rhub solaris-x86-patched-ods
OK
