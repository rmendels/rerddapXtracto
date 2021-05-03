##  Version 1.1.0

Fixing present warnings and errors:

Believe existing problms are fixed. Two are from 3.6.x releases,  the DESCRIPION clearly states requires at least R4.0.0

## Test environments
* local OS X install, R 4.0.5
* rhub check_for_cran Debian
* rhub check_for_cran Ubuntu
* rhub macos-highsierra-release-cran
* rhub solaris-x86-patched-ods
* winbuilder (devel and release)

## R CMD check result

On Mac OS X  R 4.0.5 I get:

Duration: 1m 22.9s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

on winbuilder_release:

Installation time in seconds: 10
Check time in seconds: 147
Status: OK
R version 4.0.5 (2021-03-31)


on winbuilder_develop:

Installation time in seconds: 9
Check time in seconds: 140
Status: OK
R Under development (unstable) (2021-04-05 r80144)

rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC:
OK

rhub Debian Linux, R-release, GCC
OK

rhub macos-highsierra-release-cran
OK

rhub solaris-x86-patched-ods
OK
