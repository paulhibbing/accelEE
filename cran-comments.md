## Resubmission

This version of 'accelEE' is being submitted to address an earlier failure on Fedora GCC. The failure was tied to a dependency (EE.Data, v0.1.1), which has since been fixed and re-released (v0.2.0). Thus, the main change in this resubmission of 'accelEE' is to explicitly require 'EE.Data' >=0.2.0.


## Test environments

* Local Windows 11 install, R 4.5.3
* win-builder (devel and release)
* Ubuntu Linux 24.04.4 LTS, R-devel (on R-hub)


## R CMD check results

0 errors | 0 warnings | 1 note


* checking CRAN incoming feasibility ... [20s] NOTE (20.3s)
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'
   
  New submission
   
  Package was archived on CRAN
   
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2026-06-29 as issues were not corrected in time.

Possibly misspelled words in DESCRIPTION:
  Ellingson (14:8)
  al (13:8, 14:21)
  et (13:5, 14:18)
  
  [This is a new submission. The possibly-misspelled words are false positives.]
