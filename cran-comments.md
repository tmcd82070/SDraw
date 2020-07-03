## Current submission: SDraw 2.1.13

## Current version on CRAN: SDraw 2.1.11

* Version 2.1.12 failed incoming CRAN tests on 
`checking data for ASCII and uncompressed saves ... WARNING`. 
Resolved using `save(..., compress = 'xz')`

* Version 2.1.11 failed on Linux systems after change 
in GEOS package. New version resolves

> test_check('SDraw')
  1. Failure: bas.point() operates appropriately (@test-bas.point.R#19)
  testSamp <- bas.point(spdf, 20) produced warnings.
  

## R version 
R 4.0.0 (2020-04-24) -- "Arbor Day"

## Test environments
* local Windows 10 Pro
* passes devtools::check_win_devel()
* passes devtools::check_rhub()
* Linux (via travis)

## R CMD check --as-cran results
*No ERRORs.  
*No WARNINGs: 

## Spell Check

* via devtools::spell_check()
* All miss-spellings are part of a url, variable names, 
acceptable abbreviations, or are in fact correct.

## Downstream dependencies
spatialfusion

## Thank you!
Thank you to the R team for their service to the R community. 