## Current version: SDraw 2.1.9

## Previous submission: SDraw 2.1.8
* On about 25-mar-2020, grts.lines and grts.polygon examples 
began timing out on linux builds.  I noticed this, 
and I recieved emails from 
Kurt Hornik and Brian Ripley to this effect.  Window builds
work.  Issue seems to be udate of the Debian system 
to proj7. 

The time-out occurred in spsurvey::grts.  
I am not the author of spsurvey.  I note that 
spsurvey::grts creates a temporary directory, and that 
I have had issues with that on Linux before.  I was able 
to reduce sample size and both grts samples now run; but, 
they take > 5s each. I wrapped the grts.line and 
grts.polygon examples in \dontrun{} and noted 
why.


## R version 
R 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

## Test environments
* local Windows 10 Pro
* passes devtools::check_win_devel()
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