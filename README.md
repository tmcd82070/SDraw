![CRAN/METACRAN](https://img.shields.io/cran/v/SDraw)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SDraw)](http://www.r-pkg.org/pkg/SDraw)
[![HitCount](http://hits.dwyl.com/tmcd82070/SDraw.svg)](http://hits.dwyl.com/tmcd82070/SDraw)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/SDraw)](http://cran.rstudio.com/web/packages/SDraw/index.html)
[![Build Status](https://travis-ci.org/tmcd82070/SDraw.svg?branch=master)](https://travis-ci.org/tmcd82070/SDraw)
[![codecov](https://codecov.io/gh/tmcd82070/SDraw/branch/master/graph/badge.svg)](https://codecov.io/gh/tmcd82070/SDraw)

# An R package to draw samples

**SDraw** draws five types of equal-probability samples from spatial objects, focusing on spatially balanced algorithms that are  especially suited for environmental monitoring.  **SDraw** implements the following:

* Halton Iterative Partitions (HIP)
* Balanced Acceptance Samples (BAS)
* Generalized Random Tesselation Stratified (GRTS)
* Simple Systematic Samples (SSS)
* Simple Random Samples (SRS)

The first four sample types (HIP, BAS, GRTS, and SSS) provide spatially balanced samples.  SRS does not.  Frame types can be points, lines, or polygons implemented as `SpatialPoints*`, `SpatialLines*`, or `SpatialPolygons*` objects.  

See package help (`help("SDraw")`) and help for function `sdraw` (`help("sdraw")`) to get started.  A list of available functions can be obtained with `help(package="SDraw")`. [For the newby: all of R is case sensitive.  `SDraw` is different than `sdraw`.] 

License: GNU General Public License

# Dependencies

The author has found it best to install dependencies before attempting to install **SDraw**. To install dependencies, execute the following: 
* `install.packages( c("spsurvey", "rgeos", "sp", "deldir"), repos="http://cran.r-project.org")`

`rgeos` requires java. Java should be installed *a priori*.  On Unix systems, the libraries needed for `rgeos` are cryptically named.  Google 'install rgeos unix'.   

# Installation

## From CRAN

The current stable release of **SDraw** can be installed like any other package: 
* `install.packages( c("SDraw"), repos="http://cran.r-project.org")`

## Using `devtools`

The current development version of **SDraw** can be installed from _GitHub_. Assuming `devtools` is installed and loaded, the following should work:

* `install_github("tmcd82070/SDraw")`


## From source 

* Download the source tarball (the `tar.gz`) from the [current release](https://github.com/tmcd82070/SDraw/releases)
* In R, execute the following: `install.packages( pkgs=file.choose(), type="source"" )`
* A choose-file dialog will appear.  Navigate to the `tar.gz` file and click "Open"


# After installation
Issue `library(SDraw)` at the command prompt.  


