# SDraw
An R package to draw samples

Description: This package draws several types of statistical samples, but focuses on spatially balanced samples that are  especially suited for environmental monitoring.  It draws BAS, GRTS, HAL, SSS and SRS samples from points, lines, and polygons.  An friendly Shiny interface is included for the common functions.  See `help(sdraw)` to get started. 

License: GNU General Public License

# Dependencies

The author has found it best to install dependencies before attempting to install SDraw. To install dependencies, execute the following: 
* `install.packages( c("spsurvey", "rgdal", "rgeos", "sp", "shiny", "deldir"), repos="http://cran.r-project.org")`

`rgdal` and `rgeos` require java routines, so java must be installed.  On Unix systems, the libraries needed for `rgdal` and `rgeos` are criptically named.  Google 'install rgdal unix'.   

# Installation

There are multiple ways to install the current version.  

## Using `devtools`

Assuming `devtools` is installed and loaded, the following should work:

* `install_github("tmcd82070/SDraw")`


## From source 

* Download the source tarball (the `tar.gz`) from the [current release](https://github.com/tmcd82070/SDraw/releases)
* In R, execute the following: `install.packages( pkgs=file.choose(), type="source"" )`
* A choose-file dialog will appear.  Navigate to the `tar.gz` file and click "Open"


# After installation
Issue `library(SDraw)` at the command prompt.  To bring up the Shiny interface (which is barely working as of version 2.1.2), issue `runUI()` at the command prompt.

# Bugs

https://github.com/tmcd82070/SDraw/issues