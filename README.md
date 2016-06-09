# Dependencies

The author has found it best to install dependencies before attempting to install **SDraw**. To install dependencies, execute the following: 
* `install.packages( c("spsurvey", "rgdal", "rgeos", "sp", "deldir", "OpenStreetMap"), repos="http://cran.r-project.org")`

`rgdal` and `rgeos` require java routines, so java must be installed *a priori*.  On Unix systems, the libraries needed for `rgdal` and `rgeos` are criptically named.  Google 'install rgdal unix'.   

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


