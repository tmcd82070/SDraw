# SDraw
An R package to draw samples

Description: This package draws several types of statistical samples, but focuses on 
spatially balanced samples that are  especially suited for environmental monitoring.  
It draws BAS, GRTS, HAL, and SSS samples from points, lines, and polygons.  An friendly 
user interface is included for the common functions. 

License: GNU General Public License

# Installation

There are multiple ways to install.  One is from source, the other is from the binary build. 

## From source 

* Download the source tarball (the `tar.gz`) from the [current release](https://github.com/tmcd82070/SDraw/releases)
* In R, execute the following: `install.packages( pkgs=file.choose(), type="source"" )`
* A choose-file dialog will appear.  Navigate to the `tar.gz` file and click "Open"

## From binary build

* Download the binary build zip file from the link provided in the [current release notes.](https://github.com/tmcd82070/SDraw/releases) 
* In R, select "Install from local zip file..." from the "Packages" menu in the standard R interface. Or, execute the following: `install.packages( pkgs=file.choose(), type="binary")` 
* Navigate to the binary build .zip and click Open 

## Dependencies

The above methods should install all dependencies. If not, execute the following: 
* `install.packages( c("RGtk2", "spsurvey", "rgdal", "rgeos", "sp"), repos="http://cran.r-project.org")`

# After installation
Issue `library(SDraw)` at the command prompt.  The first time you do this, R will ask whether you want to install the GTK+ runtime library.  Say 'yes'.  At the end of this, R may give you some sort of error.  Ignore it.  Restart R. 


Re-start R and issue `library(SDraw)`.  An 'SDraw' menu item will appear in your R gui if you are using Windows and the default R console.  From the "SDraw" menu, choose the appropriate action.   The dialog boxes that pop up should be self explanatory.  See `help(package="SDraw")` for a list of functions.  The functions that open the dialog boxes are `equi.GUI()` and `stratified.GUI()` .  

# Bugs

https://github.com/tmcd82070/SDraw/issues