\name{equi.GUI}
\alias{equi.GUI}

\title{Graphic User Interface (GUI) for selection of equi-probable samples}

\description{
Initiates a dialog box which provides a GUI to selection of equi-probable samples
from 2-D resources.
}

\usage{
equi.GUI()
}

%- maybe also 'usage' for other objects documented here.
\details{
This routine is intended to be called from the 'SDraw' menu, but it can also 
be called from the command line. This routine uses the RGtk2 package windowing 
capabilities to construct a dialog box which pops up on the screen. In the 
dialog box, users specify input parameters to other routines, 
then press 'Run' to draw the sample.  

The minimum requirements to drawn an equal probability 
sample using the dialog are as follows:
\tabular{ll}{
1: \tab Select the 'Sampling Type' (BAS, GRTS, or SSS) in the top drop down list.\cr
2: \tab Specify sample size in 'Sample Size (n)' box\cr
3: \tab Click 'Browse' button beside the 'Shape file' box to browse for 
a shapefile containing one or more 2-D polygons outlining the study area.  The 
name of a shape file (without the .shp extention) can also be 
typed into the 'Shape file' box, provided the shape file resides 
in the current directory (i.e., the one returned by \code{getwd()}). 
Once the shapefile is specified and before other parameters are specified, 
the user can click the 'Map' button
to display the shape file. This is a good way to check the study area
boundary.\cr
4: \tab Specify the output object's name in the 'Sample's R name' box. This
output object is a 'SpatialPointsDataFrame' (from the 'sp' package)
containing the selected sample points in BAS or GRTS order.\cr
5: \tab Hit 'Run'\cr
} 

Optional parameters are the number of points in the 'Over sample', and the
'Random number seed'.  

Over sample points are listed at the bottom of the output
object and can be identified using (output$pointType == "OverSample"), where 'output'
is the sample's R name.  

If a random number seed is specified, the same sample 
is output every time the 'Run' button is pressed.  In some cases, it may be 
important to replicate the sample draw exactly, and this is accomplished by 
specifying the same random number seed.   Any integer value is accpetable 
as the random number seed. 

The 'Run' button: After all required and optional inputs are specified, the 'Run'
    Button actually draws the sample and places it in the output 'SpatialPointsDataFrame' 
    object.

The 'Map' button: After the shapefile is specified, pressing the 'Map' button
        displays a plot of the shape file. After the sample has been drawn, 
        the 'Map' button displays a plot of the study area as well as locations 
        of the sample and over-sample (if present) points. 
        
The 'View Sample' button: After the sample has been drawn, the 'View Sample' 
    button displays the sample points in a separate spreadsheet-like window.  
    No editing is allowed. 
    
The 'Export' button: After the sample has been drawn, the 'Export' button 
    brings up a save file dialog box which allows the user to specify the name
    of a file to contain the sample.  From this dialog box, the sample can be 
    exported in the following formats: ArcGIS shapefile (.SHP); Comma Separated (.CSV); 
    Google Earth (.KML); and Garmin format (.GPX). Shapefiles actually consist 
    of 3 or 4 files.  
    When exporting to a shapefile, 
    do not include the .SHP extension in the 'Name' field because the  
    associated files have different extensions. 
    
The 'Done' button: Dismisses the dialog box, leaving the sample object in the 
    .GlobalEnv workspace. 
}
\value{
Nothing is returned by the function.  The sample object will be stored in the 
current workspace, and any export files will be on the hard drive. Any maps 
drawn during the life of the dialog will remain afte the dialog closes. }

\references{
Robertson, B. L., J. A. Brown, T. L. McDonald, and P. Jakson (2013). 
    BAS: Balanced acceptance sampling of natural resources. Biometrics 69, 776-784.
    
Stevens, D. L. and A. R. Olsen (2004). Spatially balanced sampling of 
natural resources. Journal of the American Statistical Association 99, 262-278.
}
\author{
Trent McDonald (tmcdonald@west-inc.com)
}


\seealso{
\code{\link{bas}}, \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{bas.point}}, \code{\link{sss.polygon}}, 
\code{\link{halton}}
}

\examples{
equi.GUI()
}

\keyword{ design }
\keyword{ survey }
