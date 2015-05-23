\name{stratified.GUI}
\alias{stratified.GUI}

\title{Graphic User Interface (GUI) for selection of stratified samples}

\description{
Initiates a dialog box which provides a GUI to selection of stratified samples
from 2-D resources.
}

\usage{
stratified.GUI()
}

\details{
This routine is intended to be called from the 'SDraw' menu, but it can also 
be called from the command line in non-interactive environments. 
This routine uses the RGtk2 package windowing 
capabilities to construct a dialog box which pops up on the screen. In the 
dialog box, users specify at least the required input parameters, 
then press 'Run' to draw the sample.  

The requirements parameters are :  
\tabular{ll}{
1: \tab Select the 'Sampling Type' (HAL, BAS, GRTS, or SSS) in the top drop down list.\cr
2: \tab Specify the shapefile or 'sp' object that constitutes the sample frame, or click 'Browse' to browse for 
a shapefile (select the .shp file when browsing). Shapefiles or sp objects containing points, lines, and polygons are allowed. 
When specifying the name of a shape file (actually typing it in the box), do not include the .shp extention and recognize that all files associated with the shapefile must reside in the current working directory (see \code{getwd}, \code{setwd}).
Once the frame is specified and before other parameters are specified, 
the user can click the 'Inspect Frame' button
to plot the frame and list variables associated with objects in the frame.\cr
3: \tab Specify the name of the stratification variable.  This variable must be contained in the data attached to the spatial objects.  For example, this could be an elevational class associated with every point in the shapefile.  This name is case sensitive and must match that in the shapefile or 'sp' object exactly.  Constant values of this variable define the strata. \cr
4: \tab Specify the output object's R name. This
output object will be a 'SpatialPointsDataFrame' (see the 'sp' package)
containing the selected sample points in HAL, BAS or GRTS order.\cr
5: \tab Specify the sample size allocation scheme.  Available options are the following: 'proportional' to number, length, or size of all objects in each strata; 'constant' or same number in all strata; and 'user' defined or custom sample Size.  Depending on allocation scheme, the meaning of sample size changes. \cr
6: \tab Specify the sample size.  Interpretation of sample size changes based on the allocation scheme.  
\enumerate{
\item If the allocation scheme is 'proportional', enter one number for sample size.  This number of points will be distributed among strata based on the relative number of points, length of lines, or area of polygons in each strata.  
\item If the allocation scheme is 'constant', enter one number for sample size.  This number of points will be selected from each strata.  For example, if 50 is specified, 50 points will be selected from objects in each strata. 
\item If the allocation scheme is 'user', enter a list of numbers separated by commas.  If there are H strata in the frame, specify H numbers (one number per strata).  Order of sample sizes should be same as the \code{levels} of the strata variable, as if the stata variable were converted to a factor. In fact, to match sample sizes to strata, SDraw calls \code{factor} with the strata variable (and uses defaults values for other parameters of \code{factor}) and extracts the \code{levels} of this factor.  The order of these levels is  the order of samples sizes in the list.  For example, if the strata variable contains strings "low" and "high", converting this variable to a factor generally results in alphabetic  ordering of levels, as in \code{c("high", "low")}.  In this case, first number  in the specified list should be sample size in the "high" strata, second number should be number from the "low" strata, etc. The default ordering of levels when vectors are converted to factors is alphabetic, unless global options have changed. }\cr

} 


The optional parameters are: 
\tabular{ll}{
a: \tab The random number seed.  If the random number seed is specified, the same sample 
is output every time the 'Run' button is pressed.  In some cases, it may be 
important to replicate the sample draw exactly, and this is accomplished by 
specifying the same random number seed.  For HAL and BAS samples, the seed must be retained in order 
to extend the number of sampled units in each strata.  Any integer value is accpetable 
as the random number seed. \cr
b: \tab IF the sample type is GRTS, the number of 'over sample points' can be specified.  Over sample points are listed at the bottom of the output
object and can be identified using (output$pointType == "OverSample"), where 'output'
is the sample's R name.  Caution should be applied when asking for over sample points as spatial balance is affected when the oversample is much larger than the intended sample. \cr



Following is a description of the functioning of all buttons at the bottom of the dialog: 
\enumerate{


\item The 'Run' button: After all required and optional inputs are specified, the 'Run' button should be pressed and the sample will actually be drawn.  Output sample will be placed in a \code{SpatialPointsDataFrame} in the \code{.GlobalEnv} environment.  When the sample is complete, a confirmation dialog appears.  Until that time, it can appear that R is doing nothing (especially when drawing GRTS samples as they can take considerable time, 10's of minutes).  Be patient.  



\item The 'Inspect frame' button: After the shapefile or 'sp' object name is specified, pressing the 'Inspect Frame' button displays a plot of the frame and lists any variables in the data associated with each object. After the sample has been drawn, the 'Inspect Frame' button displays a plot of the study area as well as locations of the sample points. 
        
\item The 'View Sample' button: After the sample has been drawn, the 'View Sample' 
    button displays the sample points in a separate spreadsheet-like window.  
    No editing is allowed. 
    
\item The 'Export' button: After the sample has been drawn, the 'Export' button 
    brings up a save file dialog box which allows the user to specify the name
    of a file to contain the sample.  From this dialog box, the sample can be 
    exported in the following formats: ArcGIS shapefile (.SHP); Comma Separated (.CSV); 
    Google Earth (.KML); and Garmin format (.GPX). Shapefiles actually consist 
    of 3 or 4 files.  
    When exporting to a shapefile, 
    do not include the .SHP extension in the 'Name' field because the  
    associated files have different extensions. 
    
\item The 'Done' button: Dismisses the dialog box, leaving the sample object in the 
    .GlobalEnv workspace. 
    
}    
}

After the sample draw, one can check allocation sample sizes in each strata using the \code{table} function.  For example, if the output R name is 'samp', one can check sample sizes with \code{table(samp$strata)}.   One can plot the study area and sample points with \code{plot(frame); points(samp)}, assuming \code{frame} is the 'sp' object containing the frame


}

\value{
Nothing is returned by the function.  The sample object will be stored in the 
current workspace, and any export files will be on the hard drive. Any maps 
drawn during the life of the dialog will remain in R after the dialog closes. These graphs must be saved before R is closed if they are needed (see \code{dev.copy, jpg} and other graphics device functions)}

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
\code{\link{halton}}, \code{\link{grts.equi}}
}

\examples{
stratified.GUI()
}

\keyword{ design }
\keyword{ survey }
