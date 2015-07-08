


require(spsurvey)
require(rgdal)

data(UT_ecoregions)
att <- UT_ecoregions@data

tapply(att$Area_ha,att$Level3_Nam,sum)

desktop <- 'C:/Users/jmitchell/Desktop/GRTS'
writeOGR(UT_ecoregions,desktop,"UT_ecoregions",driver="ESRI Shapefile",overwrite_layer=TRUE)
setwd(desktop)

# 3. Unstratified, equal probability, GRTS survey design
# first set is to create a list named Equaldsgn that contains info for specifying the survey design.

# --- 1. the survey is unstratified, so the list contains a single item that also is a list (named "None").
#        a. list "None" contains two items
#           (1). panel   -- used to specify the sample size for each panel -- here, assign "PanelOne = 115"
#           (2). seltype -- type of random selection -- here, assign "Equal" (this means equal probability selection)
# --- 2. the grts f'n in spsurvey selects the design (b/c it's geographic) via certain arguments
#        a. design: the named list of stratum design specs -- this is assigned the Equaldsgn list
#        b. DesignID: name for the design (use this to create a site ID for each site -- assigned value of "Equal")
#        c. type.frame: the type of frame, assigned "area" here (point/line for other sphfiles?)
#        d. src.frame: source of the frame, assigned "shapefile" to indicate a shapefile
#        e. in.shape: name of the input shapefile "UT_ecoregions" here
#        f. att.frame: the data frame of attributes associated with elements in the frame
#        g. shapefile: option to create a shapefile containing the survey design info (assigned FALSE)
# --- 3. during execution, console communicates info, for each stratum, about the number of GRTS levels. 

set.seed(4447864)                                                         # set seed
Equaldsgn <- list(None=list(panel=c(PanelOne=115), 
                            seltype="Equal"))      # create the design list (see above)
Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL",
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape="UT_ecoregions",
                   att.frame=att,
                   shapefile=FALSE)                                       # select the sample

# --- Look at the results!
Equalsites@data
dsgnsum(Equalsites)



# 4. Unstratified, unequal probability, GRTS survey design
# first set is to creat a list named Unequaldsgn that contains info for specifying the survey desing.
# --- Create the design list.


Unequaldsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype="Unequal",
                              caty.n=c("Central Basin and Range"=25,
                                       "Colorado Plateaus"=25,
                                       "Mojave Basin and Range"=10,
                                       "Northern Basin and Range"=10,
                                       "Southern Rockies"=10,
                                       "Wasatch and Uinta Mountains"=25,
                                       "Wyoming Basin"=10),
                              over=20))

Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="UNEQUAL",
                     type.frame="area",
                     src.frame="shapefile",
                     in.shape="UT_ecoregions",
                     att.frame=att,
                     mdcaty="Level3_Nam",
                     shapefile=FALSE)


# 5. Stratified, equal probability, GRTS survey design
                    # list name                  # vec    col     value
Stratdsgn <- list("Central Basin and Range"=list(panel=c(PanelOne=25),
                                                 seltype="Equal",
                                                 over=10),
                  "Colorado Plateaus"=list(panel=c(PanelTwo=25),
                                           seltype="Equal",
                                           over=4),                                           
                  "Mojave Basin and Range"=list(panel=c(PanelOne=10),
                                                seltype="Equal"),                                               
                  "Northern Basin and Range"=list(panel=c(PanelOne=10),
                                                  seltype="Equal"),                                                  
                  "Southern Rockies"=list(panel=c(PanelOne=10),
                                          seltype="Equal"),                                         
                  "Wasatch and Uinta Mountains"=list(panel=c(PanelOne=25),
                                                     seltype="Equal"),                                                    
                  "Wyoming Basin"=list(panel=c(PanelOne=10),
                                       seltype="Equal"))

shp <- UT_ecoregions

Stratsites <- grts(design=Stratdsgn,
                   DesignID="STRATIFIED",
                   type.frame="area",
                   src.frame="sp.object",
                   sp.object=shp,
                   att.frame=att,
                   stratum="Level3_Nam",
                   shapefile=FALSE)





# 6 - Continuous
# --- Create the design list.

Contdsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype="Continuous",
                              over=20))

Contsites <- grts(design=Contdsgn,
                     DesignID="CONT",
                     type.frame="area",
                     src.frame="shapefile",
                     in.shape="UT_ecoregions",
                     att.frame=att,
                     mdcaty="Area_ha",
                     shapefile=FALSE)








# make polygonal shpfile with category
dir <- '//LAR-FILE-SRV/Data/NPS/GitHub/2015.06.11/inst/doc/Shapefiles'
file <- 'SRI_Watersheds_shapefile'

SR.watershed <- readOGR(dir,file)

plot(SR.watershed)
head(SR.watershed@data)
SR.watershed@data$C <- SR.watershed@data$Shape_Leng^2 / SR.watershed@data$Shape_Area / 4 / pi
SR.watershed@data$Category <- ifelse(SR.watershed@data$C <= 3,"Low",ifelse(SR.watershed@data$C > 6,"High","Medium"))
SR.watershedJason <- SR.watershed
writeOGR(dsn=dir,obj=SR.watershedJason,layer="SRI_Watersheds_shapefile_J",overwrite_layer=TRUE,driver="ESRI Shapefile")

# make linear shpfile with category
dir <- '//LAR-FILE-SRV/Data/NPS/GitHub/2015.06.11/inst/doc/Shapefiles'
file <- 'SRI_Stream_Derived_Lengths_shapefile'
SR.stream <- readOGR(dir,file)

plot(SR.stream)
head(SR.stream@data)
SR.stream@data$Category <- ifelse(SR.stream@data$SHAPE_Leng <= 100,"Short",ifelse(SR.stream@data$SHAPE_Leng > 250,"Long","Medium"))
SR.streamJason <- SR.stream
writeOGR(dsn=dir,obj=SR.streamJason,layer="SRI_Stream_Derived_Lengths_shapefile_J",overwrite_layer=TRUE,driver="ESRI Shapefile")


# rm(list=ls()) 




















