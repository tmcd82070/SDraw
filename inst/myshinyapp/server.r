server <- function(session,input, output){
  
  require(rgdal)
  
  
  #display the shapefile image when uploaded
  shape <- reactive({
    myshape<- input$shape
    if (is.null(myshape)) 
      return(NULL)       
    dir<-dirname(myshape[1,4])
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
    }
    getshp <- list.files(dir, pattern="*.shp", full.names=FALSE)
    getshp <- getshp[grep('.xml',getshp,invert= TRUE)]
    shape<-rgdal::readOGR(dir, gsub('.shp','',getshp), verbose = FALSE)
    })
  
   
  output$shape<-renderPlot({
    if(is.null(input$shape))
      return(NULL)
    plot(shape())
  })
  
  
  
#   method = input$method
#   
#   #shp will be the uploaded shapefile
#   shp = readOGR(input$shape)
#   #shp = readOGR('M:/IbatMigrationModel/Shapefiles/ProjectBoundaries/BeechRidgeProject','BeechRidgeProject')
#  class(shp)
# 
#   #read in functions from GitHub-SDraw folder
   lapply(list.files('C:/Users/rtupling/Documents/GitHub/SDraw/R/', full.names = TRUE, pattern = '.r'), source)    
#   
#   test = bas.polygon(n = 10, shp = shp)
#   
#   plot(test)
#   plot(shp)
#   
#   
#   #This stuff recognizes the shapefile as points, line, or polygons. 
#   eval(parse(text=paste(paste(tolower(method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shp))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shp))))-1)),sep='.'), '(', 'n =', n, ',', 'shp = shp',')')))
#   
#   
#   
#   
  run <- reactive({
    #make text outside here
    text=paste(paste(tolower(method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shp))), 1, 
                                                    nchar(as.character(gsub('Spatial|DataFrame','',class(shp))))-1)),sep='.'), '(', 'n =', input$n, ',', 'shp = shape',')')
    isolate({
      # PUT THE COOL STUFF INSIDE HERE
       eval(parse(text=text))
    })})
#   
#   
#   
#   #for run button
   output$sample<-renderPlot(input$Run)
#   
#   #for export button
  output$export<-downloadHandler(
     filename = function(){paste('SDraw Sample ', input$method, ' .shp', sep = '')},
    content = function(filename){
      writeOGR()
    }
  )
#   #for quit button
   observeEvent(input$Quit, {stopApp()})
 }
