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
 # end plot of original shapefile 
  
  
  
  
   #read in functions from GitHub-SDraw folder
<<<<<<< HEAD
   lapply(list.files('C:/Users/rtupling/Documents/GitHub/SDraw/R/', full.names = TRUE, pattern = '.r'), source)    
  
=======
#    lapply(list.files('C:/Users/rtupling/Documents/GitHub/SDraw/R/', full.names = TRUE, pattern = '.r'), source)    
#   
>>>>>>> origin/ShinySDraw
    
   
   
   # 'run' function recognizes the shapefile as points, lines, or polygons  
  run <- reactive({
    #make text outside here
<<<<<<< HEAD
    text=paste(paste(tolower(method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shp))), 1, 
                                                    nchar(as.character(gsub('Spatial|DataFrame','',class(shp))))-1)),sep='.'), '(', 'n =', input$n, ',', 'shp = shape',')')
=======
    text=paste(paste(tolower(input$method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shape()))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shape()))))-1)),sep='.'), '(', 'n =', input$n, ',', 'shp = shape()',')')
>>>>>>> origin/ShinySDraw
    isolate({
       eval(parse(text=text))
    })})
   
  
  
  # for run button, execute above 'run' function for selected input$method (HAL, BAS, GRTS, SSS)
<<<<<<< HEAD
  observeEvent(input$Run, {run(paste(tolower(input$method, '.', text, sep = "")))}) 
  
=======
  #observeEvent(input$Run, {run(paste(tolower(input$method, '.', text, sep = "")))}) 
  output$shape2<-renderPlot({
    if(!input$Run)
      return(NULL)
      plot(run())
  })
>>>>>>> origin/ShinySDraw


  
  
  
  
  
  
  
  

  
  #for export button
  output$Export<-downloadHandler(
     filename = function(){paste('SDraw Sample','.shp', sep = '')},
    content = function(filename){
      writeOGR(shape, filename)
    }
  )

  
<<<<<<< HEAD
=======

  #for export button
  output$Export<-downloadHandler(
     filename = function(){paste('SDraw Sample','.shp', sep = '')},
    content = function(filename){
      writeOGR(shape, filename)
    }
  )

  
>>>>>>> origin/ShinySDraw
   #for quit button
   observeEvent(input$Quit, {stopApp()})
 
   } # end server code bracket




# testing code - delete when done
#    test = bas.polygon(n = 10, shp = shp)
#    plot(test)
<<<<<<< HEAD

=======
#    plot(shp)
>>>>>>> origin/ShinySDraw
#   method = input$method
#   #method = "bas"
#   
#   #Sample Size n input
#   #n = 10
#  n = input$n
#   
#   #shp will be the uploaded shapefile
#   shp = readOGR(input$shape)
#   #shp = readOGR('M:/IbatMigrationModel/Shapefiles/ProjectBoundaries/BeechRidgeProject','BeechRidgeProject')
#  class(shp)
# 
#   #read in functions from GitHub-SDraw folder
#   lapply(list.files('C:/Users/rtupling/Documents/GitHub/SDraw/R/', full.names = TRUE, pattern = '.r'), source)    
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
#   
#   
#   
#   runBAS <- reactive({
#     #make text outside here
#     text=paste(paste(tolower(method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shp))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shp))))-1)),sep='.'), '(', 'n =', n, ',', 'shp = shp',')')
#     isolate({
#       # PUT THE COOL STUFF INSIDE HERE
#        eval(parse(text=text))
#     })})
#   
#   ########################
#   # ---- BAS Sampling Functions ---- #
#   ########################
#   
#   
#   ########################
#   # ---- HAL Sampling Functions ---- #
#   ########################
#   
#   
#   
#   ########################
#   # ---- GRTS Sampling Functions ---- #
#   ########################
#   
#   
#   
#   ########################
#   # ---- SSS Sampling Functions ----#
#   ########################
#   
#   
#   
#   
#   #for run button
#   output$sample<-renderPlot(input$Run)
#   
#   #for export button
# #   output&export<-downloadHandler(
# #     filename = function(){paste('SDraw Sample', '.shp', sep = '')},
# #     content = function(filename){
# #       writeOGR()
# #     }
# #   )
#   #for quit button
#   observeEvent(input$Quit, {})
<<<<<<< HEAD
=======


>>>>>>> origin/ShinySDraw
