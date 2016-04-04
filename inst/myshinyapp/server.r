server <- function(session,input, output){
  
  require(rgdal)
  require(shinydashboard)
  
  #increase file upload size to 30MB
  options(shiny.maxRequestSize=30*1024^2)
  
  #process shapefile inputs
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
  
  # show shapefile (& sample if produced)
  output$shape<-renderPlot({
    if(is.null(input$shape)){
     return(NULL)
    } else {  
     plot(shape())
    }
    if(input$Run){
     points(run(), col = 'red')
    }
  })
 # end plot of original shapefile 
  
  

   # 'run' function recognizes the shapefile as points, lines, or polygons  
   run <- reactive({
    
    text=paste(paste(tolower(input$method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shape()))), 
                                                          1, nchar(as.character(gsub('Spatial|DataFrame','',
                                                                                     class(shape()))))-1)),sep='.'),
               '(', 'n =', input$n, ',', 'shp = shape()',')')
    
    isolate({
      withProgress(message = 'Generating sample...',
                   value = 0.75,
                   {
      out = eval(parse(text=text))
      return(out)
      setProgress(1)
                   }) # end progress bar
    }) # end isolate 
    
  }) # end reactive
  
  # Only take sample when run button has been pressed
  observe({
    go <- input$Run
    if(go){
      run()
    }
  })
  
  # determine what the 'run' function is doing (what function is it calling)
  # output$text<-renderText({
  #   if(!input$Run)
  #     return(NULL)
  #     paste(text=paste(paste(tolower(input$method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shape()))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shape()))))-1)),sep='.')))
  #   })
  # 

  filename = paste('data-', gsub('-| |\\:','',Sys.time()),sep='')
  
  #for export button
  #currently handles .csv and .shp file types - not working
  output$Export<- downloadHandler(
    filename = filename,
    content = function(file){
      if(input$outputType == 'CSV'){
        write.csv(run()@data, file=paste0(choose.dir(),'\\',filename,'.csv'))
      } 
      #if(input$outputType == 'SHP'){
      if(input$outputType != 'CSV'){
        writeOGR(obj=run(),
                 #dsn='c:/users/mkauffman/downloads',
                 dsn = choose.dir(),
                 layer=filename,
                 #driver='ESRI Shapefile')
                 driver = input$outputType)
      }
      # if(input$outputType == 'KML'){
      #   writeOGR(obj = run(),
      #            dsn = choose.dir(),
      #            layer = filename,
      #            driver = input$outputType)
      # }
    }
  )

  
   #for quit button
   observeEvent(input$Quit, {stopApp()})
 
   } # end server code bracket



