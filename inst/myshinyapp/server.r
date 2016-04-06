server <- function(session,input, output){
  
  require(rgdal)
  require(shinydashboard)
  require(dismo)
  
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
      if (any(c(input$Img_terrain, input$Img_satellite) == TRUE)){
        bckgrnd = gmap(x=shape(), exp=1, 
                       type=ifelse(input$Img_terrain==TRUE,
                                   'terrain', 'satellite'), 
                       scale=1)
        plot(bckgrnd)
        plot(spTransform(shape(), bckgrnd@crs), col = 'white', pch = 21,
               bg = 'black', add = TRUE)
      } else {
        plot(shape(), pch = 16, col = 'black')
      }
    }
    if(input$Run){
      if (any(c(input$Img_terrain, input$Img_satellite) == TRUE)){
        points(spTransform(run(), bckgrnd@crs), col = 'red', pch = 16)
      } else {
        points(run(), col = 'red')
      }
    }
  })
 # end plot of original shapefile 
  
  

  
  run <- eventReactive(input$Run, {
    
    out = sdraw(x=shape(),
                n=input$n,
                type = input$method)
                     
  }, ignoreNULL=TRUE)
  


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

