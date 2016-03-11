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
  
  

   # 'run' function recognizes the shapefile as points, lines, or polygons  
  run <- reactive({
    #make text outside here
    text=paste(paste(tolower(input$method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shape()))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shape()))))-1)),sep='.'), '(', 'n =', input$n, ',', 'shp = shape()',')')
    isolate({
       eval(parse(text=text))
    })})
  

  
  # for run button, execute above 'run' function for selected input$method (HAL, BAS, GRTS, SSS)
  #observeEvent(input$Run, {run(paste(tolower(input$method, '.', text, sep = "")))}) 
  output$shape2<-renderPlot({
    if(!input$Run)
      return(NULL)
      plot(run())
  })
  
  # determine what the 'run' function is doing (what function is it calling)
  output$text<-renderText({
    if(!input$Run)
      return(NULL)
      paste(text=paste(paste(tolower(input$method),tolower(substr(as.character(gsub('Spatial|DataFrame','',class(shape()))), 1, nchar(as.character(gsub('Spatial|DataFrame','',class(shape()))))-1)),sep='.')))
    })
  

  #for export button
  output$Export<-downloadHandler(
    filename = function(){
      paste(input$type, input$method, 'SDraw Sample', '.zip', sep = ' ')
    },
    content = function(file){
      writeOGR(shape2(), dsn = paste(input$shape, '.shp', sep = ''), driver = 'ESRI Shapefile')
      zip(zipfile = paste(input$type, input$method, 'SDraw Sample', '.zip', sep = ' '), files = Sys.glob('shape2.*'))
      }
    )

  
   #for quit button
   observeEvent(input$Quit, {stopApp()})
 
   } # end server code bracket



