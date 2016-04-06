shinyUI(fluidPage(
  
  fluidRow(column(1,img(src='S-draw.png', align='left')),column(11,titlePanel("SDraw"))),
  
  
  
  column(4,wellPanel(
    #fluidRow(h3('Sample parameters:')),
    fileInput("shape", h4("Upload Shapefile:"), 
              accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'), multiple = TRUE),
    selectInput("type", h4("Sample Type"), choices = list("Equiprobable"), selected = 1),
    selectInput("method", h4("Sample Method"), choices = list("HAL", "BAS", "GRTS", "SSS"), selected = 1),
    numericInput("n", h4("Sample Size"), 10, min = 1, max = 1000, step = 1))),

  column(5, wellPanel(
        plotOutput("shape"),
        actionButton("Run", label = "Take sample", icon = NULL, width = NULL),
        checkboxInput("Img_terrain", "Terrain", value=FALSE),
        checkboxInput("Img_satellite", "Satellite", value = FALSE))),
  
  column(3, wellPanel(
        selectInput(inputId = 'outputType', label = 'Export File Type:',
                choices = c('CSV'='CSV', 'SHP'='ESRI Shapefile', 'KML'='KML')),
        downloadButton("Export", label = "Export"),
        actionButton("Quit", label = "Quit", icon = NULL))),
   
  theme = "journal.css"))

