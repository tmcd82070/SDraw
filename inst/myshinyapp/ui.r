shinyUI(fluidPage(
  
  titlePanel("Sample Draw"),

  shinydashboard::box(wellPanel(
    fluidRow(h3('Please select desired sampling options below.')),
    fluidRow(selectInput("type", h4("Sampling Type"), choices = list("Equiprobable", "Stratified", "Variable"), selected = 1, width = '80%')),
    selectInput("method", h4("Sampling Method"), choices = list("HAL", "BAS", "GRTS", "SSS"), selected = 1),
    numericInput("n", h4("Sample Size"), 10, min = 1, max = 1000, step = 10),
    fileInput("shape", h4("Upload Shapefile:"), 
              accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'), multiple = TRUE),
    fluidRow(h3('Please preview your shapefile to the right.'))
  ), width = 4),

  shinydashboard::box(
      wellPanel(
        plotOutput("shape"),
        actionButton("Run", label = "Take sample", icon = NULL, width = NULL), 
        width = 4, height =2)
  ),
  
  shinydashboard::box( wellPanel(
        selectInput(inputId = 'outputType', label = 'Export File Type:',
                choices = c('CSV'='CSV', 'SHP'='ESRI Shapefile', 'KML'='KML')),
        downloadButton("Export", label = "Export"),
        actionButton("Quit", label = "Quit", icon = NULL), 
        width = 4))
  ,
  
  theme = "journal.css"))

