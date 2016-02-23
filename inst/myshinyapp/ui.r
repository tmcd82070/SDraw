shinyUI(fluidPage(
  
  titlePanel("Sample Draw"),

  column(4,
    selectInput("type", h3("Sampling Type"), choices = list("Equiprobable", "Stratified", "Variable"), selected = 1)
  ),
  
  column(4,
    selectInput("method", h3("Sampling Method"), choices = list("HAL", "BAS", "GRTS", "SSS"), selected = 1)
  ),
  
  column(4,
  numericInput("n", "Sample Size", 100, min = 1, max = 1000, step = 10)
  ),
  
  sidebarLayout(
  sidebarPanel(
    fileInput("shape", "Upload Shapefile Here", 
              accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'), multiple = TRUE),
    width = 4
    ),
  
  mainPanel(
    "View Shapefile", 
    fluidRow(plotOutput("shape")), 
    width = 12
  )),
  
  sidebarPanel(
    actionButton("Run", label = "Run", icon = NULL, width = NULL), 
    downloadButton("Export", label = "Export"), 
    actionButton("Quit", label = "Quit", icon = NULL, width = NULL)
  ), 
  
  theme = "journal.css"))
