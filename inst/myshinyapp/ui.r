shinyUI(fluidPage(
  
  titlePanel("Sample Draw"),
  
  navlistPanel(
    "Sampling Type",
    tabPanel("Equiprobable", h3("Equiprobable")),
    tabPanel("Stratified", h3("Stratified")),
    tabPanel("Variable", h3("Variable"))
  ),
  navlistPanel(
    "Sampling Method",
    tabPanel("HAL", h3("Halton Lattice Sampling")),
    tabPanel("BAS", h3("BAS Sampling")),
    tabPanel("GRTS", h3(" ")),
    tabPanel("SSS", h3(" "))
  ),
  numericInput(
    "n",
    "Sample Size",
    100, min = 1, max = 1000
  ),
  sidebarLayout(
  sidebarPanel(
    fileInput("shape", "Upload Shapefile Here", accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj'), multiple = TRUE),
    #actionButton("View", label = "View Shapefile", icon = NULL, width = NULL),
    
    width = 4
    ),
  mainPanel(
    "View Shapefile", 
    fluidRow(plotOutput("shape"))
      #plotOutput("shape"))
    #, width = "100%", height = "100%")
  )),
  sidebarPanel(
    actionButton("Run", label = "Run", icon = NULL, width = NULL), 
    actionButton("Export", label = "Export", icon = NULL, width = NULL), 
    actionButton("Quit", label = "Quit", icon = NULL, width = NULL)
  )
, theme = "journal.css"))
