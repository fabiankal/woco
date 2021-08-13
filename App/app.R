library(shiny)

#Code outside server funciton will be run once per R session
#Code inside the server function will be run once per user
#Code inside a reactive function will be run once per reaction

#HTML('') raw html can be put in here between '' -> this line should be put into a fluidPage function

#Layout functions
#fluidRow()
#column(2, offset = 8) #specify the width and offset of each column out of 12
#wellPanel() #Groups elements into a grey "well"

#tabsetPanel(   #OR  #navlistPanel(
#tabPanel("tab 1", "contents"),
#tabPanel("tab 2", "contents"),
#tabPanel("tab 3", "contents"),
#)

#prepackaged layout:
  #sidebarLayout(
  #sidebarPanel(),
  #mainPanel()
  #)

  #navbarPage(title = "Title",
    #tabPanel("tab 1", "contents"),
    #tabPanel("tab 2", "contents"),
    #navbarMenu(title="more",
      #tabPanel("tab 3", "contents"),
      #tabPanel("tab 4", "contents"),
      #)
    #)

#library(shinydashboard)
#ui <- dashboardPage(
#  dashboardHeader(),
#  dashboardSidebar(),
#  dashboardBody()
#)

ui <- fluidPage(
  #*Input() functions,
  tags$h1("My first ",tags$strong("Shiny"), "app"),
  tags$hr(),
  wellPanel(
  fluidRow(
      column(5,
      sliderInput(inputId = "num",
                  label = "Choose a number",
                  value = 25, min = 1, max = 100)),
      column(5,
      textInput(inputId = "title",
                label = "Write a title",
                value = "Histogram of Random Normal Values")),
      column(2,actionButton(inputId = "clicks",label = "Update"))
    )
  ),
  #*Output() functions
  fluidRow(
    plotOutput(outputId = "hist"),
    column(6,offset=3,verbatimTextOutput("stats")),
    tags$hr(),
  ),
  fluidRow(
    actionButton(inputId = "norm", label= "Normal"),
    actionButton(inputId = "unif", label= "Uniform")
  ),
  fluidRow(
    plotOutput(outputId = "hist2"),
  ),
  tags$a(href = "www.rstudio.com", "RStudio"), # tags function is used to create html code
  tags$img(height=100,width=300,src = "http://www.rstudio.com/images/Rstudio.2x.png") # source also directly looks at the www folder
)


#data <- reactive( {  rnorm(input$num) }) # store reactive data for later use
#data <- isolate( {  rnorm(input$num) }) # do not react to changes in the value
#observeEvent(input$clicks, { print(input$clicks) }) #run code in the server only if first event (here input$clicks) changes
#observe({print(input$clicks)}) # will respond to every reactive value in the code
#data <- eventReactive(input$clicks, { rnorm(input$num) }) #react to values (only input$go changes) and then run only that code (used to delay reactions)
#rv <- reactiveValues(data = rnorm(100)) #creates a list of reactive values to manipulate

server <- function(input, output) {
  
  rv <- reactiveValues(data = rnorm(100))
  
  data <- eventReactive(input$clicks, { 
    rnorm(input$num) 
    }) 
  data2 <- eventReactive(input$clicks, {
    input$title
  })
  
  observeEvent(input$clicks, { 
    print(as.numeric(input$clicks)) 
    })
  output$hist <- renderPlot({ 
    hist(data(),main = data2()) # instead of data() also directly imputing rnorm(input$num) would be possible
    })
  output$stats <- renderPrint({
    summary(data())
  })
  
  observeEvent(input$norm, { rv$data <- rnorm(100)})
  observeEvent(input$unif, { rv$data <- runif(100)})
  
  output$hist2 <- renderPlot({
    hist(rv$data)
  })
}

shinyApp(ui = ui, server = server)

