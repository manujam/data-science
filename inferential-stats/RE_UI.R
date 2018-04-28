library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Discrete Random Variable - Frequency Distribution"),
  titlePanel(hr()), 
  
  
  # Action button
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"), 
      hr(),
      p("Click the button to simulate the game once."),
      br(),
      actionButton("action", label="Pick 4 balls!", class="btn btn-danger"),
      hr(),
      p("Number of games played (max limit 100):"), 
      fluidRow(column(6, verbatimTextOutput("length")))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("The (vector of) the number of red balls obtained in each trial is printed below."),
      fluidRow(column(12, verbatimTextOutput("value"))),
      plotOutput("redPlot")
      
    )
  )
))