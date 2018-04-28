library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  title = "Central Limit Theorem",
  
  h3("Sampling Distribution"), 
  hr(),
  
  fluidRow(
    column(12,
           p("The plots show the population distribution and sampling distribution of 
             the incomes of about 71,295 credit card applicants of a bank. Choose different 
             sample sizes and number of samples and observe how the sampling distribution changes."))
           ),
  
  br(),
  
  fluidRow(
    column(6,
           br(),
           sliderInput("n",
                       "Sample size (n)",
                       min = 1,
                       max = 50,
                       value = 10)
    ),
    column(6,  
           br(),
           sliderInput("N",
                       "Number of samples",
                       min = 10,
                       max = 1000,
                       value = 300)
    )
  ),
  hr(),
  h4("Summary Statistics and Plots", class = "text-center"),
  br(),
  fluidRow(
    column(6, offset = 3,
           tableOutput("stattable")
    )
  ),
  
  fluidRow(
    column(6,
           h5("Distribution of Income", class = "text-center"),
           br(),
           plotOutput("popPlot")
    ),
    column(6,  
           h5("Sampling Distribution of Mean Incomes", class = "text-center"),
           br(),
           plotOutput("sampPlot")
           
    )
  ),
  hr()
           ))
