library(shiny)
library(ggplot2)
library(ggthemes)

# Define server logic required to draw population density plot
shinyServer(function(input, output) {
  bank <- read.csv("https://raw.githubusercontent.com/kshitijjain91/Credit-Risk-Capstone/master/datasets/Demogs_v1.csv")
  
  output$sampPlot <- renderPlot({
    v = vector(mode="integer", length = input$N)
    income <- bank$Income
    for (sample_num in 1:input$N){
      s <- sample(income, input$n, replace=F)
      v[sample_num] <- mean(s)
    }
    v <- data.frame(v)
    ggplot(v, aes(v)) + geom_density(colour="black", fill="blue", alpha=0.3) + 
      xlab("Sample Mean of Income") + ylab("Probability Density")+
      coord_cartesian(xlim=c(15,40))+theme_minimal() + theme(text = element_text(size=14))
  })
  
  output$popPlot <- renderPlot({
    income <- bank$Income
    income <- data.frame(income)
    ggplot(income, aes(income)) + geom_density(colour="black", fill="#f98866", alpha=0.4) + xlab("Income") + ylab("Probability Density") + 
      theme_minimal() + theme(text = element_text(size=14))
  })
  
  output$stattable <- renderTable({
    income <- bank$Income
    pop_mean <- mean(income, na.rm=T)
    pop_sd_by_root_n <- sd(income, na.rm = T)/sqrt(input$n)
    
    v = vector(mode="integer", length = input$N)
    for (sample_num in 1:input$N){
      s <- sample(income, input$n, replace=F)
      v[sample_num] <- mean(s)
    }
    
    mean_samp_means <- mean(v, na.rm = T)
    standard_error <- sd(v, na.rm=T)
    
    d = data.frame(pop_mean, mean_samp_means, pop_sd_by_root_n, standard_error)
  })
})
