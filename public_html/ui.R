library(shiny)
library(ggplot2)

shinyUI(
  pageWithSidebar(
       headerPanel("PNL Hedging"),
       sidebarPanel(
         actionButton("run", label = "Re-run"),
         
         numericInput("S_0", "Current Underlying Price",
                      100, min = 0, max = 1000, step = .01),
         numericInput("K", "Strike Price",
                      100, min = 0, max = 1000, step = .01),
         numericInput("T", "Time until Maturity",
                      10, min = 0, max = 100, step = .01),
         numericInput("at", "Amivest Ratio",
                      .5, min = 0, max = 100, step = .01),
         numericInput("tc", "Transaction costs (fraction)",
                      1, min = 0, max = 100, step = .01),
         
         sliderInput("vol", 
                     label = "Volatilty:",
                     min = 0, max = 1, value = .05, step=.05),
        
         sliderInput("dt", 
                     label = "Step Size:",
                     min = 0, max = 1, value = .05, step=.005),
         
         sliderInput("r_f", 
                     label = "RiskFree Rate:",
                     min = 0, max = 1, value = .05, step=.01)
         
         ),
       mainPanel(plotOutput("data"),textOutput("num_pressed"))
       
    )
  )
if (FALSE){
shinyUI(fluidPage(
  titlePanel("PNL Hedging"),
  fluidRow(
    
    numericInput("S_0", "Current Underlying Price",
                 100, min = 0, max = 1000, step = .01),
    numericInput("K", "Strike Price",
                 100, min = 0, max = 1000, step = .01),
    numericInput("T", "Time until Maturity",
                 10, min = 0, max = 100, step = .01),
    numericInput("at", "Amivest Ratio",
                 .5, min = 0, max = 100, step = .01),
    numericInput("tc", "Transaction costs (fraction)",
                 1, min = 0, max = 100, step = .01),
    
    sliderInput("vol", 
                label = "Volatilty:",
                min = 0, max = 1, value = .05, step=.01),
    
    sliderInput("dt", 
                label = "Step Size:",
                min = 0, max = 1, value = .05, step=.01),
    
    sliderInput("r_f", 
                label = "RiskFree Rate:",
                min = 0, max = 1, value = .05, step=.01),
    
           mainPanel(plotOutput("data"))
           
)))
}