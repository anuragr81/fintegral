library(shiny)
library(ggplot2)


dataset <- diamonds

shinyUI(fluidPage(
  titlePanel("PNL Hedging"),
  fluidRow(
    selectInput("var", 
                label = "Choose a variable to display",
                choices = c("Percent White", "Percent Black",
                            "Percent Hispanic", "Percent Asian"),
                selected = "Percent White"),
    column(3,
           textOutput("text1")
#           ,actionButton("action", label = "Simulate")  
           )
)))

if(FALSE){
shinyUI(pageWithSidebar(

  headerPanel("Diamonds Explorer"),

  sidebarPanel(

    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=500, round=0),

    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset))),
    
    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),

    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  ),

  mainPanel(
    plotOutput('plot')
  )
))

}