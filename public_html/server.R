library(shiny)
library(ggplot2)
source("../validation.r")

shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      paste("You have selected", input$var)
    })
    
  }
)

if (FALSE) {
  

shinyServer(function(input, output) {

  dataset <- reactive(function() {
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })

  output$plot <- reactivePlot(function() {

    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()

    print(p)

  }, height=700)

})
}