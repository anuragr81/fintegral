library(shiny)
library(ggplot2)

##########################################

generate_path <- function (S_0,r_f,vol,dt,T){
  t=0
  n=T/dt;
  z=rnorm(n-1);
  tarray=seq(0,n-1)*dt
  vec=cumsum((r_f-vol*vol/2)*dt +vol*z*sqrt(dt))
  values=S_0*exp(vec)
  values=c(S_0,values) # appending initial value
  return (data.frame(values=values,t=tarray));
}
##########################################

shinyServer(
  function(input, output) {
    path = generate_path(S_0=100,r_f=.05,vol=.1,dt=.01,T=1);
    #output$plot <- ggplot(path$values, aes_string(x=input$x, y=input$y)) + geom_point()
    output$data  <- renderPlot( 
                      expr=plot(path$t,path$values,type='l')
                       )
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