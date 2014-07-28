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

display<- function(S_0,K,r_f,vol,dt,T){
  print(S_0);
  path = generate_path(S_0=S_0,r_f=r_f,vol=vol,dt=dt,T=T);
  plot(path$t,path$values,type='l')
}
shinyServer(
  function(input, output) {
    output$data  <- renderPlot( 
                      expr=display(S_0=input$S_0,
                                   K=input$K,
                                   r_f=input$r_f,
                                   vol=input$vol,
                                   dt=input$dt,
                                   T=input$T)
                       )
  }
)
