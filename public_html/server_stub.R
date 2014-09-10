
library(shiny)
library(ggplot2)

##<START>##

##<END>##

display<- function(S_0,K,B,r_f,vol,at,tc,dt,T,rerun,calculate,minsz,maxdelta,option_type){
  
  if (rerun>=0){
    
    nh=T/dt;
    vec_r_f=rep(r_f,nh);
    vec_vol=rep(vol,nh);
    
    if (option_type==0){
      pricer_func=bscallprice;
      pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
      check_args= checkargs_bscallpricer;
    }else{
      pricer_func=downandout_callprice;
      pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,B=B,tol=.001)
      check_args = checkargs_downandout_callpricer;
    }
    
    if (calculate==0){
      path = generate_path(S_0,r_f,vol,dt,T);
      bs=pricer_func(S_0=path$values,t=path$t,pricer_args);
      
      hp_deltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                     path_t=path$t,path_values=path$values,
                                     tc=0,at=at,
                                     pricerFunc=pricer_func,pricerArgs=pricer_args,
                                     minTradesize=minsz,maxTradedelta=maxdelta);
      
      hp_zerodpmethod=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                                      path_t=path$t,path_values=path$values,
                                      tc=0,at=at,
                                      pricerFunc=pricer_func,pricerArgs=pricer_args,
                                      minTradesize=minsz,maxTradedelta=maxdelta);
      
      par(mfrow=c(3,2));
      
      plot(path$t,main="Stock Price", xlab="Time",ylab='Stock Price',path$values,type='l')
      plot(path$t,main="Derivative Price", xlab="Time",ylab='Derivative Price',bs$price,type='l')
      
      plot(path$t,main="Delta", xlab="Time",ylab='Delta',hp_deltamethod$deltas,type='l')
      
      show_comparison (t=hp_deltamethod$t,
                       regular=(hp_zerodpmethod$nstocks),
                       special=(hp_deltamethod$nstocks),
                       tagname="Number of Stocks Shorted (n)");
      
      show_comparison (t=hp_deltamethod$t,
                       regular=hp_zerodpmethod$hedged_pos,
                       special=hp_deltamethod$hedged_pos,
                       tagname="Hedge Portfolio Value (P-nS)");
      
    } else
    {
      
      sd_dmk=array();
      mean_dmk=array();
      sd_zpk=array();
      mean_zpk=array();
      
      for ( k in seq(500)){
        
        path = generate_path(S_0,r_f,vol,dt,T);
        
        hp_deltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                       path_t=path$t,path_values=path$values,
                                       tc=0,at=at,
                                       pricerFunc=pricer_func,pricerArgs=pricer_args,
                                       minTradesize=minsz,maxTradedelta=maxdelta);
        
        hp_zerodpmethod=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                                        path_t=path$t,path_values=path$values,
                                        tc=0,at=at,
                                        pricerFunc=pricer_func,pricerArgs=pricer_args,
                                        minTradesize=minsz,maxTradedelta=maxdelta);
        
        sd_dmk[k]=sd(hp_deltamethod$hedged_pos);
        mean_dmk[k]=mean(hp_deltamethod$hedged_pos);
        sd_zpk[k]=sd(hp_zerodpmethod$hedged_pos);
        mean_zpk[k]=mean(hp_zerodpmethod$hedged_pos);
      }
      
      par(mfrow=c(2,2));
      
      hist(mean_dmk,main="Average Position (Delta-Method)")
      hist(mean_zpk,main="Average Position (Alternate-Method)")
      hist(sd_dmk,main="Position Stdev (Delta-Method)")
      hist(sd_zpk,main="Position Stdev (Alternate-Method)")
      
    } # {end: calcualte}
    
    
  } # {end: rerun}
  
}


shinyServer(
  function(input, output) {
    
    output$data  <- renderPlot(
      expr=display(S_0=input$S_0,
                   K=input$K,
                   B=input$B,
                   r_f=input$r_f,
                   vol=input$vol,
                   dt=input$dt,
                   at=input$at,
                   tc=input$tc,
                   T=input$T,
                   rerun=input$run,
                   calculate=input$calculate,
                   option_type=input$option_type,
                   minsz=input$minsz,
                   maxdelta=input$maxdelta
      ))
    #output$path <-renderPlot()
    
  }
)
