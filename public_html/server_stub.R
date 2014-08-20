
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
      
      hp_notcdeltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                         path_t=path$t,path_values=path$values,
                                         tc=tc,at=at,
                                         pricerFunc=pricer_func,pricerArgs=pricer_args,
                                         minTradesize=minsz,maxTradedelta=maxdelta);
      
      hp_notc=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                              path_t=path$t,path_values=path$values,
                              tc=tc,at=at,
                              pricerFunc=pricer_func,pricerArgs=pricer_args,
                              minTradesize=minsz,maxTradedelta=maxdelta);
      
      #    hp_tc=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,path_t=path$t,path_values=path$values,tc=tc,at=at,pricerFunc=pricer_func,pricerArgs=pricer_args,minTradesize=minsz,maxTradedelta=maxdelta);
      
      show_deltas (t=hp_notc$t,
                   path_values=path$values,
                   notc_deltas=hp_notc$deltas,
                   notc_hedged_pos=hp_notc$hedged_pos,
                   tc_hedged_pos=hp_notcdeltamethod$hedged_pos);
      
    } else
    {
      
      stdev_zerodp=array();
      mean_zerodp=array();
      stdev_delta=array();
      mean_delta=array();
      
      for ( k in seq(500)){
        path = generate_path(S_0,r_f,vol,dt,T);
        pos_zerodp=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                           path_t=path$t,path_values=path$values,
                                           tc=tc,at=at,
                                           pricerFunc=pricer_func,pricerArgs=pricer_args,
                                           minTradesize=minsz,maxTradedelta=maxdelta);
        
        stdev_zerodp[k]=sd(pos_zerodp$hedged_pos);
        mean_zerodp[k]=mean(pos_zerodp$hedged_pos);
  
        pos_delta=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                                path_t=path$t,path_values=path$values,
                                tc=tc,at=at,
                                pricerFunc=pricer_func,pricerArgs=pricer_args,
                                minTradesize=minsz,maxTradedelta=maxdelta);
        
        stdev_delta[k]=sd(pos_delta$hedged_pos);
        mean_delta[k]=mean(pos_delta$hedged_pos);
        
      }
      par(mfrow=c(2,2));
      hist(stdev_delta);
      hist(mean_delta);
      hist(stdev_zerodp);
      hist(mean_zerodp);
      
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
