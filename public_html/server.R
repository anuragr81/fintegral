
library(shiny)
library(ggplot2)

##<START>##

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

checkargs_bscallpricer<- function(pricerArgs){
  if (min(pricerArgs$K)<=0){
    return(FALSE);
  }
  if (min(pricerArgs$r_f)<0){
    return(FALSE);
  }
  if (min(pricerArgs$vol)<0){
    return(FALSE);
  }
  if (min(pricerArgs$T)<=0){
    return(FALSE);
  }
  return(TRUE);
}


bscallprice <- function(S_0,t,pricerArgs){
    K=pricerArgs$K;
    r_f=pricerArgs$r_f;
    vol=pricerArgs$vol;
    T=pricerArgs$T;
    #print(paste("S_0=",S_0,"K=",K,"r_f=",r_f,"T=",T,"t=",t));
    numerator=log(S_0/K) + (r_f+(vol*vol)*0.5)*(T-t);
    denominator=vol*sqrt(T-t);
    d1=(numerator/denominator);
    d2=d1-vol*sqrt(T-t);
    price=(S_0*pnorm(d1)-K*exp(-r_f*(T-t))*pnorm(d2));
    delta=pnorm(d1);
    gamma=dnorm(d1)/(S_0*vol*sqrt(T-t));    
    return (data.frame(price=price,delta=delta,gamma=gamma));
}

value_downandout_func <- function(S,K,B){
  # if S shoots up above B payoff is S_T-K
  if (min(S)<B){
    return(0);
  }else{
    return(max(S[length(S)]-K,0));
  }
}


value_downandout <- function (npaths,S_0,K,B,r_f,vol,dt,T) {
  
  res=array(0);
  out=array(0);
  
  for ( i in seq(npaths)) {
    paths=generate_path(S_0=S_0,r_f=r_f,vol=vol,dt=dt,T=T)
    out[i]=value_downandout_func(S=paths$values,K=K,B=B);
  }
  
  res=(sum(out)/length(out))*exp(-r_f*T);
  sg_c=sd(out);
  
  values_left=res-1.96*sg_c/sqrt(npaths);
  values_right=res+1.96*sg_c/sqrt(npaths);
  
  return (data.frame(left=values_left,right=values_right));
  
}

value_downandout_exp <- function(S_0,t,pricerArgs) {
  r_f=pricerArgs$r_f;
  vol=pricerArgs$vol;
  B=pricerArgs$B;
  
  cbs=bscallprice(S_0=S_0,t=t,pricerArgs=pricerArgs);
  cbsi=bscallprice(S_0=(B*B/S_0),t=t,pricerArgs=pricerArgs);
  n1=cbs$price;
  n2=((S_0/B)^(1-(2*r_f/(vol*vol)))*cbsi$price)
  return(n1-n2);
  #ts.plot(S_0,ts((S_0[1]^2)/S_0)) # <-- beauty
  
}

checkargs_downandout_callpricer<- function(pricerArgs){
  if(pricerArgs$tol<=0 && pricerArgs$tol<=1 ){
    return(FALSE);
  }
  if (min(pricerArgs$K)<=0){
    return(FALSE);
  }
  if (min(pricerArgs$r_f)<0){
    return(FALSE);
  }
  if (min(pricerArgs$vol)<0){
    return(FALSE);
  }
  if (min(pricerArgs$T)<=0){
    return(FALSE);
  }  
  return(TRUE);
}

downandout_callprice <-function(S_0,t,pricerArgs){
  tol=pricerArgs$tol;
  cbs=value_downandout_exp(S_0=S_0,t=t,pricerArgs=pricerArgs);
  dS=tol;
  cbsnext=value_downandout_exp(S_0=S_0+dS,t=t,pricerArgs=pricerArgs)
  delta=((cbsnext-cbs)/dS);
  return(data.frame(price=cbs,delta=delta));
}

hedged_position <- function (path_t,path_values,
                             tc,at,
                             checkArgs,pricerFunc,pricerArgs,
                             stepFunc,minTradesize,maxTradedelta) {
  isPrint<-FALSE;

  nh = length(path_values);
  option_prices=array();
  hedged_pos=array();
  if (!checkArgs(pricerArgs)){
    stop("Improper Pricer Args.")
  }
  bs=pricerFunc(S_0=path_values,t=path_t,pricerArgs);
  nShortedStocks = bs$delta[1];
  hedged_pos[1] =  pnl_value(S_t=path_values[1],P_t=bs$price[1],nShorted=nShortedStocks,at=at,tc=tc)
  
  if (isPrint){
    print(paste("hedged_pos[1]=",hedged_pos[1]));
  }
  
  for ( i in seq (2,nh) ) {
    
    dS= path_values[i] - path_values[i-1];
    if (isPrint){
      print(paste("dS=",dS,"nShortedStocks=",nShortedStocks))
    }
    
    nShort=stepFunc(tc=tc,
                    amivest=at,
                    underlying_price=path_values[i],
                    delta=bs$delta[i-1],
                    nxtdelta=bs$delta[i],
                    nShortedStocks=nShortedStocks,
                    dS=dS);
    if (abs(nShort)<minTradesize  ) {
      hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,at=at,tc=tc);
#      print(paste("Do nothing since suggested size(",nShort,") is smaller than threshold(",minTradesize,")"));
    } else {
      
      if (abs(bs$delta[i])>maxTradedelta)
      {
        hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,at=at,tc=tc);
#        print(paste("Do nothing since current delta(",bs$delta[i],") is greater than threshold(",maxTradedelta,")"));        
      }else{
        
        if (isPrint){
          print(paste("Before: nShort(to short)=",nShort,"nShortedStocks=",nShortedStocks))
        }
        nShortedStocks = nShortedStocks + nShort;
        if (isPrint){
          print(paste("After: nShort(shorted)=",nShort,"nShortedStocks=",nShortedStocks))
        }
        hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,at=at,tc=tc);
        if (isPrint){
          print(paste("hedged_pos[",i,"]=",hedged_pos[i]));
          print(paste("price=",bs$price[i],"nshort=",nShort,"nShortedStocks=",nShortedStocks,"Position=",hedged_pos[i]));
        }
      }# end if (maxTradedelta)
    } # end if (maxTradesize)
#    readline();
  } # end for
  return(data.frame(hedged_pos=hedged_pos,t=path_t,deltas=bs$delta,bsprice=bs$price));
}

num_stocks_to_short_zerodp <- function(underlying_price,tc,delta,nxtdelta,nShortedStocks,dS){
  # Check if x<0 or x>0 conditions apply. If neither works, return x=0 (don't hedge) and report error
  numerator=(delta-nShortedStocks)*dS
  if (numerator>0){
    denominator=underlying_price+tc # always +ve
    return (numerator/denominator)
  } else {
    denominator=underlying_price-tc # for -ve return values
    if (denominator>0){
      return(numerator/denominator); # -ve return value
    } else {
      print("Error: invalid hedge parameters");
      return (0); # do nothing
    }
  }
  #return ((1/(tc+underlying_price))*((delta-nShortedStocks)*dS));
}

num_stocks_to_short_zerodp_g <- function(underlying_price,tc,amivest,delta,nxtdelta,nShortedStocks,dS){
  # Check if x<0 or x>0 conditions apply. If neither works, return x=0 (don't hedge) and report error
  #print(paste("S_t=",underlying_price,"n=",nShortedStocks,"amivest=",amivest,"tc=",tc))
  if (amivest==0){
    return(num_stocks_to_short_zerodp(underlying_price=underlying_price,tc=tc,delta=delta,nShortedStocks=nShortedStocks,dS=dS,nxtdelta=nxtdelta));
  }
  
  A  = 2*amivest
  B1 = underlying_price-nShortedStocks*amivest+tc
  D1 = (underlying_price-nShortedStocks*amivest+tc)^2-4*amivest*(delta-nShortedStocks)*dS; # x>0
  B2 = underlying_price-nShortedStocks*amivest-tc
  D2 = (underlying_price-nShortedStocks*amivest-tc)^2-4*amivest*(delta-nShortedStocks)*dS; # x<0
  #print(paste("D1=",D1))
  #print(paste("D2=",D2))
  
  if ( (D1<0) && (D2<0) ){
    print("Error: invalid hedge parameters (D1,D2) <0");
    return (0); # do nothing
  }
  if (D1<0) {
    # D2> 0, x<0
    xO1=(B2+sqrt(D2))/A
    xO2=(B2-sqrt(D2))/A
    if (xO1<0){
      if (xO2<0) {
        return(max(xO1,xO2))
      }
      else {
        return (xO1);
      } 
    } else {
      if (xO2<0) {
        return(xO2);
      } else {
        print("Error: hedging params (D2,xO)>0)")
        return (0);
      }
    } 
  }
  if (D2<0) {
    # D1>0, x>0
    xT1=(B1+sqrt(D1))/A
    xT2=(B1-sqrt(D1))/A
    if (xT1>0){
      if (xT2>0){
        return(min(xT1,xT2));
      } else {
        return(xT1);
      }
    } else {
      if (xT2>0){
        return(xT2);
      }else{
        print("Error: hedging params (D2,xT)<0)")
        return (0);
      }
    }  
  }
  #D1>0 and D2>0
  sol1=0
  xO1=(B2+sqrt(D2))/A # select(xO1,xO2) <0
  xO2=(B2-sqrt(D2))/A # 
  if (xO1<=0) {
    if (xO2<=0){
      sol1=max(xO1,xO2);
    }else {
      sol1=xO1;
    }
  } else {
    if (xO2 <=0 ){
      sol1=xO2
    }
  }
  sol2=0;
  xT1=(B1+sqrt(D1))/A # select (xT1,xT2)> 0
  xT2=(B1-sqrt(D1))/A
  if (xT1>=0){
    if (xT2>=0){
      sol2=min(xT1,xT2);
    } else {
      sol2=xT1;
    }
  } else {
    if (xT2>=0){
      sol2=xT2;
    }
  }
  
  if (sol1==0){
    return (sol2);
  } 
  if (sol2==0){
    return(sol1);
  }
  
  if (abs(sol1)>abs(sol2)){
    return(sol2);
  } else {
    return(sol1);
  }
  
  #print(paste("xO1=",xO1,"xO2=",xO2,"xT1=",xT1,"xT2=",xT2))
  
}


num_stocks_to_short_deltas <- function(underlying_price,amivest,tc,delta,nxtdelta,nShortedStocks,dS){
  return(nxtdelta-delta);
}

num_stocks_to_short_direct <- function(underlying_price,dP,nShortedStocks,dS){
  return ((1/underlying_price)*(dP-nShortedStocks*dS));
}

pnl_value <- function(S_t,P_t,nShorted,at,tc) {
  return (P_t-nShorted*(S_t-nShorted*at)-abs(nShorted)*tc);
}

show_deltas <- function(t,path_values,deltas,regular_hedged_pos,special_hedged_pos) {
  
  par(mfrow=c(1,2));
  # show underlying and delta
  factor=10^(as.integer(log(max(path_values),10)));
  #a = a/factor X factor
  plot(0,0,xlab="Time", ylab=paste("price/",factor), 
       xlim=c(0,max(t)),ylim=c(-10,10));
  cl<-rainbow(2);
  lines(t,path_values/factor,col=cl[1],lty=1)
  lines(t,deltas,col=cl[2],lty=2);
  legend(.1,10,c(paste("UnderlyingPrice/",factor),
                                          "Delta"),
         col=cl, lty=c(1,2));
  
  plot_ylim<-c(min(min(regular_hedged_pos),min(special_hedged_pos)),max(max(regular_hedged_pos),max(special_hedged_pos)));  
  plot(0,0,xlab="Time", ylab="HedgedPosition", xlim=c(0,max(t)),ylim=plot_ylim);
  cl<-rainbow(2);
  lines(t,regular_hedged_pos,col=cl[1],lty=1);
  lines(t,special_hedged_pos,col=cl[2],lty=2);
  #legend(.1*max(t),plot_ylim[1]*(0.86),c("hedged-pos (regular)","hedged-pos (special))"),col=cl, lty=c(1,2));
}

show_stock_opt <- function(path,option_prices,hedged_pos) {
  plot(0,0,xlab="Time", ylab="Prices" , xlim=c(0,max(path$t)),ylim=c(-max(path$values),max(path$values)));
  cl<-rainbow(3);
  lines(path$t,option_prices,col=cl[1],lty=1)
  lines(path$t,path$values,col=cl[2],lty=2);
  lines(path$t,hedged_pos,col=cl[3],lty=3);
  legend(1,-20,c("option","stock","hedged pos"),col=cl, lty=c(1,2,3));
}

show_barrier_opt_wrt_time <- function(t,callprice,calldelta,doutprice,doutdelta) {
  plot(0,0,xlab="Time", ylab="Prices" , xlim=c(0,max(t)),ylim=c( min(0,min(doutdelta)), max(doutdelta)))
  cl<-rainbow(4);
  lines(t,callprice/max(callprice),col=cl[1],lty=1)
  lines(t,calldelta,col=cl[2],lty=2);
  lines(t,doutprice/max(doutprice),col=cl[3],lty=3);
  lines(t,doutdelta,col=cl[4],lty=4);
  legend(.5,.2,c("call_normalized","call-delta","barrier_normalized","barrier-delta"),col=cl, lty=c(1,2,3,4));
}

show_barrier_opt_wrt_stock <- function(S_t,callprice,calldelta,doutprice,doutdelta) {
  plot(0,0,xlab="Underlying", ylab="Prices" , xlim=c(0,max(S_t)),ylim=c( min(0,min(calldelta)), max(calldelta)))
  cl<-rainbow(4);
  #lines(S_t,callprice/max(callprice),col=cl[1],lty=1)
  lines(S_t,calldelta,col=cl[2],lty=2);
  #lines(S_t,doutprice/max(doutprice),col=cl[3],lty=3);
  lines(S_t,doutdelta,col=cl[4],lty=4);
  #legend(S_t[1]/32,.6,c("call_normalized","call-delta","barrier_normalized","barrier-delta"),col=cl, lty=c(1,2,3,4));
}
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
