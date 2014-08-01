library(shiny)
library(ggplot2)

##########################################

num_stocks_to_short_zerodp <- function(underlying_price,tc,delta,nShortedStocks,dS){
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

num_stocks_to_short_zerodp_g <- function(underlying_price,tc,amivest,delta,nShortedStocks,dS){
  # Check if x<0 or x>0 conditions apply. If neither works, return x=0 (don't hedge) and report error
  #print(paste("S_t=",underlying_price,"n=",nShortedStocks,"amivest=",amivest,"tc=",tc))
  if (amivest==0){
    return(num_stocks_to_short_zerodp(underlying_price=underlying_price,tc=tc,delta=delta,nShortedStocks=nShortedStocks,dS=dS));
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
  xO1=(B2+sqrt(D2))/A
  xO2=(B2-sqrt(D2))/A
  xT1=(B1+sqrt(D1))/A
  xT2=(B1-sqrt(D1))/A
  #print(paste("xO1=",xO1,"xO2=",xO2,"xT1=",xT1,"xT2=",xT2))
  sols=c(xO1,xO2,xT1,xT2)
  positives=sols[sols>=0]
  negatives=sols[sols<=0]
  if (length(positives)==0) {
    return (max(negatives));
  } 
  if (length(negatives)==0){
    return(min(positives))
  }
  if (min(positives) < min(abs(negatives)))
  { 
    return(min(positives))
  }
  else {
    return(max(negatives))
  }
  
}

num_stocks_to_short_deltas <- function(underlying_price,tc,delta,nxtdelta,nShortedStocks,dS){
  return(nxtdelta-delta);
}

num_stocks_to_short_direct <- function(underlying_price,dP,nShortedStocks,dS){
  return ((1/underlying_price)*(dP-nShortedStocks*dS));
}

pnl_value <- function(S_t,P_t,nShorted,at,tc) {
  return (P_t-nShorted*(S_t-nShorted*at)-abs(nShorted)*tc);
}


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

bscallprice <- function(S_0,K,r_f,vol,t,T){
  numerator=log(S_0/K) + (r_f+(vol*vol)*0.5)*(T-t);
  denominator=vol*sqrt(T-t);
  d1=(numerator/denominator);
  d2=d1-vol*sqrt(T-t);
  res=list();
  price=(S_0*pnorm(d1)-K*exp(-r_f*(T-t))*pnorm(d2));
  delta=pnorm(d1);
  gamma=dnorm(d1)/(S_0*vol*sqrt(T-t));
  return (data.frame(price=price,delta=delta,gamma=gamma));
}






hedged_position <- function (path_t,path_values,r_f,vol,dt,T,K,tc,at) {
  isPrint=FALSE;
  nh = length(path_values);
  option_prices=array();
  hedged_pos=array();
  bs=bscallprice(S_0=path_values,K=K,r_f=r_f,vol=vol,t=path_t,T);
  
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
    nShort=num_stocks_to_short_zerodp_g(tc=tc,amivest=at,underlying_price=path_values[i],delta=bs$delta[i-1],nShortedStocks=nShortedStocks,dS=dS);
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
  }
  return(data.frame(hedged_pos=hedged_pos,t=path_t,deltas=bs$delta));
}


show_deltas <- function(t,notc_deltas,notc_hedged_pos,tc_deltas,tc_hedged_pos) {
  plot(0,0,xlab="Time", ylab="Delta, HedgedPosition(normalized)", xlim=c(0,max(t)),ylim=c(-2,3));
  cl<-rainbow(3);
  lines(t,notc_deltas,col=cl[1],lty=1)
  lines(t,notc_hedged_pos/max(abs(notc_hedged_pos)),col=cl[2],lty=2);
  lines(t,tc_hedged_pos/max(abs(tc_hedged_pos)),col=cl[3],lty=3);
  legend(1,3,c("delta","hedged pos (normalized-noTC)","hedged pos(normalized-TC)"),col=cl, lty=c(1,2,3));
}


##########################################

display<- function(S_0,K,r_f,vol,at,tc,dt,T,rerun,calculate,option_type){
  if (option_type==0){
    
  
  if (rerun>=0){
  nh=T/dt;
  vec_r_f=rep(r_f,nh);
  vec_vol=rep(vol,nh);
  
  if (calculate==0){
  path = generate_path(S_0,r_f,vol,dt,T);
  hp_notc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=0,at=at);
  hp_tc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc,at=at);
  par(mfrow=c(1,2));
  plot(path$t,xlab="Time",path$values,ylab="Underlying Price",type='l');
  show_deltas (t=hp_notc$t,notc_deltas=hp_notc$deltas,notc_hedged_pos=hp_notc$hedged_pos,tc_deltas=hp_tc$deltas,tc_hedged_pos=hp_tc$hedged_pos)
  } else{
    
    stdev=array();
    mean_k=array();
    
    for ( k in seq(500)){
      path = generate_path(S_0,r_f,vol,dt,T);
      pos_k=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc,at=at);
      stdev[k]=sd(pos_k$hedged_pos)
      mean_k[k]=mean(pos_k$hedged_pos)
    }
    par(mfrow=c(1,2));
    hist(stdev);
    hist(mean_k);
  }
  }
  
}

}


shinyServer(
  function(input, output) {
    
    output$data  <- renderPlot(
      expr=display(S_0=input$S_0,
                   K=input$K,
                   r_f=input$r_f,
                   vol=input$vol,
                   dt=input$dt,
                   at=input$at,
                   tc=input$tc,
                   T=input$T,
                   rerun=input$run,
                   calculate=input$calculate,
                   option_type=input$option_type
      ))
    #output$path <-renderPlot()
    
  }
)
