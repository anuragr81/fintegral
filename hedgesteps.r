
display_position <- function(isPrint,nShort,nShortedStocks,bsprice,pos)
{
  if (isPrint){
    print(paste("After: nShort(shorted)=",nShort,"nShortedStocks=",nShortedStocks))
    print(paste("hedged_pos=",pos));
    print(paste("price=",bsprice,"nshort=",nShort,"nShortedStocks=",nShortedStocks,"Position=",pos));
  }
}

hedged_position <- function (path_t,path_values,
                             tc,at,
                             checkArgs,pricerFunc,pricerArgs,
                             stepFunc,minTradesize,maxTradedelta) {
  isPrint<-FALSE;
  
  nh = length(path_values);
  option_prices=array();
  hedged_pos=array();
  nstocks=array();
  if (!checkArgs(pricerArgs)){
    stop("Improper Pricer Args.")
  }
  bs=pricerFunc(S_0=path_values,t=path_t,pricerArgs);
  nShortedStocks = bs$delta[1];
  nstocks[1]=nShortedStocks;
  hedged_pos[1] =  pnl_value(S_t=path_values[1],P_t=bs$price[1],nShorted=nShortedStocks,ntraded=0,at=at,tc=tc)
  
  if (isPrint){
    print(paste("hedged_pos[1]=",hedged_pos[1]));
  }
  
  for ( i in seq (2,nh) ) {
    
    dS= path_values[i] - path_values[i-1];
    dP= bs$price[i]-bs$price[i-1];
    
    if (isPrint){
      print(paste("dS=",dS,"dP=",dP,"nShortedStocks=",nShortedStocks))
    }
    
    nShort=stepFunc(tc=tc,
                    amivest=at,
                    underlying_price=path_values[i],
                    delta=bs$delta[i-1],
                    nxtdelta=bs$delta[i],
                    nShortedStocks=nShortedStocks,
                    dS=dS,
                    dP=dP);
    if (isPrint){
      print(paste("Before: nShort(to short)=",nShort,"nShortedStocks=",nShortedStocks))
    }
    
    zp_delta=bs$delta[i]-nShortedStocks; # nShortedStocks is the current effective delta (for all methods )
    
    if (abs(nShort)<minTradesize  ) {
      hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,ntraded=nShort,at=at,tc=tc)
      nstocks[i]=nShortedStocks;
      display_position(isPrint=isPrint,nShort=nShort,nShortedStocks=nShortedStocks,bsprice=bs$price[i],pos=hedged_pos[i]);
      next;
    } 
    
    if (zp_delta>maxTradedelta){
      # we short only maxTradeDelta
#      print("zp_delta>maxTradedelta")
      nShortedStocks=nShortedStocks+maxTradedelta;
      hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,ntraded=nShort,at=at,tc=tc);
      nstocks[i]=nShortedStocks;
      display_position(isPrint=isPrint,nShort=nShort,nShortedStocks=nShortedStocks,bsprice=bs$price[i],pos=hedged_pos[i]);
      next;
    } 
    
    if (zp_delta < -maxTradedelta){
      # we short only -maxTradedelta
#      print("zp_delta<maxTradedelta")
      nShortedStocks=nShortedStocks-maxTradedelta;
      hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,ntraded=nShort,at=at,tc=tc);
      nstocks[i]=nShortedStocks;
      display_position(isPrint=isPrint,nShort=nShort,nShortedStocks=nShortedStocks,bsprice=bs$price[i],pos=hedged_pos[i]);
      next;
    } 
    
    nShortedStocks = nShortedStocks + nShort;
    hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,ntraded=nShort,at=at,tc=tc);
    nstocks[i]=nShortedStocks;
    display_position(isPrint=isPrint,nShort=nShort,nShortedStocks=nShortedStocks,bsprice=bs$price[i],pos=hedged_pos[i]);
    
  } # end for
#  print(paste("len(hedged_pos)=",length(hedged_pos),"len(nstocks)=",length(nstocks),
#              "len(bsdelta)=",length(bs$delta),"len(bsprice)=",length(bs$price)))
  return(data.frame(hedged_pos=hedged_pos,nstocks=nstocks,t=path_t,deltas=bs$delta,bsprice=bs$price));
}

num_stocks_to_short_zerodp <- function(underlying_price,tc,delta,nxtdelta,nShortedStocks,dS,dP)
{
  # Check if x<0 or x>0 conditions apply. If neither works, return x=0 (don't hedge) and report error
  numerator=dP-nShortedStocks*dS
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


num_stocks_to_short_zerodp_g <- function(underlying_price,tc,amivest,delta,nxtdelta,nShortedStocks,dS,dP)
{
  # Check if x<0 or x>0 conditions apply. If neither works, return x=0 (don't hedge) and report error
  #print(paste("S_t=",underlying_price,"n=",nShortedStocks,"amivest=",amivest,"tc=",tc))
  if (amivest==0){
    return(num_stocks_to_short_zerodp(underlying_price=underlying_price,tc=tc,delta=delta,nShortedStocks=nShortedStocks,dS=dS,nxtdelta=nxtdelta,dP=dP));
  }
  
  A  = 2*amivest
  B1 = underlying_price-nShortedStocks*amivest+tc
  D1 = (underlying_price-nShortedStocks*amivest+tc)^2-4*amivest*(dP-nShortedStocks*dS); # x>0
  B2 = underlying_price-nShortedStocks*amivest-tc
  D2 = (underlying_price-nShortedStocks*amivest-tc)^2-4*amivest*(dP-nShortedStocks*dS); # x<0
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

num_stocks_to_short_deltas <- function(underlying_price,amivest,tc,delta,nxtdelta,nShortedStocks,dS,dP){
  return(nxtdelta-delta);
}

pnl_value <- function(S_t,P_t,nShorted,ntraded,at,tc) {
  return (P_t-nShorted*(S_t-ntraded*at)-abs(ntraded)*tc);
}
