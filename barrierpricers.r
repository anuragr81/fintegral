
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
  m=pricerArgs$m;
  
  cbs=bscallprice(S_0=S_0,t=t,pricerArgs=pricerArgs);
  cbsi=bscallprice(S_0=(B*B/S_0),t=t,pricerArgs=pricerArgs);
  n1=cbs$price;
  n2=((S_0/B)^(1-(2*r_f/(vol*vol)))*cbsi$price)
  nr=(n1-n2);
  zeroed=(nr*((sign(nr)+1)/2));
  #continuity_factor=exp(sign(B-S_0)*vol*.5826*sqrt(T/m));
  continuity_factor=1;
  return(zeroed*continuity_factor);  
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
  
  if (min(pricerArgs$m)<0){
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
