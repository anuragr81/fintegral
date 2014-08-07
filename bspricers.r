
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
