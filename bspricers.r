generate_path <- function (npaths,S_0,r_f,vol,dt,T){
  t=0
  n=T/dt;
  z=rnorm(n-1);
  tarray=seq(0,n-1)*dt
  vec=cumsum((r_f-vol*vol/2)*dt +vol*z*sqrt(dt))
  values=S_0*exp(vec)
  values=c(S_0,values) # appending initial value
  return (data.frame(values=values,t=tarray));
}

value_bscall_func <- function(S,K,B){
  # if S_T is above K, then payoff is S_T-K
  return(max(S[length(S)]-K,0));
}

value_bscall <- function (npaths,S_0,K,r_f,vol,dt,T) {
  fname="paths.txt"
  if (file.exists(fname)){
    rpaths=load(fname);
    print(rpaths);
  } else {
    paths=array();
    rfile=file("paths.txt","w");
    for ( i in seq(npaths)) {
      paths[i]=generate_path(S_0=S_0,r_f=r_f,vol=vol,dt=dt,T=T);
    }
    print(paths)
    save(file=fname,paths);
    close(rfile);
  }
  return(0)
  res=array(0);
  out=array(0);
  
  for ( i in seq(npaths)) {
    paths=generate_path(S_0=S_0,r_f=r_f,vol=vol,dt=dt,T=T)
    out[i]=value_bscall_func(S=paths$values,K=K,B=B);
  }
  
  res=(sum(out)/length(out))*exp(-r_f*T);
  sg_c=sd(out);
  
  values_left=res-1.96*sg_c/sqrt(npaths);
  values_right=res+1.96*sg_c/sqrt(npaths);
  
  return (data.frame(left=values_left,right=values_right));
  
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

call_bscallpricer <- function (S_0,t,vec_r_f,vec_vol,dt,T,K){
  pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
  return(bscallprice(S_0=path$values,t=path$t,pricer_args));
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
