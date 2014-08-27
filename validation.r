
#source ("file://c:/local_files/anurag/model_validation/validation.r")
#source("file://C:/Users/anuragr/Desktop/model_validation/validation.r")
# Reiner & Rubinstein Barrier Paper

source("bspricers.r")
source("barrierpricers.r")
source("hedgesteps.r")
source("display.r")

#
#S_0=100;
#vol=0.4;
#dt=.001;
#T=1;
#K=50;
#r_f=.05;
#tc=0;
#at=0;
#
# I checked that d\Pi = dP-\Delta dS grows as r\Pidt
# What I am not sure about is whether I need 
# to worry about volatility (variance) of d\Pi or \Pi
# The minimization of volatility of d\Pi (across space rather than 
# ) leads to delta hedging. volatility of d\Pi across time is
# path dependent and hence depends on S,P. Volatility of \Pi is 
# therefore the right quantity to optimize
# With delta-strategy we expect variance of d\Pi as near zero.
# Experimentally the mean sd of d\Pis (across time evolution) is 1e-3.
# variance across time vs space needs to be considered. 
# variance across space can be measured with time fixed. For S and with t=T,
# S would show log-normal distribution and hence it's variance is
# easy to verify. Mean is ~105 and mean-sd ~ 43. 
# The  option-prices also have the lognormal distribution. mean 
# is ~56 and mean-sd is ~43. Deltas (N(d_1)) don't have lognormal 
# distribution - mean is .95 and mean sd is .208.
# d1 is normally distributed ( ln (S/K) ) so N(d1) should be as well.
# Pi - mean is 47.5 - mean sd 10.6.

# The idea that I am now thinking is the deviation from deltas that would 
# affect the overall variance of \Pi.

test <- function(){
  S_0=100;
  vol=0.4;
  dt=.001;
  T=1;
  K=50;
  r_f=.05;
  tc=0;
  at=0;
  B=40;
  minsz=.000001;
  maxdelta=3;
  nh=T/dt;
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  pricer_func=bscallprice;
  pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
  check_args= checkargs_bscallpricer;
  out=array();
  par(mfrow=c(1,1));
  for (i in seq(1,10)){
    path = generate_path(S_0,r_f,vol,dt,T);
    bs=pricer_func(S_0=path$values,t=path$t,pricer_args);
    #dSs=diff(path$values);
    #dPs=diff(bs$price);
    #dPis=dPs-bs$delta[1:length(bs$delta)-1]*dSs;
    t=T-dt;
    S_t=path$values[length(path$values)];
    Pi=bs$price[length(bs$price)]-bs$delta[length(bs$delta)]*S_t;  
    numerator=log(S_t/K) + (r_f+(vol*vol)*0.5)*(T-t);
    denominator=vol*sqrt(T-t);
    d1=(numerator/denominator);
    d2=d1-vol*sqrt(T-t);
    out[i]=-Pi;
  }
  
  hist(out);
  print(mean(out));
  print(sd(out));
  
}

simul <- function(calculate,optionType){
  S_0=100;
  vol=0.4;
  dt=.01;
  T=1;
  K=50;
  r_f=.05;
  tc=0;
  at=0;
  B=40;
  minsz=.000001;
  maxdelta=3;
  nh=T/dt;
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  
  if(maxdelta<0){
    stop("maxTradedelta must be positive.");
  }
  
  if (optionType==1){
    pricer_func=bscallprice;
    pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
    check_args= checkargs_bscallpricer;
  }else{
    pricer_func=downandout_callprice;
    pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,B=B,tol=.001)
    check_args = checkargs_downandout_callpricer;
  }
  
  if (calculate==0){
    # could be replaced with an IR model
    path = generate_path(S_0,r_f,vol,dt,T);
    bs=pricer_func(S_0=path$values,t=path$t,pricer_args);
    
#    print("<<<<<<<<<<<<<<DELTA>>>>>>>>>>>>>>>>>")
    hp_deltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                   path_t=path$t,path_values=path$values,
                                   tc=0,at=at,
                                   pricerFunc=pricer_func,pricerArgs=pricer_args,
                                   minTradesize=minsz,maxTradedelta=maxdelta);
#    print(data.frame(bsprice=hp_deltamethod$bsprice,
#                     bsdeltas=hp_deltamethod$deltas));
    print(exp(r_f*T)*
            (hp_deltamethod$bsprice[1]-hp_deltamethod$deltas[1]*path$values[1])
    )
#    print("<<<<<<<<<<<<<<ZERODP>>>>>>>>>>>>>>>>>")
    
    hp_zerodpmethod=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                                    path_t=path$t,path_values=path$values,
                                    tc=0,at=at,
                                    pricerFunc=pricer_func,pricerArgs=pricer_args,
                                    minTradesize=minsz,maxTradedelta=maxdelta);
    if(TRUE){
      par(mfrow=c(2,2));
      
      plot(path$t,xlab="Time",ylab='Stock Price',path$values,type='l')
      plot(path$t,xlab="Time",ylab='Delta of Call',hp_deltamethod$deltas,type='l')
      
      #      show_comparison (t=hp_deltamethod$t[1:length(hp_deltamethod$t)-1],
      #                   regular_hedged_pos=diff(hp_zerodpmethod$nstocks),
      #                   special_hedged_pos=diff(hp_deltamethod$nstocks),
      #                   tagname="Change in ShortedStocks");
      show_comparison (t=hp_deltamethod$t,
                       regular=(hp_zerodpmethod$nstocks),
                       special=(hp_deltamethod$nstocks),
                       tagname="ShortedStocks");
      
      show_comparison (t=hp_deltamethod$t,
                       regular=hp_zerodpmethod$hedged_pos,
                       special=hp_deltamethod$hedged_pos,
                       tagname="Net PNL");
      
    }
  } else {
    sd_dmk=array();
    mean_dmk=array();
    sd_zpk=array();
    mean_zpk=array();
    
    for ( k in seq(2000)){
      
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
    #sink("file://C:/Users/anuragr/Desktop/model_validation/output.txt");
    #cat(out);
    #sink();
    #print(out);
    par(mfrow=c(1,2));
    
    hist(mean_dmk)
    hist(mean_zpk)
  }
}

testStockGamma <- function(K) {
  ds=.1;
  finS=3*K;
  veclen=as.integer(finS/ds);
  S_0=seq(0,finS,ds); S_0=S_0[1:length(S_0)-1];
  K=rep(K,veclen);
  #r_f=seq(0,.05,(.05/veclen)); r_f=r_f[1:(length(r_f)-1)];
  r_f=rep(.05,veclen);
  vol=rep(.6,veclen);
  t=rep(0,veclen);
  T=rep(1,veclen);
  v=bscallprice(S_0,K,r_f,vol,t,T);
  plot(S_0,v$gamma); #,S_0,v$delta,S_0,v$gamma);
}


testBarrier<- function(){
  S_0=100;
  K=80;
  B=89;
  r_f=.05;
  vol=.3;
  T=1;
  dt=.01;
  nh=T/dt
  #t=seq(0:(nh-1))*dt
  t=rep(0,nh)
  path = generate_path(S_0,r_f,vol,dt,T);
  #S_t=path$values
  S_t=2*(S_0)*seq(1:nh)/nh
  
  cb=value_downandout_exp(S_0=S_t,K=rep(K,nh),B=rep(B,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=t,T=rep(T,nh));
  
  c=bscallprice(S_0=S_t,K=K,r_f=r_f,vol=vol,t=t,T);
  dS=.001
  cbn=value_downandout_exp(S_0=S_t+dS,K=rep(K,nh),B=rep(B,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=t,T=rep(T,nh));
  delta=(cbn-cb)/dS
  show_barrier_opt_wrt_stock(S_t,c$price,c$delta,doutprice=cb,doutdelta=delta)
  
}

testBSStock <- function() {
  S_0=100;
  r_f=.1;
  vol=.5;
  dt=.01;
  T=3;
  K=50;
  path = generate_path(S_0,r_f,vol,dt,T);
  nh=T/dt;
  start=0;
  end=2;
  df=(end-start)/nh;
  vec_vol=rep(.4,nh);#seq(start,end-df,df)
  S_t=2*S_0*(1+seq(1:nh))/nh;
  pricer_args = data.frame(r_f=r_f,vol=vec_vol,dt=dt,T=T,K=K,B=0,tol=.001)
  bs = bscallprice(pricerArgs=pricer_args,S_0=S_t,t=path$t);
  
  plot(0,0,xlab="Stock", ylab="", xlim=c(0,max(S_t)),ylim=c(-max(bs$price/K,1),max(bs$price/K,1)));
  cl<-rainbow(2);
  lines(S_t,bs$delta,col=cl[1],lty=1)
  lines(S_t,bs$price/K,col=cl[2],lty=2);
  legend(1,-0.5,c(expression("delta"),expression("price/strike")),col=cl, lty=c(1,2));
  
}
testBS <- function(){
  # observations: 
  # in the money option-deltas come close to unity as time to maturity approaches (and stock-price remains same). This is because the payoff from stock is the same as the option (weight unity in the tracking portfolio).
  # out of the money option-deltas come close to zero as time to maturity approaches (and stock-price remains constant). This is because owning a stock (delta) pays nothing. 
  
  S_0=100
  r_f=.1
  vol=.5
  dt=.01
  T=3
  K=110
  path = generate_path(S_0,r_f,vol,dt,T);
  nh=T/dt
  start=0
  end=2
  df=(end-start)/nh
  volvec=seq(start,end-df,df)
  bs = bscallprice(S_0=rep(S_0,nh),K=rep(K,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=path$t,T=rep(T,nh));
  plot(0,0,xlab="Time", ylab="", xlim=c(0,max(path$t)),ylim=c(-max(bs$price/K,1),max(bs$price/K,1)));
  cl<-rainbow(2);
  lines(path$t,bs$delta,col=cl[1],lty=1)
  lines(path$t,bs$price/K,col=cl[2],lty=2);
  legend(1,-0.5,c(expression("delta"),expression("price/strike")),col=cl, lty=c(1,2)); 
}
