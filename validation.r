
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

test01 <- function(){
  S_0=100;
  r_f=.05;
  vol=0.4;
  dt=.001;
  T=1;
  
  N<-500000
  path_k=array();
  
  for ( k in seq(N)){
    path = generate_path(S_0,r_f,vol,dt,T);  
    path_k[k]=(path$values[length(path$values)])
  }
  
  hist(path_k,main="Final Stock Price Values")
  
  print(paste("mean S_T=",mean(path_k)))
  print(paste("var(log(S_T))=",var(log(path_k))))
  
  
  print(paste("theoretical mean S_T=",S_0*exp(r_f*T)));
  print(paste("theoretical var(log(S_T))=",vol*vol*T));
}

test02 <- function (){
  
  S_0=100;
  r_f=.05;
  vol=0.4;
  dt=.05;
  K=100;
  T=1;
  
  N<-500000
  
  callpayoff_k=array();
  putpayoff_k=array();
  ncallpayoff_k=array();
  nputpayoff_k=array();
  dS=.001;
  
  for ( k in seq(N)){
    path = generate_path(S_0,r_f,vol,dt,T);
    callpayoff_k[k]=max(path$values[length(path$values)]-K,0);  
    putpayoff_k[k]=max(K-path$values[length(path$values)],0);    
  }
  
  #for ( k in seq(N)){
  #  npath = generate_path(S_0+dS,r_f,vol,dt,T);
  #  ncallpayoff_k[k]=max(npath$values[length(npath$values)]-K,0);  
  #  nputpayoff_k[k]=max(K-npath$values[length(npath$values)],0);
  #}
  
  par(mfrow=c(1,2));
  
  hist(callpayoff_k,main="CallOption PayOff")
  hist(putpayoff_k,main="PutOption PayOff")
  
  print(paste("sd(callpayoff_k)=",sd(callpayoff_k)))
  
  print(paste("Monte-Carlo call-option price =",mean(callpayoff_k)*exp(-r_f*T)));
  #print(paste("Monte-Carlo ncall-option price =",mean(ncallpayoff_k)*exp(-r_f*T)));
  
  #print(paste("Monte-Carlo call-option delta =",(((mean(ncallpayoff_k)-mean(callpayoff_k))/dS)*exp(-r_f*T))));  
  
  print(paste("Monte-Carlo put-option price =",mean(putpayoff_k)*exp(-r_f*T)));
  #print(paste("Monte-Carlo nput-option price =",mean(nputpayoff_k)*exp(-r_f*T)));
  #print(paste("Monte-Carlo put-option delta =",(((mean(nputpayoff_k)-mean(putpayoff_k))/dS)*exp(-r_f*T))));
  
  nh=T/dt;
  
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  
  pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
  bs=bscallprice(S_0=path$values,t=path$t,pricer_args);
  
  print(paste("theoretical BS CallOption price =",bs$price[1]));  
  #print(paste("theoretical BS CallOption delta =",bs$delta[1]));  
  
  print(paste("theoretical BS PutOption price =",bs$price[1]+K*exp(-r_f*T)-path$values[1]));
  #print(paste("theoretical BS PutOption delta =",bs$delta[1]-1));
}

test03 <- function(){
  
  S_0=100;
  vol=0.4;
  dt=.01;
  T=1;
  K=100;
  r_f=.05;
  
  nh=T/dt;
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  
  #path = generate_path(S_0,r_f,vol,dt,T);
  B_values=S_0*(seq(0.01,1.2,.01))
  k=1;
  do_k=array();
  bs_k=array();
  for (b in B_values){
    pricer_args = data.frame(r_f=r_f,vol=vol,dt=dt,T=T,K=K,B=b,tol=.001,m=nh)
    doprice=downandout_callprice(S_0=S_0,t=0,pricer_args);
    do_k[k]=((doprice$price))
    bs=bscallprice(S_0=S_0,t=0,pricer_args);
    bs_k[k]=bs$price;
    k=k+1
  }
  par(mfrow=c(1,1));
  
  plot_ylim=c(min(min(do_k),min(bs_k)),max(max(bs_k),max(do_k)))
  
  plot(0,0,main=paste("Derivatives Price(Strike=",K,")"),xlab="Barrier", ylab="Derivatives Price", 
       xlim=c(0,max(B_values)),ylim=plot_ylim);
  
  cl<-rainbow(2);
  lines(B_values,do_k,col=cl[1],lty=1)
  lines(B_values,bs_k,col=cl[2],lty=2) 
  legend(B_values[1],.3*mean(plot_ylim),c(expression("Down-and-out"),expression("Plain-Vanilla Call")),col=cl, lty=c(1,2));
  
  filename=("file://c:/local_files/anurag/model_validation/liquidity_task/PvsB.csv");
  dat=(data.frame(B=B_values,P=do_k))
  write.csv(dat,file=filename);
  
}

simul <- function(calculate,optionType,vol,at,tc,npaths,dt,minsz){
  
  npaths=npaths;
  S_0=100;
  #vol=0.4;
  vol=vol;
  dt=dt;
  T=1;
  K=100;
  r_f=.05;
  tc=tc;
  at=at;
  B=99;
#  minsz=.0000001;
  minsz=minsz;
  maxdelta=100
  nh=T/dt;
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  #  vec_vol=vol*(1+3*(seq(0,1-dt,dt)))
  print(vec_vol)
  
  if(maxdelta<0){
    stop("maxTradedelta must be positive.");
  }
  
  if (optionType==1){
    pricer_func=bscallprice;
    pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K);
    check_args= checkargs_bscallpricer;
  }else{
    pricer_func=downandout_callprice;
    pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,B=B,tol=.001,m=nh)
    check_args = checkargs_downandout_callpricer;
  }
  
  if (calculate==0){
    # could be replaced with an IR model
    path = generate_path(S_0,r_f,vec_vol[1:length(vec_vol)-1],dt,T);
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
      par(mfrow=c(3,2));
      
      plot(path$t,main="Stock Price", xlab="Time",ylab='Stock Price',path$values,type='l')
      plot(path$t,main="Derivative Price", xlab="Time",ylab='Derivative Price',bs$price,type='l')
      
      plot(path$t,main="Delta", xlab="Time",ylab='Delta',hp_deltamethod$deltas,type='l')
      
      #      show_comparison (t=hp_deltamethod$t[1:length(hp_deltamethod$t)-1],
      #                   regular_hedged_pos=diff(hp_zerodpmethod$nstocks),
      #                   special_hedged_pos=diff(hp_deltamethod$nstocks),
      #                   tagname="Change in ShortedStocks");
      
      show_comparison (t=hp_deltamethod$t,
                       regular=(hp_zerodpmethod$nstocks),
                       special=(hp_deltamethod$nstocks),
                       tagname="Number of Stocks Shorted (n)");
      #print(paste("var(hedged_error)=",var(hp_zerodpmethod$hedged_pos-hp_zerodpmethod$hedged_pos[1])))
      show_comparison (t=hp_deltamethod$t,
                       regular=hp_zerodpmethod$hedged_pos,
                       special=hp_deltamethod$hedged_pos,
                       tagname="Hedge Portfolio Value (P-nS)");
      
    }
  } else {
    
    sd_dmk=array();
    mean_dmk=array();
    sd_zpk=array();
    mean_zpk=array();
    
    for ( k in seq(npaths)){
      
      path = generate_path(S_0,r_f,vec_vol[1:length(vec_vol)-1],dt,T);
      
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
      
      sd_dmk[k]=sd(hp_deltamethod$hedged_pos-hp_deltamethod$hedged_pos[1]);
      #mean_dmk[k]=mean(hp_deltamethod$hedged_pos);
      mean_dmk[k]=(hp_deltamethod$hedged_pos[length(hp_deltamethod$hedged_pos)]);
      
      sd_zpk[k]=sd(hp_zerodpmethod$hedged_pos-hp_zerodpmethod$hedged_pos[1]);
      mean_zpk[k]=mean(hp_zerodpmethod$hedged_pos);
      
    }
    
    #sink("file://C:/Users/anuragr/Desktop/model_validation/output.txt");
    #cat(out);
    #sink();
    #print(out);
    par(mfrow=c(2,2));
    #    print(paste("Mean-dmk:",mean(mean_dmk)))
    hist(mean_dmk,main="Average Position (Delta-Method)")
    hist(mean_zpk,main="Average Position (Alternate-Method)")
    hist(sd_dmk,main="Position Stdev (Delta-Method)")
    hist(sd_zpk,main="Position Stdev (Alternate-Method)")
    return(data.frame(vol=vol,at=at,tc=tc,mean_dmk=mean(mean_dmk),mean_zpk=mean(mean_zpk),sd_zpk=mean(sd_zpk),sd_dmk=mean(sd_dmk)))
  }
}

testStockGamma <- function(K) {
  dt=.01;
  ds=.1;
  vol=.4;
  finS=K;
  veclen=as.integer(finS/ds)-1;
  
  #S_0=seq(0,finS,ds); S_0=S_0[1:length(S_0)-1];
  S_0=rep(finS,finS/ds); S_0=S_0[1:length(S_0)-1];
  
  K=rep(K,veclen);
  #vec_vol=rep(vol,veclen)
  vec_vol=seq(1,veclen)*vol/14;
  print(vec_vol)
  r_f=.05;
  vec_r_f=rep(r_f,veclen);
  #vec_r_f=seq(1,veclen)*r_f/2;
  #r_f=seq(0,.05,(.05/veclen)); 
  #r_f=r_f[1:(length(r_f)-1)];
  #  path = generate_path(S_0,r_f,vec_vol[1:length(vec_vol)-1],dt,T);
  
  pricer_args = data.frame(r_f=vec_r_f,vol=vec_vol,T=T,K=K);
  if (checkargs_bscallpricer(pricer_args)){
    bs=bscallprice(S_0=S_0,t=0,pricer_args); 
  }
  par(mfrow=c(2,2));
  plot(xlab="vol",ylab="CallPrice",vec_vol,bs$price,type='l')
  plot(xlab="vol",ylab="CallDelta",vec_vol,bs$delta,type='l')
  plot(xlab="vol",ylab="CallGamma",vec_vol,bs$gamma,type='l')
  
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

  path = generate_path(npaths=1,S_0=S_0,r_f=r_f,vol=vol,dt=dt,T=T);
  print(T)
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
  dt=.01;
  T=3;
  K=110;
  path = generate_path(S_0,r_f,vol,dt,T);
  nh=T/dt;
  start=0;
  end=2;
  df=(end-start)/nh;
  volvec=seq(start,end-df,df);
  bs = bscallprice(S_0=rep(S_0,nh),K=rep(K,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=path$t,T=rep(T,nh));
  plot(0,0,xlab="Time", ylab="", xlim=c(0,max(path$t)),ylim=c(-max(bs$price/K,1),max(bs$price/K,1)));
  cl<-rainbow(2);
  lines(path$t,bs$delta,col=cl[1],lty=1);
  lines(path$t,bs$price/K,col=cl[2],lty=2);
  legend(1,-0.5,c(expression("delta"),expression("price/strike")),col=cl, lty=c(1,2)); 
}
