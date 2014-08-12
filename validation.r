
#source ("file://c:/local_files/anurag/model_validation/validation.r")
#source("file://C:/Users/anuragr/Desktop/model_validation/validation.r")
# Reiner & Rubinstein Barrier Paper

source("bspricers.r")
source("barrierpricers.r")
source("hedgesteps.r")
source("display.r")

simul <- function(calculate,optionType){
  S_0=100;
  vol=.4;
  dt=.01;
  T=1;
  K=50;
  r_f=.05;
  tc=1;
  at=0;
  B=40;
  minsz=.000001;
  mindelta=3;
  nh=T/dt;
  vec_r_f=rep(r_f,nh)
  vec_vol=rep(vol,nh)
  out=array();
  mean_k=array();
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
    
    print("<<<<<<<<<<<<<<DELTA>>>>>>>>>>>>>>>>>")
    hp_deltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                       path_t=path$t,path_values=path$values,
                                       tc=0,at=at,
                                       pricerFunc=pricer_func,pricerArgs=pricer_args,
                                       minTradesize=minsz,maxTradedelta=mindelta);
    print("<<<<<<<<<<<<<<ZERODP>>>>>>>>>>>>>>>>>")
    
    hp_zerodpmethod=hedged_position(stepFunc=num_stocks_to_short_zerodp_g,checkArgs=check_args,
                            path_t=path$t,path_values=path$values,
                            tc=0,at=at,
                            pricerFunc=pricer_func,pricerArgs=pricer_args,
                            minTradesize=minsz,maxTradedelta=mindelta);
    if(TRUE){
      
    show_deltas (t=hp_deltamethod$t,
                 path_values=path$values,
                 deltas=hp_deltamethod$deltas,
                 regular_hedged_pos=hp_zerodpmethod$hedged_pos,
                 special_hedged_pos=hp_deltamethod$hedged_pos);
    }
  } else {
    for ( k in seq(500)){
      path = generate_path(S_0,r_f,vol,dt,T);
      hp_notcdeltamethod=hedged_position(stepFunc=num_stocks_to_short_deltas,checkArgs=check_args,
                                         path_t=path$t,path_values=path$values,
                                         tc=0,at=at,
                                         pricerFunc=pricer_func,pricerArgs=pricer_args,
                                         minTradesize=minsz,maxTradedelta=mindelta);
      
      #pos_k=hedged_position(path_t=path$t,path_values=path$values,tc=tc,at=at,checkArgs=check_args,pricerFunc=pricer_func,pricerArgs=pricer_args);
      
      out[k]=sd(pos_k$hedged_pos);
      mean_k[k]=mean(pos_k$hedged_pos);
    }
    #sink("file://C:/Users/anuragr/Desktop/model_validation/output.txt");
    #cat(out);
    #sink();
    #print(out);
    print(paste("Average PNL:",mean(mean_k)))
    #hist(out); 
    print(paste("mean-stdev(PNL):",mean(out)));
  }
}

test <- function(K) {
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
  S_0=100
  r_f=.1
  vol=.5
  dt=.01
  T=3
  K=70
  path = generate_path(S_0,r_f,vol,dt,T);
  nh=T/dt
  start=0
  end=2
  df=(end-start)/nh
  volvec=seq(start,end-df,df)
  S_t=2*S_0*(1+seq(1:nh))/nh;
  bs = bscallprice(S_0=S_t,K=rep(K,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=rep(0,nh),T=rep(T,nh));
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
