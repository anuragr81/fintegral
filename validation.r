
#source ("file://c:/local_files/anurag/model_validation/validation.r")
#source("file://C:/Users/anuragr/Desktop/model_validation/validation.r")
# Reiner & Rubinstein Barrier Paper

source("bspricers.r")
source("hedgesteps.r")
source("display.r")

hedged_position <- function (path_t,path_values,r_f,vol,dt,T,K,tc,at,pricerFunc) {
  isPrint=FALSE;
  nh = length(path_values);
  option_prices=array();
  hedged_pos=array();
  bs=pricerFunc(S_0=path_values,K=K,r_f=r_f,vol=vol,t=path_t,T);
  
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

simul <- function(calculate){
    S_0=50;
    vol=.4;
    dt=.01;
    T=1;
    K=50;
    r_f=.05;
    tc=.2;
    at=1;

    nh=T/dt;
    vec_r_f=rep(r_f,nh)
    vec_vol=rep(vol,nh)
    out=array();
    mean_k=array();

    if (calculate==0){
    # could be replaced with an IR model
        path = generate_path(S_0,r_f,vol,dt,T);
        hp_notc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=0,at=at,pricerFunc=bscallprice);
        hp_tc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc,at=at,pricerFunc=bscallprice);

        show_deltas (t=hp_notc$t,notc_deltas=hp_notc$deltas,notc_hedged_pos=hp_notc$hedged_pos,tc_deltas=hp_tc$deltas,tc_hedged_pos=hp_tc$hedged_pos)
    } else {
        for ( k in seq(500)){
            path = generate_path(S_0,r_f,vol,dt,T);
            pos_k=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc,at=at,pricerFunc=bscallprice);
            out[k]=sd(pos_k$hedged_pos)
	    mean_k[k]=mean(pos_k$hedged_pos)
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
