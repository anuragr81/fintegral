
#source ("file://c:/local_files/anurag/model_validation/validation.r")
#source("file://C:/Users/anuragr/Desktop/model_validation/validation.r")
# Reiner & Rubinstein Barrier Paper

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

#========== BARRIER PRICER ============

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

num_stocks_to_short <- function(underlying_price,tc,delta,nShortedStocks,dS){
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

num_stocks_to_short_direct <- function(underlying_price,dP,nShortedStocks,dS){
    return ((1/underlying_price)*(dP-nShortedStocks*dS));
}

show_deltas <- function(t,notc_deltas,notc_hedged_pos,tc_deltas,tc_hedged_pos) {
    plot(0,0,xlab="Time", ylab="Delta, HedgedPosition(normalized)", xlim=c(0,max(t)),ylim=c(-2,3));
    cl<-rainbow(4);
    lines(t,notc_deltas,col=cl[1],lty=1)
    lines(t,notc_hedged_pos/max(abs(notc_hedged_pos)),col=cl[2],lty=2);
    lines(t,tc_deltas,col=cl[3],lty=3)
    lines(t,tc_hedged_pos/max(abs(tc_hedged_pos)),col=cl[4],lty=4);
    legend(1,3,c("delta(noTC)","hedged pos (normalized-noTC)","delta(TC)","hedged pos(normalized-TC)"),col=cl, lty=c(1,2,3,4));
}

show_stock_opt <- function(path,option_prices,hedged_pos) {
    plot(0,0,xlab="Time", ylab="Prices" , xlim=c(0,max(path$t)),ylim=c(-max(path$values),max(path$values)));
    cl<-rainbow(3);
    lines(path$t,option_prices,col=cl[1],lty=1)
    lines(path$t,path$values,col=cl[2],lty=2);
    lines(path$t,hedged_pos,col=cl[3],lty=3);
    legend(1,-20,c("option","stock","hedged pos"),col=cl, lty=c(1,2,3));
}


pnl_value <- function(S_t,P_t,nShorted,tc) {
   return (P_t-nShorted*S_t-abs(nShorted)*tc);
}



hedged_position <- function (path_t,path_values,r_f,vol,dt,T,K,tc) {
    nh = length(path_values);
    option_prices=array();
    hedged_pos=array();
    bs=bscallprice(S_0=path_values,K=K,r_f=r_f,vol=vol,t=path_t,T);

    nShortedStocks = bs$delta[1];
    hedged_pos[1] =  pnl_value(S_t=path_values[1],P_t=bs$price[1],nShorted=nShortedStocks,tc=tc)

    for ( i in seq (2,nh) ) {
           dS= path_values[i] - path_values[i-1];
           nShort=num_stocks_to_short(tc=tc,underlying_price=path_values[i],delta=bs$delta[i-1],nShortedStocks=nShortedStocks,dS=dS);
           nShortedStocks = nShortedStocks + nShort;
           hedged_pos[i] = pnl_value(S_t=path_values[i],P_t=bs$price[i],nShorted=nShortedStocks,tc=tc);
           #print(paste("price=",bs$price,"nshort=",nShort,"nShortedStocks=",nShortedStocks,"Position=",hedged_pos[i]));
    }
    return(data.frame(hedged_pos=hedged_pos,t=path_t,deltas=bs$delta));
}

simul <- function(calculate){
    S_0=50;
    vol=.4;
    dt=.1;
    T=10;
    K=50;
    r_f=.05;
    tc=3;


    nh=T/dt;
    vec_r_f=rep(r_f,nh)
    vec_vol=rep(vol,nh)
    out=array();
    mean_k=array();

    if (calculate==0){
    # could be replaced with an IR model

        path = generate_path(S_0,r_f,vol,dt,T);
        hp_notc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=0);
        hp_tc=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc);

        show_deltas (t=hp_notc$t,notc_deltas=hp_notc$deltas,notc_hedged_pos=hp_notc$hedged_pos,tc_deltas=hp_tc$deltas,tc_hedged_pos=hp_tc$hedged_pos)
    } else {
        for ( k in seq(500)){
            path = generate_path(S_0,r_f,vol,dt,T);
            pos_k=hedged_position(path_t=path$t,path_values=path$values,r_f=vec_r_f,vol=vec_vol,dt=dt,T=T,K=K,tc=tc);
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

value_downandout_exp <- function(S_0,K,B,r_f,vol,t,T) {
cbs=bscallprice(S_0=S_0,K=K,r_f=r_f,vol=vol,t=t,T);
cbsi=bscallprice(S_0=(B*B/S_0),K=K,r_f=r_f,vol=vol,t=t,T=T);
n1=cbs$price
n2=((S_0/B)^(1-(2*r_f/(vol*vol)))*cbsi$price)
return(n1-n2);
#ts.plot(S_0,ts((S_0[1]^2)/S_0)) # <-- beauty

}

delta_downandout_exp <-function(S_0,K,B,r_f,vol,dt,T){
   cbs=value_downandout_exp(S_0=S_0,K=K,B=B,r_f=r_f,vol=vol,dt=dt,T=T)
   dS=.001;
   cbsnext=value_downandout_exp(S_0=S_0+dS,K=K,B=B,r_f=r_f,vol=vol,dt=dt,T=T)
   return ((cbsnext$price-cbs$price)/dS)
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
