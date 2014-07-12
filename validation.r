
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
    numerator=log(S_0/K) + (r_f+(vol^2)*0.5)*(T-t);
    denominator=vol*sqrt(T-t);
    d1=(numerator/denominator);
    d2=d1-vol*sqrt(T-t);
    res=list();
    price=(S_0*pnorm(d1)-K*exp(-r_f*(T-t))*pnorm(d2));
    delta=pnorm(d1);
    gamma=dnorm(d1)/(S_0*vol*sqrt(T-t));
    return (data.frame(price=price,delta=delta,gamma=gamma));
}

generate_path <- function (S_0,mu,vol,dt,T) {
    if (dt==0) {
        print ("Error in input: dt");
    }
    n=T/dt;
    t=0;
    tarray=array();
    values=array();
    values[1]=S_0;
    tarray[1]=t;
    t=t+dt;
    for ( i in seq(2,n)){
        dS = values[i-1]*(mu*dt+vol*rnorm(1)*sqrt(dt));
        values[i]=values[i-1]+dS;
        tarray[i]=t;
        t=t+dt;
    }
    return (data.frame(values=values,t=tarray));
}

num_stocks_to_short <- function(underlying_price,delta,nxtdelta,gamma,dgamma,nShortedStocks,dS){
#    return ((1/underlying_price)*((delta-nShortedStocks)*dS+gamma*(dS^2)/2+dgamma*(dS^3)/6));
#    return ((1/underlying_price)*((delta-nShortedStocks)*dS+gamma*(dS^2)/2));
    return ((1/underlying_price)*((delta-nShortedStocks)*dS));
#     return ((nxtdelta-delta));
}

num_stocks_to_short_direct <- function(underlying_price,dP,nShortedStocks,dS){
    return ((1/underlying_price)*(dP-nShortedStocks*dS));
}

show_deltas <- function(path,bs,hedged_pos) {
    plot(0,0,xlab="Time", ylab="Delta, HedgedPosition" , xlim=c(0,max(path$t)),ylim=c(-max(2*hedged_pos),max(2*hedged_pos)));
    cl<-rainbow(2);
    lines(path$t,bs$delta,col=cl[1],lty=1)
    lines(path$t,hedged_pos,col=cl[2],lty=2);
    print(hedged_pos)
    legend(1,10,c("delta","hedged pos"),col=cl, lty=c(1,2));
}

show_stock_opt <- function(path,option_prices,hedged_pos) {
    plot(0,0,xlab="Time", ylab="Prices" , xlim=c(0,max(path$t)),ylim=c(-max(path$values),max(path$values)));
    cl<-rainbow(3);
    lines(path$t,option_prices,col=cl[1],lty=1)
    lines(path$t,path$values,col=cl[2],lty=2);
    lines(path$t,hedged_pos,col=cl[3],lty=3);
    legend(1,-20,c("option","stock","hedged pos"),col=cl, lty=c(1,2,3));
}

hedged_position <- function (S_0,mu,vol,dt,T,K,r_f) {
    path = generate_path(S_0,mu,vol,dt,T);
    nh = length(path$values);
    option_prices=array();
    hedged_pos=array();
    bs=bscallprice(S_0=path$values,K=rep(K,nh),r_f=rep(r_f,nh),vol=rep(vol,nh),t=path$t,T=rep(T,nh));

    nShortedStocks = bs$delta[1];
    hedged_pos[1] =  -nShortedStocks*(path$values[1]) + bs$price[1];
    for ( i in seq (2,nh) ) {
           dS= path$values[i] - path$values[i-1];
           nShort=num_stocks_to_short(underlying_price=path$values[i],delta=bs$delta[i-1],nxtdelta=bs$delta[i],gamma=bs$gamma[i-1],nShortedStocks=nShortedStocks,dS=dS);
           #nShort=num_stocks_to_short_direct(underlying_price=path$values[i],dP=(bs$price[i]-bs$price[i-1]),nShortedStocks=nShortedStocks,dS=dS)
           nShortedStocks = nShortedStocks + nShort;
           hedged_pos[i] =  -nShortedStocks*(path$values[i]) + bs$price[i];
           #print(paste("price=",bs$price,"nshort=",nShort,"nShortedStocks=",nShortedStocks,"Position=",hedged_pos[i]));
    }
#    show_stock_opt(path=path,option_prices=bs$price,hedged_pos=hedged_pos);
    show_deltas(path,bs,hedged_pos)
    return(hedged_pos);
}

simul <- function(){
    S_0=50;
    mu=.2;
    vol=.9;
    dt=.01;
    T=2;
    K=50;
    r_f=.05;
    out=array();
 
    return(hedged_position(S_0,mu,vol,dt,T,K,r_f));
   
    for ( k in seq(500)){
        out[k]=sd(hedged_position(S_0,mu,vol,dt,T,K,r_f));
    }
    #sink("file://C:/Users/anuragr/Desktop/model_validation/output.txt");
    #cat(out);
    #sink();
    print(out);
    hist(out); return(sd(out));
}

 
