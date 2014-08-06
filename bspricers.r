
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

