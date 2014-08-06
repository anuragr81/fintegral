
show_deltas <- function(t,notc_deltas,notc_hedged_pos,tc_deltas,tc_hedged_pos) {
    plot(0,0,xlab="Time", ylab="Delta, HedgedPosition(normalized)", xlim=c(0,max(t)),ylim=c(-2,3));
    cl<-rainbow(2);
    lines(t,notc_deltas,col=cl[1],lty=1)
    lines(t,notc_hedged_pos/max(abs(notc_hedged_pos)),col=cl[2],lty=2);
    lines(t,tc_hedged_pos/max(abs(tc_hedged_pos)),col=cl[3],lty=3);
    legend(1,3,c("delta","hedged pos (normalized-noTC)","hedged pos(normalized-TC)"),col=cl, lty=c(1,2,3,4));
}

show_stock_opt <- function(path,option_prices,hedged_pos) {
    plot(0,0,xlab="Time", ylab="Prices" , xlim=c(0,max(path$t)),ylim=c(-max(path$values),max(path$values)));
    cl<-rainbow(3);
    lines(path$t,option_prices,col=cl[1],lty=1)
    lines(path$t,path$values,col=cl[2],lty=2);
    lines(path$t,hedged_pos,col=cl[3],lty=3);
    legend(1,-20,c("option","stock","hedged pos"),col=cl, lty=c(1,2,3));
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

