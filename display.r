
show_comparison <- function(t,regular_hedged_pos,special_hedged_pos,tagname) {
  
  # show underlying and delta
  # factor=10^(as.integer(log(max(path_values),10)));
  #a = a/factor X factor

  #plot(0,0,xlab="Time", ylab=paste("price/",factor), 
  #     xlim=c(0,max(t)),ylim=c(-10,10));
  #cl<-rainbow(2);
  #lines(t,path_values/factor,col=cl[1],lty=1)
  #lines(t,deltas,col=cl[2],lty=2);
  #legend(.1,10,c(paste("UnderlyingPrice/",factor),
  #                                        "Delta"),
  #      col=cl, lty=c(1,2));
  
  plot_ylim<-c(min(min(regular_hedged_pos),min(special_hedged_pos)),max(max(regular_hedged_pos),max(special_hedged_pos)));  
  plot(0,0,xlab="Time", ylab=tagname, xlim=c(0,max(t)),ylim=plot_ylim);
  cl<-rainbow(2);
  lines(t,regular_hedged_pos,col=cl[1],lty=1,type='s');
  lines(t,special_hedged_pos,col=cl[2],lty=2,type='s');
  #legend(.1*max(t),plot_ylim[1]*(0.86),c("hedged-pos (regular)","hedged-pos (special))"),col=cl, lty=c(1,2));
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
