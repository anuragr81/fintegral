source('validation.r');

test04 <- function(){
  
  vec_dt=c(.01,.02,.05,.1,.15,.2,.3) 
  #vec_dt=c(.2,.3);
  for (dt in vec_dt){
    print(simul(calculate=1,optionType=1,dt=dt))
  }
  
}

test04_vol <- function(){
  
#  vec_vol=c(.03,.1,.14,.2,.3,.4,.8) 
  vec_vol=c(.4,.8);
  out= data.frame(vol=0,at=0,  mean_dmk=0,  mean_zpk=0,      sd_zpk=0,   sd_dmk=0)
  filename=("file://c:/local_files/anurag/model_validation/liquidity_task/output_vol.csv");
  
  for (vol in vec_vol){

    dat=simul(calculate=1,optionType=1,vol=vol,at=0,npaths=100000);
    out= data.frame(vol=c(out$vol,dat$vol),at=c(out$at,dat$at),  mean_dmk=c(out$mean_dmk,dat$mean_dmk),  
                    mean_zpk=c(out$mean_zpk,dat$mean_zpk),      
                    sd_zpk=c(out$sd_zpk,dat$sd_zpk),   sd_dmk=c(out$sd_dmk,dat$sd_dmk))
    
  }
  write.csv(out,file=filename);
  
}

test04_at_2 <- function(filename){
  
  vec_at=c(.4,.8);
  filename=(paste("file://c:/local_files/anurag/model_validation/liquidity_task/",filename,sep=""));
  out= data.frame(vol=0,at=0,  mean_dmk=0,  mean_zpk=0,      sd_zpk=0,   sd_dmk=0)
  for (at in vec_at){
    dat=simul(calculate=1,optionType=1,vol=.1,at=at,npaths=200000,dt=.01);
    out= data.frame(vol=c(out$vol,dat$vol),at=c(out$at,dat$at),  mean_dmk=c(out$mean_dmk,dat$mean_dmk),  
                    mean_zpk=c(out$mean_zpk,dat$mean_zpk),      
                    sd_zpk=c(out$sd_zpk,dat$sd_zpk),   sd_dmk=c(out$sd_dmk,dat$sd_dmk))
  }
  write.csv(out,file=filename);
  
}

test04_at <- function(filename){
  
  vec_at=c(0,.2);
  filename=(paste("file://c:/local_files/anurag/model_validation/liquidity_task/",filename,sep=""));
  out= data.frame(vol=0,at=0,  mean_dmk=0,  mean_zpk=0,      sd_zpk=0,   sd_dmk=0)
  for (at in vec_at){
    dat=simul(calculate=1,optionType=1,vol=.1,at=at,npaths=200000,dt=.01);
    out= data.frame(vol=c(out$vol,dat$vol),at=c(out$at,dat$at),  mean_dmk=c(out$mean_dmk,dat$mean_dmk),  
                    mean_zpk=c(out$mean_zpk,dat$mean_zpk),      
                    sd_zpk=c(out$sd_zpk,dat$sd_zpk),   sd_dmk=c(out$sd_dmk,dat$sd_dmk))
  }
  write.csv(out,file=filename);
  
}

test04_tc <- function(filename,vec_tc){
  
  filename=(paste("file://c:/local_files/anurag/model_validation/liquidity_task/",filename,sep=""));
  out= data.frame(vol=0,tc=0,at=0,  mean_dmk=0,  mean_zpk=0,      sd_zpk=0,   sd_dmk=0);
  for (tc in vec_tc){
    dat=simul(calculate=1,optionType=1,vol=.1,at=0,tc=tc,npaths=200000,dt=.01);
    out= data.frame(vol=c(out$vol,dat$vol),at=c(out$at,dat$at),tc=c(out$tc,dat$tc),  
                    mean_dmk=c(out$mean_dmk,dat$mean_dmk),  
                    mean_zpk=c(out$mean_zpk,dat$mean_zpk),      
                    sd_zpk=c(out$sd_zpk,dat$sd_zpk),   sd_dmk=c(out$sd_dmk,dat$sd_dmk))
  }
  write.csv(out,file=filename);
}


test04_sz <- function(filename,vec_sz){
  
  filename=(paste("./",filename,sep=""));
  out= data.frame(vol=0,tc=0,at=0,  mean_dmk=0,  mean_zpk=0,      sd_zpk=0,   sd_dmk=0);
  for (minsz in vec_sz){
    dat=simul(calculate=1,optionType=1,vol=.1,at=0,tc=0,minsz=minsz,npaths=200000,dt=.01);
    out= data.frame(vol=c(out$vol,dat$vol),at=c(out$at,dat$at),tc=c(out$tc,dat$tc),  
                    mean_dmk=c(out$mean_dmk,dat$mean_dmk),  
                    mean_zpk=c(out$mean_zpk,dat$mean_zpk),      
                    sd_zpk=c(out$sd_zpk,dat$sd_zpk),   sd_dmk=c(out$sd_dmk,dat$sd_dmk))
  }
  write.csv(out,file=filename);
}