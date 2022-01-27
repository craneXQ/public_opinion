intrerst_ca<-function(t=1,time_step=100,omega=pi/8,beta=0.1){
  k1<-c(t:time_step)
  k2<-seq(k1)
  k<-abs(exp(-1*beta*k2)*sin(omega*k2))
  k[which(k<0.00001)]<-0
  return(k)
}
# 最后一个非零观点
f1<-function(a){
  if(any(a!=0)){
    return(a[max(which(a!=0))])
  }
}
# 观点交互
opinion_change<-function(op1,op2,delta,miu){
  if(abs(op1-op2)>delta){
    return(op1)
  }else{
    return(op1+miu*(op2-op1))
  }
}

opinion_change_hk<-function(op1,op2,delta){
  op<-op2[which(abs(op2-op1)<delta)]
  return((op1+sum(op))/(length(op)+1))
  }

get_opinion_all<-function(li,time_step=100){
  ma<-matrix(rep(NA,length(li)*time_step),nrow =length(li))
  for (i in 1:length(li)) {
    for (ii in 1:time_step) {
      op<-li[[i]]$o[which(li[[i]]$t==ii)]
      if(length(op)>0){
        ma[i,ii]<-op[1]
      }else{
        if(ii>1){
          if(!is.na(ma[i,(ii-1)])){
            ma[i,ii]<-ma[i,(ii-1)]
          }
        }
      }
    }
  }
  return(ma)
}

get_inte<-function(data_1){
  fun<-approxfun(data_1[,1],data_1[,2])
  return(integrate(fun, 1, length(data_1[,2]),subdivisions = 3000,stop.on.error = F)[[1]])
}

f_op_change1<-function(op1,op2,edu,r=0.5,o_ran){
  k<-(1-abs(op1-0.5)*2)*(1-edu)*r
  if(k>o_ran){
    return(op2)
  }else{
    return(op1)
  }
}
f_op_change2<-function(op1,op2,edu,r=0.5,o_ran){
  k<-(1-edu)*r
  if(k>o_ran){
    return(op2*0.8+op1*(1-0.8))
  }else{
    return(op1)
  }
}







