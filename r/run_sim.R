run_sim<-function(data1,delta,miu,time_step=100,event,opinion,edu,de=1,ad=1){
  # 将列表数据取出
  d1<-data1$d1
  num_node<-data1$num_node
  
  # 群体观点 
  opinion_i<-matrix(rep(0,(time_step+1)*num_node),ncol = (time_step+1))
  opinion_i[,1]<-opinion
  # 个体观点的改变值
  op_change<-matrix(rep(0,(time_step+1)*num_node),ncol = (time_step+1))
  # 个体交互的矩阵
  op_int<-matrix(rep(0,(time_step+1)*num_node),ncol = (time_step+1))
  for(i in seq(num_node)){
    op_int[i,]<-sample(d1[[i]][-1],size = (time_step+1), replace = T)
  }
  #事件相关设置
  event_time<-length(event$time)
  ei<-1
  #与事件的交互
  
  
  
  # 提前生成随机数
  o_random<-runif(10000)
  o_r<-1
  
  if(de==0){
    if(ad==1){
    for (i in seq(time_step)) {
      if(i==event$time[ei]){
        op_e_in<-sample(seq(num_node),num_node*event$fugai[ei],replace = F)
        for(ii in seq(num_node)){
          if(ii %in% op_e_in){
            if(abs(opinion_i[ii,i]-event$opinion[ei])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              
            }else{
              opinion_i[ii,(i+1)]<-f_op_change1(op1=opinion_i[ii,(i)],op2=event$opinion[ei],edu=edu[ii],r=0.5,o_ran=o_random[o_r])
              o_r<-o_r+1
              if(o_r>length(o_random)){
                o_random<-c(o_random,runif(10000))
              }
            }
          }
          if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
            opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
            
          }else{
            opinion_i[ii,(i+1)]<-f_op_change1(op1=opinion_i[ii,(i)],op2=opinion_i[op_int[ii,i],i],edu=edu[ii],r=0.5,o_ran=o_random[o_r])
            
            o_r<-o_r+1
            if(o_r>length(o_random)){
              o_random<-c(o_random,runif(10000))
            }
          }
          op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
        }
      }
      for(ii in seq(num_node)){
        if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
          opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
          
        }else{
          opinion_i[ii,(i+1)]<-f_op_change1(op1=opinion_i[ii,(i)],op2=opinion_i[op_int[ii,i],i],edu=edu[ii],r=0.5,o_ran=o_random[o_r])
          
          o_r<-o_r+1
          if(o_r>length(o_random)){
            o_random<-c(o_random,runif(10000))
          }
        }
        op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
      }
    }
      # if(sum(op_change[,i]<0.01)) {
      #   break
      #   }
    }else if(ad==2){
      for (i in seq(time_step)) {
        if(!ei>length(event$time)){
        if(i==event$time[ei]){
          op_e_in<-sample(seq(num_node),num_node*event$fugai[ei],replace = F)
          for(ii in seq(num_node)){
           
            if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              
            }else{
              opinion_i[ii,(i+1)]<-f_op_change2(op1=opinion_i[ii,(i)],op2=opinion_i[op_int[ii,i],i],edu=edu[ii],r=event$r[ei],o_ran=o_random[o_r])
              
              o_r<-o_r+1
              if(o_r>length(o_random)){
                o_random<-c(o_random,runif(10000))
              }
            }
            if(ii %in% op_e_in){
              if(abs(opinion_i[ii,i]-event$opinion[ei])<delta[ii]){
                opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-event$opinion[ei])
                
              }else{
                opinion_i[ii,(i+1)]<-f_op_change2(op1=opinion_i[ii,(i)],op2=event$opinion[ei],edu=edu[ii],r=event$r[ei],o_ran=o_random[o_r])
                o_r<-o_r+1
                if(o_r>length(o_random)){
                  o_random<-c(o_random,runif(10000))
                }
              }
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
          ei<-ei+1
        }else{
          for(ii in seq(num_node)){
            if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              # browser()
            }else{
              opinion_i[ii,(i+1)]<-f_op_change2(op1=opinion_i[ii,(i)],op2=opinion_i[op_int[ii,i],i],edu=edu[ii],r=0.5,o_ran=o_random[o_r])
              # browser()
              o_r<-o_r+1
              if(o_r>length(o_random)){
                o_random<-c(o_random,runif(10000))
              }
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
        }}else{
          for(ii in seq(num_node)){
            if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              # browser()
            }else{
              opinion_i[ii,(i+1)]<-f_op_change2(op1=opinion_i[ii,(i)],op2=opinion_i[op_int[ii,i],i],edu=edu[ii],r=0.5,o_ran=o_random[o_r])
              # opinion_i[ii,(i+1)]<-opinion_i[ii,(i)]
              # browser()
              o_r<-o_r+1
              if(o_r>length(o_random)){
                o_random<-c(o_random,runif(10000))
              }
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
        }
       
        
      }
    }
  }else if(de==1){
    for (i in seq(time_step)) {
      if(!ei>length(event$time)){
        if(i==event$time[ei]){
          op_e_in<-sample(seq(num_node),num_node*event$fugai[ei],replace = F)
          for(ii in seq(num_node)){
            if(abs(opinion_i[ii,i]-event$opinion[ei])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-event$opinion[ei])
              # browser()
            }else{
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
          ei<-ei+1
        }else{
          for(ii in seq(num_node)){
            if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              # browser()
            }else{
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
        }}else{
          for(ii in seq(num_node)){
            if(abs(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])<delta[ii]){
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]-miu[ii]*(opinion_i[ii,i]-opinion_i[op_int[ii,i],i])
              # browser()
            }else{
              opinion_i[ii,(i+1)]<-opinion_i[ii,i]
            }
            op_change[ii,i]<-abs(opinion_i[ii,(i+1)]-opinion_i[ii,i])
          }
        }
      
    }
  }
  
  
    return(list(op_change=op_change,opinion_i=opinion_i,op_int=op_int,o_r=o_r,delta=delta))
  }
  