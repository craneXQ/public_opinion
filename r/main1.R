library(igraph)
library(ggplot2)
library(ineq)
source("r/graphG.R")
# source("r/graphG.R")
source("r/run_sim.R")
source("r/function.R")
# delta 观点差值的阈值， miu 观点收敛系数， time_step 运行步数 graphmode 图的类型 "scalefree" "er_mode"  "full", event 加入事件的属性
# main1<-function(ac=2.0,delta=0.3,miu=0.2,p=0.3,a=10,time_step=100, num_node=400, continue=5,omega_max=pi/2,omega_min = pi/4,hk=1,sr_m=1,sr_v=0.5,graphmode="scalefree"){
main1<-function(delta,miu,time_step=100, num_node=400,graphmode="full",event,opinion,edu,de=0,ad=1){
  # 建立网络,节点数量N，----
  data1<-graphG(num_node=num_node,graphmode=graphmode)

  data1<-run_sim(data1,delta=delta,miu=miu,time_step=time_step,event=event,opinion=opinion,edu=edu,de=de,ad=ad)
  return(data1)
}



