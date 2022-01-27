graphG<-function(num_node=1000,graphmode="scalefree"){
  
  # 建立网络----------------------------
  if(graphmode=="scalefree"){
    g1 <- sample_pa(num_node,directed = F)
  }else if(graphmode=="full"){
    g1 <- make_full_graph(num_node)
  }else if(graphmode=="er_mode"){
    g1<- sample_gnp(num_node,0.5)
  }
  # 节点的邻居统计
  d1<-ego(g1,order = 1,nodes=V(g1),mode="all")

  g_list<-list(d1=d1,num_node=num_node)
  
  return(g_list)
  
}