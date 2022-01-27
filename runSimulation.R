source("r/main1.R")
#  Attributes of new information----
e1<-c(0.8,0.8,0.8)# the opinion of new information
e2<-c(50,51,52) # the time when new information was released
e3<-c(0.8,0.8,0.8) # the reliability of new information
e4<-c(0.8,0.8,0.8)# the coverage rate of new information
e5<-c(3,3,3)# the duration of new information
event_m<-data.frame(opinion=e1,time=e2,r=e3,fugai=e4,last_time=e5)# the data of new information
# The default settings
num_node<-400 # the number of nodes
time_step<-100 # the max time step of the simulation
delta<-rep(0.3,num_node) # the threshold of trust
miu<-rep(0.3,num_node)# updating factor
edu<-rtriangle(num_node,0,1,.9)# stubbornness of each individual
opinion<-rtriangle(num_node,0,1,.2) # opinion of each individual
de<-0 # de=1,use deffuant model,de=0, use new model
data1<-main1(delta=delta,miu=miu,time_step=time_step,num_node=num_node,graphmode="full",event=event_m,opinion=opinion,edu=edu,de=de,ad=2)