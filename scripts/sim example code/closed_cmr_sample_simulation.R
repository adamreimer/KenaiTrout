rm(list=ls())

require(RMark)

#IMBEDDING SIMULATION MODEL IN LOOP TO INVESTIGATE SAMPLE SIZE AND OTHER INPUTS

##FIRST DEFINE SOME FUNCTIONS NEEDED
## FUNCTION TO CREATE CAPTURE HISTORY CHARACTER STRINGS 

pasty<-function(x) 
{
k<-ncol(x)
n<-nrow(x)
out<-array(dim=n)
for (i in 1:n)
{
out[i]<-paste(x[i,],collapse="")
}
return(out)
}

#Simulating data over n_sim_reps (ideally 1000 times)
sim.data<-function(N,p,k)
{
#simulate capture histories
y<-array(dim=c(N,k))
ind<-array(dim=N)

  for(i in 1:N)
    {
     y[i,1:k]<-rbinom(k,1,p)
     ind[i]<-sum(y[i,])>0
    }
#capture history data frame 
capt.hist<-data.frame(ch=pasty(y[,1:k]),ind=ind)
capt.hist<-subset(capt.hist,ind==T,select=c(ch))
#end of function
return(capt.hist)
}

#Simulating data over n_sim_reps (ideally 1000 times)
sim.data2<-function(N,p,k)
{
  #simulate capture histories
  y<-array(dim=c(N,k))
  ind<-array(dim=N)
  
  for(i in 1:N)
  {
    y[i,1:k]<-rbinom(k,1,p[k])
    ind[i]<-sum(y[i,])>0
  }
  #capture history data frame 
  capt.hist<-data.frame(ch=pasty(y[,1:k]),ind=ind)
  capt.hist<-subset(capt.hist,ind==T,select=c(ch))
  #end of function
  return(capt.hist)
}

####FUNCTION TO SIMULATE CAPTURE HISTORIES UNDER SPECIFIED INPUTS

#simulate capture histories from assumed model and estimated parameter values
#simulate data under homogeneous p 
#set up to simulate for specified inputs

sample_sim<-function(n_sim_reps,N,p,k)
{
#WE CAN DEFINE SOME THINGS OUTSIDE THE LOOP FOR EXAMPLE WE WILL USE THE SAME TIME CONSTANT MODEL EACH SIMU;LLATION
#Define parameters
p.dot=list(formula=~1,share=TRUE)
#SET UP AN 
#set up an empty data frame to store all the simulation results
output.data<-data.frame(k=numeric(0),rep=numeric(0),N.hat=numeric(0))

for(r in 1:n_sim_reps)
{
#
#function to create capture histories from simulated data
capt.hist<-sim.data(N=N,p=p,k=k)

#additional parameters to supress output each simulation
m0<-mark(capt.hist,model="Closed",model.parameters=list(p=p.dot),silent=T,output=F,delete=T)
#pull off just the estimate of N (you can select other parameters if you want)
N.hat <- as.numeric(get.real(m0,"f0") + length(m0$data$freq))
seN.hat <- get.real(m0, "f0", se = TRUE)$se
rpN.hat <- seN.hat * 1.96 / N.hat
#put in data frame
new.data<-data.frame(k = k, rep = r, N.hat = N.hat, se.N.hat = seN.hat, rpN.hat = rpN.hat)
#append to output data
output.data<-rbind(output.data,new.data)
}
#create  summay statistics
#empirical mean, sd, and cv
N.avg<-mean(output.data$N.hat)
N.sd<-sd(output.data$N.hat)
N.cv<-N.sd/N.avg
rpN.avg<-mean(output.data$rpN.hat)
#return simulated data and summary stats in list objects
N.summary<-list(N.avg=N.avg,N.sd=N.sd,N.cv=N.cv, rpN.avg = rpN.avg)

output=list(N.summary=N.summary,N.output=output.data)
}


sample_sim2<-function(n_sim_reps,N,p,k){
  #Define parameters
  p.dot=list(formula=~1,share=TRUE)
  #set up an empty data frame to store all the simulation results
  output.data<-data.frame(k=numeric(0),rep=numeric(0),N.hat=numeric(0))
  for(r in 1:n_sim_reps){
    #function to create capture histories from simulated data
    capt.hist<-sim.data(N=N,p=p,k=k)
    #additional parameters to supress output each simulation
    m0<-mark(capt.hist,model="Closed",model.parameters=list(p=p.dot),silent=T,output=F,delete=T)
    #pull off parameters you want)
    M <- length(m0$data$freq)
    N.hat <- as.numeric(get.real(m0,"f0") + M)
    seN.hat <- get.real(m0, "f0", se = TRUE)$se
    rpN.hat <- seN.hat * 1.96 / N.hat
    #put in data frame
    new.data<-data.frame(k = k, rep = r, N.hat = N.hat, se.N.hat = seN.hat, rpN.hat = rpN.hat, M = M)
    #append to output data
    output.data<-rbind(output.data,new.data)
  }
  #create  summay statistics
  list(N.avg = mean(output.data$N.hat),
       p = p,
       M_i.avg = mean(output.data$M)/k,
       rpN.avg = mean(output.data$rpN.hat))
}

sim_N <- seq(3000, 8000, length.out = 4)
sapply(sim_N, FUN = sample_sim2, n_sim_reps = 100, p = 0.03, k = 5)
sapply(sim_N, FUN = sample_sim2, n_sim_reps = 100, p = 0.05, k = 5)
sapply(sim_N, FUN = sample_sim2, n_sim_reps = 100, p = 0.075, k = 5)