rm(list=ls())
data.dir<-"C:/Users/mike/Dropbox/teaching/workshop/13"
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




############SIMULATE DATA WITH HETEROGENEITY IN P 
#########################################

############# heterogeneity in p among individuals

require(gtools) #needed for built in logit/ inv logit functions

#Simulating data over n_sim_reps (ideally 1000 times)
#average p with sigma logit scale random effect
sim.het.data<-function(N,p.avg,k,sigma)
{
#simulate capture histories
y<-array(dim=c(N,k))
ind<-array(dim=N)

#n random effects
 p<-inv.logit(rnorm(N,logit(p.avg),sigma))

  for(i in 1:N)
    {
     y[i,1:k]<-rbinom(k,1,p[i])
     ind[i]<-sum(y[i,])>0
    }
#capture history data frame 
capt.hist<-data.frame(ch=pasty(y[,1:k]),ind=ind)
capt.hist<-subset(capt.hist,ind==T,select=c(ch))
#end of function
return(capt.hist)
}

#NOW W'ELL ESTIMATE N AS IF P IS CONSTANT 
sample_sim.het<-function(n_sim_reps,N,p.avg,k,sigma)
{
#n_sim_reps=100
#N=100
#p.avg=0.2
#k=8
#sigma=0.8

#WE CAN DEFINE SOME THINGS OUTSIDE THE LOOP FOR EXAMPLE WE WILL USE THE SAME TIME CONSTANT MODEL EACH SIMU;LLATION
#Define parameters
p.dot=list(formula=~1,share=TRUE)
#SET UP AN 
#set up an empty data frame to store all the simulation results
output.data<-data.frame(k=numeric(0),rep=numeric(0),N.hat=numeric(0))

for(r in 1:n_sim_reps)
{
cat("iteration = ", iter <- r, "\n")
#parametric bootstrap 
#function to create capture histories from simulated data
capt.hist<-sim.het.data(N=N,p.avg=p.avg,k=k,sigma=sigma)

#additional parameters to supress output each simulation

result<-try(mark(capt.hist,model="Closed",model.parameters=list(p=p.dot),silent=T,output=F),TRUE)

if (class(result)=="try-error") {m0<-NA} else {m0=result}

result2<-try(get.real(m0,"N"),TRUE)
if (class(result2)=="try-error") {N.hat<-NA} else {N.hat=result2}


#pull off just the estimate of N (you can select other parameters if you want)

#put in data frame
new.data<-data.frame(k=k,rep=r,N.hat=N.hat)
#append to output data
output.data<-rbind(output.data,new.data)
}
#create  summay statistics
#empirical bias, mse, and variance
N.bias<-mean(output.data$N.hat,na.rm=TRUE)-N
N.hat<-output.data$N.hat
N.hat<-na.omit(N.hat)

result3<-try((N.hat-N)^2,TRUE)
if (class(result3)=="try-error") {mse<-NA} else {mse=result3}

N.mse<-mean(mse,na.rm=TRUE)
N.var<-N.mse-N.bias^2
N.rbias<-N.bias/N
N.cv<-sqrt(N.var)/N

N.summary<-list(N.bias=N.bias,N.mse=N.mse,N.var=N.var,N.rbias=N.rbias,N.cv=N.cv)

output=list(N.summary=N.summary,N.output=output.data)
}


#results for specific inputs
sim.results<-sample_sim.het(n_sim_reps=10,N=100,p.avg=0.2,k=2,sigma=0.8)

sim.results$N.summary





