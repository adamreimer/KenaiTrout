library(RMark)
rm(list=ls())
#specifiy number of occasions & number new marked invidivuals/ occasion
n.occasions<-6.
marked<-rep(500,n.occasions-1)

#specify vector of survival and capture probabilities
#example with constant phi and p over time
phi<-rep(0.65,n.occasions-1)
p<-rep(0.4,n.occasions-1)
#example with phi and p varying according to uniform distribution over time
#phi<-runif(n.occasions-1,0.5,0.8)
#p<-runif(n.occasions-1,0.2,.6)

#simulate CJS data for 1 group
simul.cjs<-function(phi,p,marked)
{
n.occasions<-length(p)+1
Phi<-matrix(phi,n.occasions-1,nrow=sum(marked),byrow=T)
P<-matrix(p,n.occasions-1,nrow=sum(marked),byrow=T)

#n.occasions<-dim(Phi)[2]+1
CH<-matrix(0,ncol=n.occasions,nrow=sum(marked))
#define a vector with marking occasion
mark.occ<-rep(1:length(marked),marked[1:length(marked)])
#fill in CH
for (i in 1:sum(marked))
      {
CH[i,mark.occ[i]]<-1
 if (mark.occ[i]==n.occasions) next
    for(t in (mark.occ[i]+1):n.occasions)
          {
         #survive?
         sur<-rbinom(1,1,Phi[i,t-1])
          if(sur==0) break #move to next
          #recaptured?
          rp<-rbinom(1,1,P[i,t-1])
          if(rp==1) CH[i,t]<-1
            } #t
        } #i
return(CH)
}
###function to create capture history character strings (need for input to RMARK)
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



#call simulation for a 2-group example
n.occasions<-6.
marked<-rep(50,n.occasions-1)
phi.m<-rep(0.8,n.occasions-1)
phi.f<-rep(0.5,n.occasions-1)
p<-rep(0.4,n.occasions-1)
#simulate female and male histories
sim.fem<-simul.cjs(phi.f,p,marked)
sim.mal<-simul.cjs(phi.m,p,marked)
#put togther in format needed for RMark
fem.hist<-data.frame(ch=pasty(sim.fem),sex="Female")
mal.hist<-data.frame(ch=pasty(sim.mal),sex="Male")

sim.data<-rbind(fem.hist,mal.hist)
sim.processed=process.data(sim.data,model="CJS",groups="sex")
sim.ddl=make.design.data(sim.processed)


#formulas for general ("global") CJS model (sex*time)
Phi.sex.T=list(formula=~sex*time)
p.sex.T=list(formula=~sex*time)
#time only
Phi.t=list(formula=~time)
p.t=list(formula=~time)
global.est<-mark(sim.processed,sim.ddl,model.parameters=list(Phi=Phi.sex.T,p=p.sex.T),output=F,silent=T)
time.est<-mark(sim.processed,sim.ddl,model.parameters=list(Phi=Phi.t,p=p.t),output=F,silent=T)
null.est<-mark(sim.processed,sim.ddl,output=F,silent=T)


summary(global.est)
summary(time.est)
summary(null.est)

results<-collect.models()
