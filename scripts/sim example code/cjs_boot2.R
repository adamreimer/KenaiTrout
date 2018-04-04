rm(list=ls())
data.dir<-"C:/users/mconroy/downloads"

setwd(data.dir)

#simulate data under a CJS study with specified parameters
#see Kery and Schaub 2011 p 178

# VECTOR OF PHI AND P TO BE SPECIFIED LATER!!

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
###function to create capture history character strings (only need this for RMark runs)
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

cjs.boot<-function(Phi,p,marked,nreps)
{
require(RMark)
n.par<-length(Phi)
Phi.t.est<-Phi.0.est<-array(dim=c(nreps,n.par))

for(r in 1:nreps)
{
CH<-simul.cjs(Phi,p,marked)
CH.RMark<-pasty(CH)
rmark.data<-data.frame(ch=CH.RMark)
rmark.processed=process.data(rmark.data,model="CJS")
#default model is 'null'
#m.0<-mark(rmark.processed)

run.0<-function()
{m.0<-mark(rmark.processed,silent=TRUE,output=FALSE)
phi.0.est<-array(dim=n.par)
phi.0.est[1:n.par]<-rep(m.0$results$real$estimate[1],n.par)
return(phi.0.est)
}

test.0<-try(run.0(),TRUE)
if(any(class(test.0)=="try-error")) Phi.0.est[r,]<-NA else Phi.0.est[r,]=test.0

run.t<-function(){
Phi.t=list(formula=~time)
p.t=list(formula=~time)
m.t=mark(rmark.processed,model.parameters=list(Phi=Phi.t,p=p.t),silent=TRUE,output=FALSE)

phi.t.est<-array(dim=n.par)
phi.t.est[1:n.par]<-m.t$results$real$estimate[1:n.par]
return(phi.t.est)
}

test.t<-try(run.t(),TRUE)
if(any(class(test.t)=="try-error")) Phi.t.est[r,]<-NA else Phi.t.est[r,]<-test.t

}

Phi.true<-matrix(Phi,nrow=nreps,ncol=n.par,byrow=T)

Phi.t.bias<-apply((Phi.t.est-Phi.true),2,mean,na.rm=T)
Phi.t.var<-apply(Phi.t.est,2,var,na.rm=T)

Phi.0.bias<-apply((Phi.0.est-Phi.true),2,mean,na.rm=T)
Phi.0.var<-apply(Phi.0.est,2,var,na.rm=T)

return(list(Phi.t.est=Phi.t.est,Phi.0.est=Phi.0.est,Phi.t.bias=Phi.t.bias,Phi.t.var=Phi.t.var,Phi.0.bias=Phi.0.bias,Phi.0.var=Phi.0.var))

} #EOF



###SIMULATE DATA FOR FIXED AND PHI
n.occas<-6
marked<-rep(50,n.occas-1)
p.input<-rep(0.4,n.occas-1)
phi.input<-c(0.8,0.4,0.4,0.8,0.8)

results.fixed<-cjs.boot(Phi=phi.input,p=p.input,marked=marked,nreps=100)
results.fixed$Phi.t.bias
results.fixed$Phi.t.var
results.fixed$Phi.0.bias
results.fixed$Phi.0.var

cleanup(ask=F)


############################
#SIMULATE DATA AS A RANDOM EFFECT
n.occas<-6
marked<-rep(50,n.occas-1)
mean.phi<-0.65
var.phi<-1

p.input<-rep(.4,n.occas-1)
logit.phi<-rnorm(n.occas-1,qlogis(mean.phi),var.phi^0.5)

#plogis is a built in inverse logit transformation
phi.input<-plogis(logit.phi)

results.random<-cjs.boot(Phi=phi.input,p=p.input,marked=marked,nreps=100)
results.random$Phi.t.bias
results.random$Phi.t.var
results.random$Phi.0.bias
results.random$Phi.0.var
cleanup(ask=F)