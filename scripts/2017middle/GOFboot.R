rm(list=ls())
require(RMark)
library(magrittr)

data <- readRDS(".\\data\\dat_17")
#add fl, maturity and temperature at capture to CH
CH <- readRDS(".\\data\\CH_17") %>%
  dplyr::left_join(data[data$recap != TRUE, c("tag", "fl")], by = "tag") %>%
  dplyr::mutate(fl = ifelse(is.na(fl), 401, fl),
                fl_g = cut(fl, breaks = c(0, 400, 900), labels = c("small", "large")))
table(CH$fl_g, useNA = "always")

#process dataset
dat_fl <- process.data(data.frame(ch = CH$ch, fl = CH$fl_g, stringsAsFactors = FALSE),
                       model = "POPAN", 
                       groups = "fl")
release.gof(dat_fl)

#create design data
ddl_fl = make.design.data(dat_fl)
global<-mark(dat_fl,
             ddl_fl,
             model.parameters=list(Phi=list(formula=~fl*time),
                                   p=list(formula=~fl),
                                   pent=list(formula=~fl*time),
                                   N=list(formula=~fl)))

## FUNCTION TO CREATE CAPTURE HISTORY CHARACTER STRINGS 
pasty<-function(x) {
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n){
    out[i]<-paste(x[i,],collapse="")
  }
  return(out)
}

#Function to simulate capture-recapture data under the JS model (POPAN)
#Kery and Schaub BPA pg. 329, modifed so that CH is in MARK format
simul.js <- function(PHI, P, b, N){
  B <- rmultinom(1, N, b) # Generate no. of entering ind. per occasion
  events <- dim(PHI)[2] + 1
  CH.sur <- CH.p <- matrix(0, ncol = events, nrow = N)
  # Define a vector with the occasion of entering the population
  ent.occ <- numeric()
  for (t in 1:events){
    ent.occ <- c(ent.occ, rep(t, B[t, 1]))
  }
  # Simulating survival
  for (i in 1:N){
    CH.sur[i, ent.occ[i]] <- 1   # Write 1 when ind. enters the pop.
    if (ent.occ[i] == events) next
    for (t in (ent.occ[i]+1):events){
      # Bernoulli trial: has individual survived occasion?
      sur <- rbinom(1, 1, PHI[i,t-1])
      ifelse (sur==1, CH.sur[i,t] <- 1, break)
    } #t
  } #i
  # Simulating capture
  for (i in 1:N){
    CH.p[i,] <- rbinom(events, 1, P[i,])
  } #i
  # Full capture-recapture matrix
  ch <- CH.sur * CH.p
  
  # Remove individuals never captured
  cap.sum <- rowSums(ch)
  never <- which(cap.sum == 0)
  ch <- data.frame(ch=pasty(ch[-never,1:events]))
  Nt <- colSums(CH.sur)    # Actual population size
  return(list(ch=ch, B=B, N=Nt))
}



### get parameter estimates and number markedfrom Dipper global model 
Groups<-c("small","large")
n.occasions<-6
params<-global$results$real

sample_dev <- function(n_sim_reps,
                        b = list(c(1-sum(params$estimate[15:19]), params$estimate[15:19]), c(1-sum(params$estimate[20:24]), params$estimate[20:24])),
                        phi = list(params$estimate[1:5], params$estimate[6:10]),
                        p = list(params$estimate[11], params$estimate[12]),
                        sim_N = list(params$estimate[13], params$estimate[14])){
  events <- length(b[[1]])
  PHI <- mapply(function(x, y) matrix(rep(x, times = y), ncol = events - 1, nrow = y, byrow = TRUE), x = phi, y = sim_N) #Matrix of Survival probabilitie
  P <- mapply(function(x, y) matrix(rep(x, times = y), ncol = events, nrow = y, byrow = TRUE), x = p, y = sim_N) #Matrix of Capture probabilities
  
  #set up an empty data frame to store all the simulation results
  deviance<-dim(n_sim_reps)
  for(r in 1:n_sim_reps){
    small <- simul.js(PHI = PHI[[1]], P = P[[1]], b = b[[1]], N = sim_N[[1]])$ch
    large <- simul.js(PHI = PHI[[2]], P = P[[2]], b = b[[2]], N = sim_N[[2]])$ch
    capt.hist <- list(ch = as.character(unlist(rbind(small, large))),
                      fl = factor(c(rep("small", dim(small)[1]), rep("large", dim(large)[1]))))
    dat <- process.data(data.frame(ch = capt.hist$ch, fl = capt.hist$fl, stringsAsFactors = FALSE), model = "POPAN", groups = "fl")
    des <- make.design.data(dat)
    m0 <- mark(dat, ddl = des, model.parameters=list(Phi=list(formula=~fl*time),
                                                     p=list(formula=~fl),
                                                     pent=list(formula=~fl*time),
                                                     N=list(formula=~fl)),
                output=F, silent=T)
    deviance[r]<-m0$results$deviance
    }
  out<-list(deviance.mean=mean(deviance),deviance.025=quantile(deviance,0.025),deviance.975=quantile(deviance,0.975))
}
sim.out <- sample_dev(n_sim_reps = 100)

data.deviance<-global$results$deviance
data.deviance
sim.ci<-c(sim.out$deviance.025,sim.out$deviance.975)
cat("data deviance = ",data.deviance, "simulation mean = ", sim.out$deviance.mean, "simulation 95%CI = ", sim.ci,  "\n")
