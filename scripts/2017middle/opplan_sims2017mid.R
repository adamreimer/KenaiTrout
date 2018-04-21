rm(list=ls())
require(RMark)

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
    ent.occ <- c(ent.occ, rep(t, B[t]))
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

####Fit n_sim_reps closed population models and report average Nhat, reletive precision and average sample size per event
sample_sim<-function(n_sim_reps, events, phi_dot, p_dot, N){
  PHI <- matrix(rep(phi_dot, (events-1)*N), ncol = events-1, nrow = N, byrow = T) #Matrix of Survival probabilities
  b <- c(1, rep(0, events - 1))                  # Entry probabilities 
  P <- matrix(rep(p_dot, events*N), ncol = events, nrow = N, byrow = T) #Matrix of Capture probabilities

  #Define parameters
  p.dot=list(formula=~1,share=TRUE)
  #set up an empty data frame to store all the simulation results
  output.data<-data.frame(events=numeric(0),rep=numeric(0),N.hat=numeric(0))
  for(r in 1:n_sim_reps){
    #function to create capture histories from simulated data
    capt.hist<-simul.js(PHI = PHI, P = P, b = b, N = N)
    #Closed population model
    m0<-mark(capt.hist$ch,model="Closed",model.parameters=list(p=p.dot),silent=T,output=F,delete=T)
    #pull off parameters you want
    M <- length(m0$data$freq)
    N.hat <- as.numeric(get.real(m0,"f0") + M)
    seN.hat <- get.real(m0, "f0", se = TRUE)$se
    rpN.hat <- seN.hat * 1.96 / N.hat
    n <- plyr::adply(capt.hist$ch, 1, function(x) substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1))) %>%
        dplyr::select(starts_with("V")) %>%
        dplyr::mutate_all(as.numeric) %>%
        dplyr::summarise_all(sum) %>%
        apply(1, mean)
    #put in data frame
    new.data<-data.frame(events = events, rep = r, N.hat = N.hat, se.N.hat = seN.hat, rpN.hat = rpN.hat, n_iavg = n)
    #append to output data
    output.data<-rbind(output.data,new.data)
  }
  #create  summay statistics
  list(N.avg = mean(output.data$N.hat),
       p = p_dot,
       n_i.avg = mean(output.data$n_iavg),
       rpN.avg = mean(output.data$rpN.hat))
}
#relative precision at several population and sample sizes
sim_N <- c(4000, 6000, 8000, 10000)  # Superpopulation size
reps <- 100
ni250 <- mapply(sample_sim, p_dot = 250/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ni225 <- mapply(sample_sim, p_dot = 225/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ni200 <- mapply(sample_sim, p_dot = 200/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ni175 <- mapply(sample_sim, p_dot = 175/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ss <- rbind(ni250, ni225, ni200, ni175)
colnames(ss) <- paste0("N=", sim_N)
rownames(ss) <- paste0(rownames(ss), ", n_i=", rep(c("250", "225", "200", "175"), each = dim(ni250)[1]))
ss
WriteXLS::WriteXLS(as.data.frame(ss), ExcelFileName = "H:\\My Documents\\Kenai Trout\\2017sample sizes.xlsx", row.names = TRUE, AdjWidth = TRUE)

#relative precision at several populations and probabilities of capture (3 events)
ni04_3 <- mapply(sample_sim, p_dot = 0.04, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 3, phi_dot = 1))
ni05_3 <- mapply(sample_sim, p_dot = 0.05, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 3, phi_dot = 1))
ni06_3 <- mapply(sample_sim, p_dot = 0.06, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 3, phi_dot = 1))
ss_3events <- rbind(ni04_3, ni05_3, ni06_3)
colnames(ss_3events) <- paste0("N=", sim_N)
rownames(ss_3events) <- paste0(rownames(ss_3events), ", p=", rep(c("0.04", "0.05", "0.06"), each = dim(ni04_3)[1]))
ss_3events
WriteXLS::WriteXLS(as.data.frame(ss_3events), ExcelFileName = "H:\\My Documents\\Kenai Trout\\2017sample sizes_3events.xlsx", row.names = TRUE, AdjWidth = TRUE)

#relative precision at several probabilities of capture (4 events)
sim_N <- c(6000)  # Superpopulation size
reps <- 100
ni04_4 <- mapply(sample_sim, p_dot = 0.04, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 4, phi_dot = 1))
ni05_4 <- mapply(sample_sim, p_dot = 0.05, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 4, phi_dot = 1))
ni06_4 <- mapply(sample_sim, p_dot = 0.06, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 4, phi_dot = 1))
ss_4events <- rbind(ni04_4, ni05_4, ni06_4)
colnames(ss_4events) <- paste0("N=", sim_N)
rownames(ss_4events) <- paste0(rownames(ss_4events), ", p=", rep(c("0.04", "0.05", "0.06"), each = dim(ni04_4)[1]))
ss_4events
WriteXLS::WriteXLS(as.data.frame(ss_4events), ExcelFileName = "H:\\My Documents\\Kenai Trout\\2017sample sizes_4events.xlsx", row.names = TRUE, AdjWidth = TRUE)


####Fit n_sim_reps open population models and report average Nhat, reletive precision and sample size per event
sample_sim2 <- function(n_sim_reps, 
                        events,
                        b = c(.96, 0.01, 0.01, 0.01 ,0.01),
                        phi = rep(1, events-1),
                        p_dot, 
                        sim_N){
  PHI <- matrix(rep(phi, times = sim_N), ncol = events-1, nrow = sim_N, byrow = T) #Matrix of Survival probabilitie
  P <- matrix(rep(p_dot, events*sim_N), ncol = events, nrow = sim_N, byrow = T) #Matrix of Capture probabilities
  dot <- list(formula=~1)
  time <- list(formula=~time)
  
  #set up an empty data frame to store all the simulation results
  output.data<-data.frame(events=numeric(0),rep=numeric(0),N.hat=numeric(0))
  for(r in 1:n_sim_reps){
    capt.hist <- simul.js(PHI = PHI, P = P, b = b, N = sim_N)$ch
    dat <- process.data(capt.hist, model = "POPAN")
    des <- make.design.data(dat, list(Phi = list(pim.type = "time"), p = list(pim.type = "time"), pent = list(pim.type = "time")))
    m0 <- mark(dat, ddl = des, model.parameters=list(Phi = time, p = dot, pent = time), output=F, silent=T)
    #pull off parameters you want
    M <- length(m0$data$freq)
    N.hat <- unlist(m0$results$real$estimate[grep("^N", rownames(m0$results$real))])
    seN.hat <- unlist(m0$results$real$se[grep("^N", rownames(m0$results$real))])
    rpN.hat <- seN.hat * 1.96 / N.hat
    n <- plyr::adply(capt.hist$ch, 1, function(x) substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1))) %>%
      dplyr::select(starts_with("V")) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::summarise_all(sum)
    #put in data frame
    new.data<-data.frame(events = events, 
                         rep = r, 
                         N.hat = N.hat, 
                         se.N.hat = seN.hat, 
                         rpN.hat = rpN.hat, 
                         n1 = n[, 1], 
                         n2 = n[, 2], 
                         n3 = n[, 3], 
                         n4 = n[, 4], 
                         n5 = n[, 5])
    #append to output data
    output.data<-rbind(output.data,new.data)
  }
  #create  summay statistics
  list(N.avg = mean(output.data$N.hat),
       p = p_dot,
       n_1.avg = mean(output.data$n1),
       n_2.avg = mean(output.data$n2),
       n_3.avg = mean(output.data$n3),
       n_4.avg = mean(output.data$n4),
       n_5.avg = mean(output.data$n5),
       rpN.avg = mean(output.data$rpN.hat))
}

#relative precision at several populations and probabilities of capture with immigration
sim_N <- c(4000, 6000, 8000, 10000)  # Superpopulation size
reps <- 100
open_ss04 <- mapply(sample_sim2, p_dot = 0.04, sim_N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, b = c(0.3, 0.3, 0.4, 0, 0)))
open_ss05 <- mapply(sample_sim2, p_dot = 0.05, sim_N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, b = c(0.3, 0.3, 0.4, 0, 0)))
open_ss <- rbind(open_ss04, open_ss05)
colnames(open_ss) <- paste0("N=", sim_N)
rownames(open_ss) <- paste0(rownames(open_ss), ", p=", rep(c("0.04", "0.05"), each = dim(open_ss04)[1]))
open_ss
WriteXLS::WriteXLS(as.data.frame(open_ss), ExcelFileName = "H:\\My Documents\\Kenai Trout\\2017sample sizes_open.xlsx", row.names = TRUE, AdjWidth = TRUE)



##function to simulate AIC model selection, bias and coverage probabilities for populations with migration.
dot <- list(formula=~1)
time <- list(formula=~time)
b_fix <- list(fixed = list(index = c(1, 2, 3, 4), value = 0.01))
phi_fix <- list(fixed = list(index = c(1, 2, 3, 4), value = 0.9))
immigration <- function(n.occasions = 5, 
                        b = c(.96, 0.01, 0.01, 0.01 ,0.01),
                        b_par = b_fix,
                        phi = rep(.9, n.occasions-1),
                        phi_par = phi_fix,
                        sim_N = 5000,
                        p_dot = 0.05){
  PHI <- matrix(rep(phi, times = sim_N), ncol = n.occasions-1, nrow = sim_N, byrow = T)
  P <- matrix(rep(p_dot, n.occasions*sim_N), ncol = n.occasions, nrow = sim_N, byrow = T)
  
  dat <- process.data(simul.js(PHI = PHI, P = P, b = b, N = sim_N)$ch, model = "POPAN")
  des <- make.design.data(dat, list(Phi = list(pim.type = "time"), p = list(pim.type = "time")))
  pent_t <- mark(dat, ddl = des, model.parameters=list(Phi = phi_par, p = dot, pent = b_par),output=F,silent=T)
  pent_f <- mark(dat, ddl = des, model.parameters=list(Phi = phi_fix, p = dot, pent = b_fix),output=F,silent=T)
  list(correct_AIC = pent_t$results$AICc < pent_f$results$AICc, 
       N_t = unlist(pent_t$results$real$estimate[grep("^N", rownames(pent_t$results$real))]),
       N_t_ci = (pent_t$results$real$lcl[grep("^N", rownames(pent_t$results$real))] < sim_N) & 
         (pent_t$results$real$ucl[grep("^N", rownames(pent_t$results$real))] > sim_N),
       N_f = unlist(pent_f$results$real$estimate[grep("^N", rownames(pent_f$results$real))]),
       N_f_ci = (pent_f$results$real$lcl[grep("^N", rownames(pent_f$results$real))] < sim_N) & 
         (pent_f$results$real$ucl[grep("^N", rownames(pent_f$results$real))] > sim_N))
}

#simulate AIC model selection, bias and coverage probabilities for a few immigration scenarios.
reps <- 1000
b0_7_3 <- plyr::adply(replicate(reps, immigration(b = c(.7, .3, rep(0, 3)), b_par = time)), 1, function(x) mean(unlist(x)))
b0_5_3_2 <- plyr::adply(replicate(reps, immigration(b = c(.5, .3, .2, rep(0, 2)), b_par = time)), 1, function(x) mean(unlist(x)))
b0_3_3_4 <- plyr::adply(replicate(reps, immigration(b = c(.3, .3, .4, rep(0, 2)), b_par = time)), 1, function(x) mean(unlist(x))) #B from 2011 tags
b0_2_3_5 <- plyr::adply(replicate(reps, immigration(b = c(.2, .3, .5, rep(0, 2)), b_par = time)), 1, function(x) mean(unlist(x))) 
b0 <- dplyr::full_join(
        dplyr::full_join(
          dplyr::full_join(b0_7_3, b0_5_3_2, , by = "X1"), 
        b0_3_3_4, , by = "X1"), 
      b0_2_3_5, by = "X1")
colnames(b0) <- c("", 
                  "b=(.7, .3, 0, 0, 0)",
                  "b=(.5, .3, .2, 0, 0)",
                  "b=(.3, .3, .4, 0, 0)",
                  "b=(.2, .3, .5, 0, 0)")
b0

#simulate AIC model selection, bias and coverage probabilities for a few emigration scenarios.
phi_15 <- plyr::adply(replicate(reps, immigration(phi = c(rep(.9, 3), .765), phi_par = time)), 1, function(x) mean(unlist(x))) # phi from 2011 tags 0.15 emigartion
phi_3 <- plyr::adply(replicate(reps, immigration(phi = c(rep(.9, 3), .63), phi_par = time)), 1, function(x) mean(unlist(x))) # 0.3 emigration
phi_6 <- plyr::adply(replicate(reps, immigration(phi = c(rep(.9, 3), .36), phi_par = time)), 1, function(x) mean(unlist(x))) # 0.6 emigration
phi <- dplyr::full_join(
          dplyr::full_join(phi_15, phi_3, , by = "X1"), 
        phi_6, by = "X1")
colnames(phi) <- c("", 
                  "phi=(.9, .9, .9, .9*.85)",
                  "phi=(.9, .9, .9, .9*.7)",
                  "phi=(.9, .9, .9, .9*.4)")
phi
WriteXLS::WriteXLS(c("b0", "phi"), ExcelFileName = "H:\\My Documents\\Kenai Trout\\2017sims.xlsx", AdjWidth = TRUE)



#Just a peice of code to quickly produce and RMark object and look at it's structure
# dat0 <- simul.js(N = 7500, PHI = matrix(rep(1, (5-1)*7500), ncol = 5-1, nrow = 7500, byrow = T), 
#                  P = matrix(rep(.033, 5*7500), ncol = 5, nrow = 7500, byrow = T),
#                  b = c(1, rep(0, 5 - 1)))
# dat <- process.data(dat0$ch, model = "Closed")
# test <- mark(dat, 
#              ddl = make.design.data(dat, list(p = list(pim.type = "time", share = TRUE))),
#              output=T,silent=T)