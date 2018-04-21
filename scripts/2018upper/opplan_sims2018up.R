rm(list=ls())
require(RMark)

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
    capt.hist<-simul_js(PHI = PHI, P = P, b = b, N = N)
    #Closed population model
    m0<-mark(capt.hist$ch,model="Closed",model.parameters=list(p=p.dot),silent=T,output=F,delete=T)
    #pull off parameters you want
    M <- length(m0$data$freq)
    N.hat <- as.numeric(get.real(m0,"f0") + M)
    seN.hat <- get.real(m0, "f0", se = TRUE)$se
    rpN.hat <- seN.hat * 1.96 / N.hat
    n <- plyr::adply(capt.hist$ch, 1, function(x) substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1))) %>%
      dplyr::select(dplyr::starts_with("V")) %>%
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
sim_N <- c(3000, 5000, 7000, 9000)  # Superpopulation size
reps <- 25
ni300 <- mapply(sample_sim, p_dot = 300/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ni250 <- mapply(sample_sim, p_dot = 250/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ni200 <- mapply(sample_sim, p_dot = 200/sim_N, N = sim_N, MoreArgs = list(n_sim_reps = reps, events = 5, phi_dot = 1))
ss <- rbind(ni300, ni250, ni200)
colnames(ss) <- paste0("N=", sim_N)
rownames(ss) <- paste0(rownames(ss), ", n_i=", rep(c("300", "250", "200"), each = dim(ni250)[1]))
ss <- as.data.frame(ss)


##function to simulate AIC model selection, bias and coverage probabilities for populations with migration.
sim_modclass <- function(events = 6, 
                        b0 = 0.2,
                        phi0 = 0.8,
                        p_dot = 0.05,
                        sim_N = 6000){
  b <- c(1 - b0, rep(b0/(events - 1), events -1))
  phi <- rep(phi0^(1/(events - 1)), events - 1)
  PHI <- matrix(rep(phi, times = sim_N), ncol = events-1, nrow = sim_N, byrow = T)
  P <- matrix(rep(p_dot, times = events*sim_N), ncol = events, nrow = sim_N, byrow = T)
  
  dot <- list(formula=~1)
  time <- list(formula=~time)
  
  b_fix = list(fixed = list(index = seq(1:(events - 1)), value = 0))
  phi_fix = list(fixed = list(index = seq(1:(events - 1)), value = 1))
  
  dat <- process.data(simul_js(PHI = PHI, P = P, b = b, N = sim_N)$ch, model = "POPAN")
  des <- make.design.data(dat)
  pent_t <- mark(dat, ddl = des, model.parameters=list(Phi = dot, p = dot, pent = dot), output=F, delete=T)
  pent_f <- mark(dat, ddl = des, model.parameters=list(Phi = phi_fix, p = dot, pent = b_fix), output=F, delete=T)
  list(correct_AIC = pent_t$results$AICc < pent_f$results$AICc, 
       N_t = unlist(pent_t$results$real$estimate[grep("^N", rownames(pent_t$results$real))]),
       N_t_ci = (pent_t$results$real$lcl[grep("^N", rownames(pent_t$results$real))] < sim_N) & 
         (pent_t$results$real$ucl[grep("^N", rownames(pent_t$results$real))] > sim_N),
       N_f = unlist(pent_f$results$real$estimate[grep("^N", rownames(pent_f$results$real))]),
       N_f_ci = (pent_f$results$real$lcl[grep("^N", rownames(pent_f$results$real))] < sim_N) & 
         (pent_f$results$real$ucl[grep("^N", rownames(pent_f$results$real))] > sim_N))
}

plyr::adply(simul_js(PHI = PHI, P = P, b = b, N = sim_N)$ch, 1, function(x) substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1))) %>%
  dplyr::select(dplyr::starts_with("V")) %>%
  dplyr::mutate_all(as.numeric) %>%
  dplyr::summarise_all(sum)

#simulate AIC model selection, bias and coverage probabilities for a few immigration scenarios.
reps <- 25

b0_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0, phi0 = 1)), 1, function(x) mean(unlist(x)))
b10_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 1)), 1, function(x) mean(unlist(x)))
b20_phi99 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 1)), 1, function(x) mean(unlist(x)))
b30_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 1)), 1, function(x) mean(unlist(x)))

b0_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0, phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b10_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b20_phi90 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b30_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 0.9)), 1, function(x) mean(unlist(x)))

b0_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0)), 1, function(x) mean(unlist(x)))
b10_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1)), 1, function(x) mean(unlist(x)))
b20_phi80 <- plyr::adply(replicate(reps, sim_modclass()), 1, function(x) mean(unlist(x)))
b30_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3)), 1, function(x) mean(unlist(x)))

b0_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0, phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b10_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b20_phi70 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b30_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 0.7)), 1, function(x) mean(unlist(x)))
RMark::cleanup(ask = FALSE)

names <- c("stat", "b=0.0", "b=0.1", "b=0.2", "b=0.3")
mod_class <- rbind(
  plyr::join_all(list(b0_phi99, b10_phi99, b20_phi99, b30_phi99), by = "X1") %>% set_names(names) %>% dplyr::mutate(phi = "1.00"),
  plyr::join_all(list(b0_phi90, b10_phi90, b20_phi90, b30_phi90), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.90"),
  plyr::join_all(list(b0_phi80, b10_phi80, b20_phi80, b30_phi80), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.80"),
  plyr::join_all(list(b0_phi70, b10_phi70, b20_phi70, b30_phi70), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.70"))
###
#Ultimately decided mod class sims had limited value.  The margins (all b's with phi=1 and all phis with b=c1,0,...) were informative 
#but the other combinations represent through migration witch is unlikely. 
#Williams, Nichols and Conroy 2002 Analysis and Management of Animal Populations have a nice description of the more likely scenario
#of random migration across boundary when the study area is a subset of the population’s home range.


WriteXLS::WriteXLS(c("ss", "ss", "mod_class", "mod_class"), 
                   ".\\opplan\\2018upper\\sims2018.xlsx", 
                   SheetNames = c("ss_format", "ss_export", "modclass_format", "modclass_export"), 
                   BoldHeaderRow = TRUE)