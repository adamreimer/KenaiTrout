rm(list=ls())
require(RMark)

##function to simulate AIC model selection, bias and coverage probabilities for populations with migration.
sim_modclass <- function(events = 6, 
                        b0 = 0.2,
                        phi0 = 0.8,
                        p_dot = 0.05,
                        sim_N = 8000){
  event_b <- sample(2:events, 1)
  b <- c(1 - b0 - 0.01*(events - 2), rep(0.01, events -1))
  b[event_b] <- b0
  event_phi <- sample(1:(events - 1), 1)
  phi <- rep(0.99, events - 1)
  phi[event_phi] <- phi0
  PHI <- matrix(rep(phi, times = sim_N), ncol = events-1, nrow = sim_N, byrow = T)
  P <- matrix(rep(p_dot, times = events*sim_N), ncol = events, nrow = sim_N, byrow = T)
  
  dot <- list(formula=~1)
  time <- list(formula=~time)
  
  b_fix = list(fixed = list(index = seq(1:(events - 1)), value = 0.01))
  phi_fix = list(fixed = list(index = seq(1:(events - 1)), value = 0.99))
  
  dat <- process.data(simul_js(PHI = PHI, P = P, b = b, N = sim_N)$ch, model = "POPAN")
  des <- make.design.data(dat)
  pent_t <- mark(dat, ddl = des, model.parameters=list(Phi = time, p = dot, pent = time), output=F, silent=T)
  pent_f <- mark(dat, ddl = des, model.parameters=list(Phi = phi_fix, p = dot, pent = b_fix), output=F, silent=T)
  list(correct_AIC = pent_t$results$AICc < pent_f$results$AICc, 
       N_t = unlist(pent_t$results$real$estimate[grep("^N", rownames(pent_t$results$real))]),
       N_t_ci = (pent_t$results$real$lcl[grep("^N", rownames(pent_t$results$real))] < sim_N) & 
         (pent_t$results$real$ucl[grep("^N", rownames(pent_t$results$real))] > sim_N),
       N_f = unlist(pent_f$results$real$estimate[grep("^N", rownames(pent_f$results$real))]),
       N_f_ci = (pent_f$results$real$lcl[grep("^N", rownames(pent_f$results$real))] < sim_N) & 
         (pent_f$results$real$ucl[grep("^N", rownames(pent_f$results$real))] > sim_N))
}

#simulate AIC model selection, bias and coverage probabilities for a few immigration scenarios.
reps <- 50

b0_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0, phi0 = 0.99)), 1, function(x) mean(unlist(x)))
b10_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 0.99)), 1, function(x) mean(unlist(x)))
b20_phi99 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 0.99)), 1, function(x) mean(unlist(x)))
b30_phi99 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 0.99)), 1, function(x) mean(unlist(x)))

b0_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.01, phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b10_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b20_phi90 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 0.9)), 1, function(x) mean(unlist(x)))
b30_phi90 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 0.9)), 1, function(x) mean(unlist(x)))

b0_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.01)), 1, function(x) mean(unlist(x)))
b10_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1)), 1, function(x) mean(unlist(x)))
b20_phi80 <- plyr::adply(replicate(reps, sim_modclass()), 1, function(x) mean(unlist(x)))
b30_phi80 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3)), 1, function(x) mean(unlist(x)))

b0_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.01, phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b10_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.1, phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b20_phi70 <- plyr::adply(replicate(reps, sim_modclass(phi0 = 0.7)), 1, function(x) mean(unlist(x)))
b30_phi70 <- plyr::adply(replicate(reps, sim_modclass(b0 = 0.3, phi0 = 0.7)), 1, function(x) mean(unlist(x)))

names <- c("stat", "b=0.01", "b=0.1", "b=0.2", "b=0.3")
mod_class <- rbind(
  plyr::join_all(list(b0_phi99, b10_phi99, b20_phi99, b30_phi99), by = "X1") %>% set_names(names) %>% dplyr::mutate(phi = "0.99"),
  plyr::join_all(list(b0_phi90, b10_phi90, b20_phi90, b30_phi90), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.90"),
  plyr::join_all(list(b0_phi80, b10_phi80, b20_phi80, b30_phi80), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.80"),
  plyr::join_all(list(b0_phi70, b10_phi70, b20_phi70, b30_phi70), by = "X1") %>% set_names(names)  %>% dplyr::mutate(phi = "0.70"))
WriteXLS::WriteXLS(c("mod_class", "mod_class"), ".\\opplan\\2018upper\\modclass.xlsx", SheetNames = c("format", "export"), BoldHeaderRow = TRUE)

