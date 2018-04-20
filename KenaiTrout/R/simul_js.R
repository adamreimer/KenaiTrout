#' A function to simulate open population capture-recapture data
#'
#' Copied from Kery and Schaub BPA pg. 329, modified so that CH is in MARK format
#'
#' @param PHI A n by k - 1 array of apparent survivals where n is population size and k is the number of events
#' @param P A n by k array of probabilities of capture where n is population size and k is the number of events
#' @param b A k length vector of entry probabilities where k is the number of events
#' @param N population size
#'
#' @return A list with a character vector of capture histories, a vector of new entrants and a vector of populations size. 
#'
#' @export
#Function to simulate capture-recapture data under the JS model (POPAN)
#Kery and Schaub BPA pg. 329, modifed so that CH is in MARK format
simul_js <- function(PHI, P, b, N){
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
  Nt <- colSums(CH.sur)    # Actual population size per event
  return(list(ch=ch, B=B, N=Nt))
}