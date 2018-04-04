nz <- 50000
CH.aug <- rbind(CH, matrix(0, ncol = dim(CH)[2], nrow = nz))

# Bundle data
jags.data <- list(y = CH.aug, n.occasions = dim(CH.aug)[2], M = dim(CH.aug)[1])

# Initial values
# Good initial values for the latent state z are needed to run the model in JAGS. The simplest option that works is to give just a matrix with a 1 at all places. Moreover, initial values for the inclusion parameter w should also be given, the simplest option is to provide a vector with 1's.
z.init <- CH.aug
z.init[z.init==0] <- 1
w.init <- rep(1, nrow(CH.aug))
inits <- function(){list(mean.phi = runif(1, .9, 1), mean.p = runif(1, 0, .1), psi = runif(1, 0, 1), w = w.init, z = z.init)}  

# Parameters monitored
parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu")

# MCMC settings
ni <- 10000
nt <- 1
nb <- 5000
nc <- 4

# Call JAGS from R (BRT 40 min)
js.super <- jagsUI::jags(jags.data, 
                         inits, 
                         parameters, 
                         ".\\scripts\\js_super.txt", 
                         n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,
                         parallel = TRUE)

print(js.super, digits = 3)