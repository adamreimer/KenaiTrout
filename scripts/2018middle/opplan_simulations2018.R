rm(list=ls())
require(RMark)

####Function to fit n_sim_reps open population models and report average Nhat, reletive precision and sample size
sample_sim2 <- function(n_sim_reps,
                        b = list(c(.05, .13, .13, .11, .25, .33), 
                                 c(.13, .17, .17, .17, .18, .18)),
                        phi = list(rep(0.95, 5), #c(.95, .95, .92, .86, .83), 
                                   c(.99, .99, .99, .99, .99)),
                        p = list(rep(.015, 6), #c(.023, .021, .017, .014, .013, .012), 
                                 rep(.03, 6)), #c(.035, .031, .026, .022, .020, .019)), 
                        sim_N= list(30000, 15000)){
  events <- length(b[[1]])
  PHI <- mapply(function(x, y) matrix(rep(x, times = y), ncol = events - 1, nrow = y, byrow = TRUE), x = phi, y = sim_N) #Matrix of Survival probabilitie
  P <- mapply(function(x, y) matrix(rep(x, times = y), ncol = events, nrow = y, byrow = TRUE), x = p, y = sim_N) #Matrix of Capture probabilities
  fltime <- list(formula=~fl*time)
  fl <- list(formula=~fl)
  
  #set up an empty data frame to store all the simulation results
  output.data<-data.frame(events=numeric(0),rep=numeric(0),N.hat=numeric(0))
  for(r in 1:n_sim_reps){
    small <- simul.js(PHI = PHI[[1]], P = P[[1]], b = b[[1]], N = sim_N[[1]])$ch
    large <- simul.js(PHI = PHI[[2]], P = P[[2]], b = b[[2]], N = sim_N[[2]])$ch
    capt.hist <- list(ch = as.character(unlist(rbind(small, large))),
                      fl = factor(c(rep("small", dim(small)[1]), rep("large", dim(large)[1]))))
    dat <- process.data(data.frame(ch = capt.hist$ch, fl = capt.hist$fl, stringsAsFactors = FALSE), model = "POPAN", groups = "fl")
    des <- make.design.data(dat)
    m0 <- mark(dat, ddl = des, model.parameters=list(Phi = fl, p = fl, pent = fltime, N = fl), output=F, silent=T)
    #pull off parameters you want
    M <- length(m0$data$freq)
    N.hat <- unlist(m0$results$real$estimate[grep("^N", rownames(m0$results$real))])
    seN.hat <- unlist(m0$results$real$se[grep("^N", rownames(m0$results$real))])
    rpN.hat <- seN.hat * 1.96 / N.hat
    n <- plyr::adply(capt.hist$ch, 1, function(x) substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1))) %>%
      dplyr::select(dplyr::starts_with("V")) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::summarise_all(sum)
    #put in data frame
    new.data<-data.frame(rep = r, 
                         N.sm = N.hat[2],
                         N.lg = N.hat[1],
                         se.N.sm = seN.hat[2],
                         se.N.lg = seN.hat[1], 
                         rpN.sm = rpN.hat[2],
                         rpN.lg = rpN.hat[1],
                         n = sum(n[,1:6]))
    #append to output data
    output.data<-rbind(output.data,new.data)
  }
  #create  summary statistics
  list(N.sm = mean(output.data$N.sm),
       N.lg = mean(output.data$N.lg),
       rp.sm = mean(output.data$rpN.sm),
       rp.lg = mean(output.data$rpN.lg),
       p.sm = mean(p[[1]]),
       p.lg = mean(p[[2]]),
       n.ave = mean(output.data$n))
}

#relative precision at several populations and probabilities of capture with immigration
sim_N <- list(list(20000, 10000),
              list(30000, 15000),
              list(40000, 20000))  # Superpopulation size
sim_p = list(list(rep(.0166, 6), rep(.0255, 6)),
             list(rep(.02, 6), rep(.0255, 6)),
             list(rep(.0255, 6), rep(.0255, 6)),
             list(rep(.03, 6), rep(.03, 6)))
reps <- 30
sims18 <- sapply(rlist::list.expand(sim_N, sim_p), function(x) sample_sim2(reps, p = x[[2]], sim_N = x[[1]]))
#saveRDS(sims18, file = ".\\opplan\\sims18.rds")

sims18 <- readRDS(file = ".\\opplan\\sims18.rds")
colnames(sims18) <- NULL #rep(c("small:20K/large:10K", "small:30K/large:15K", "small:40K/large:20K"), times = 4)
sims18 <- as.data.frame(sims18)
sims18$group <- gsub(".*\\.(.*)", "\\1", rownames(sims18))
small <- 
  sims18[sims18$group == "sm", ] %>%
  dplyr::mutate(Nhat = )
large <- sims18[sims18$group == "lg", ]

temp <-
  data.frame(lapply(data.frame(t(sims18)), unlist), stringsAsFactors=FALSE) %>%
  dplyr::mutate(#"Nhat(small/large)" = paste0(format(round(N.sm), big.mark = ","), "/", format(round(N.lg), big.mark = ",")),
                rp = paste0(format(round(rp.sm, 2), nsmall = 2), "/", format(round(rp.lg, 2), nsmall = 2)),
                pcap = paste0(format(round(p.sm, 3), nsmall = 3), "/", format(round(p.lg, 3), nsmall = 3)),
                abundance = rep(c("20K/10K", "30K/15K", "40K/20K"), times = 4),
                marks = format(round(n.ave), big.mark =",")) %>%
  dplyr::select(rp, pcap, abundance, marks)

rp <- temp[, c("rp", "pcap", "abundance")] %>% tidyr::spread(pcap, rp) %>% dplyr::mutate(metric = "Relative Precision")
marks <- temp[, c("marks", "pcap", "abundance")] %>% tidyr::spread(pcap, marks) %>% dplyr::mutate(metric = "Trout marked")
table18 <- rbind(rp, marks) %>% dplyr::arrange(abundance, metric)
WriteXLS::WriteXLS(as.data.frame(table18), ExcelFileName = "H:\\My Documents\\Kenai Trout\\opplan\\2018sample sizes_open.xlsx", row.names = TRUE, AdjWidth = TRUE)
