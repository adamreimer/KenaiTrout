library(KenaiTrout)
library(RMark)
library(ggplot2)
CH <- 
  CH_UR18 %>%
  dplyr::mutate(lg_g = cut(lg, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE))
table(CH$lg_g, CH$recap)

# #Test closure assumption
# #process dataset
# dat_fl <- process.data(data.frame(ch = CH_UR18$ch, fl = CH_UR18$lg_g, stringsAsFactors = FALSE),
#                        model = "POPAN", 
#                        groups = "fl")
# 
# #create design data
# ddl_fl = make.design.data(dat_fl)
# 
# rt_models <- function(){
#   Phi.dot <- list(formula=~1, fixed = list(index = 1:10, value = 1))
#   Phi.fl <- list(formula=~fl)
#   Phi.t <- list(formula=~time)
#   Phi.flt <- list(formula=~fl*time)
# 
#   p.dot <- list(formula=~1)
#   p.fl <- list(formula=~fl)
#   p.t <- list(formula=~time)
#   p.flt <- list(formula=~fl*time)
#   
#   pent.dot <- list(formula=~1, fixed = list(index = 1:10, value = 0))
#   pent.t <- list(formula=~time)
#   pent.flt <- list(formula=~fl*time)
#   
#   N.fl <- list(formula=~fl)
#   
#   mod_list <- create.model.list("POPAN")
#   mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
#   mod_results
# }
# POPAN_mod_results <- rt_models()
# POPAN_mod_results$model.table
# #Closed population is a safe assumption, top three models are all fixed with Phi = 1 and pent = 0 or Phi is estimated very close to 1. 
#saveRDS(POPAN_mod_results, file = ".\\scripts\\2018upper\\POPAN_mod_results.rds")
# 
# #real estimates (models ordered by AIC)
# lapply(rownames(POPAN_mod_results$model.table), function(x) knitr::kable(POPAN_mod_results[[as.numeric(x)]]$results$real, digits = 3))
# 
# #drop models w AIC > ~2
# POPAN_drop_mod <- rownames(POPAN_mod_results$model.table)[POPAN_mod_results$model.table$DeltaAICc > 2]
# POPAN_mod_best <- remove.mark(mod_results, as.numeric(drop_mod))
# POPAN_mod_best$model.table
# lapply(rownames(POPAN_mod_best$model.table), function(x) knitr::kable(POPAN_mod_best[[as.numeric(x)]]$results$real, digits = 3))

#Closed models
#process dataset
# fill lg = NA
CH_UR18$lg[is.na(CH_UR18$lg)] <- mean(CH_UR18$lg, na.rm = TRUE)
closed_dat <- process.data(CH_UR18[, 2:3], model = "Huggins")
closed_ddl <- make.design.data(closed_dat)

closed_models <- function(){
  closed_ddl$p$time_g <- c(0, 0, 0, 0, 1, 1)
  closed_ddl$c$time_g <- c(0, 0, 0, 1, 1)
  
  # Define parameter models
  p.dotshared <- list(formula=~1, share=TRUE) #M0 
  p.timeshared <- list(formula=~time, share=TRUE) #Mt
  p.dot <- list(formula=~1 + c, share=TRUE) #Mb
  
  p.covtimeshare <- list(formula=~lg * time, share=TRUE)
  p.covtimeb <- list(formula=~lg * c + time, share=TRUE)
  p.covtimeb2 <- list(formula=~lg + c + time, share=TRUE)
  p.covtimeshare_g <- list(formula=~time + lg + lg:time_g, share=TRUE)
#  p.covb <- list(formula=~lg * c, share=TRUE)
#  p.covshare <- list(formula=~lg, share=TRUE)  

  mod_list <- create.model.list("Huggins")
  mod_results <- mark.wrapper(mod_list, data = closed_dat, ddl = closed_ddl)
  
  ptimemixtureshared <- list(formula=~lg * time + mixture, share=TRUE)
  pmixture <- list(formula=~mixture) #Mh
  #    Huggins heterogeneity models
  #  Mh2 - p different for mixture
  huggins.Mh2 <- mark(data = CH_UR18[2], model="HugHet", model.parameters = list(p = pmixture))
  #  Huggins Mth2 - p different for time; mixture additive
  closed_dat2 <- process.data(CH_UR18[, 2:3], model = "HugFullHet")
  huggins.Mth2.additive <- mark(data = closed_dat2, model.parameters = list(p = ptimemixtureshared), adjust=TRUE)
  
  merge.mark(mod_results, huggins.Mh2, huggins.Mth2.additive)
}
# fit models in mark by calling function created above
closed_results <- closed_models()
closed_results$model.table
lapply(rownames(closed_results$model.table), function(x) knitr::kable(closed_results[[as.numeric(x)]]$results$beta, digits = 3))
lapply(rownames(closed_results$model.table), function(x) knitr::kable(closed_results[[as.numeric(x)]]$results$real, digits = 3))
lapply(rownames(closed_results$model.table), function(x) knitr::kable(closed_results[[as.numeric(x)]]$results$derived, digits = 3))


#display index value for p coefficients
closed_ddl
plot_dat <- expand.grid(lg = seq(200,600,25), index = 1:6)
event_labs <- c("July 2-5", "July 9-11", "July 16-18", "July 23-25", "July 30-Aug. 1", "Aug 6-8")

covariate.predictions(closed_results[[3]], data = plot_dat, indices = c(1, 6))$estimates %>%
  dplyr::mutate(event = factor(index, labels = event_labs)) %>%
  ggplot(aes(x = lg, ymin = lcl, ymax = ucl)) +
    geom_ribbon(alpha = 0.25) +
    geom_line(aes(y = estimate)) +
    facet_grid(~event) +
    labs(y = "Probability of Capture", x = "Total Length")

knitr::kable(closed_results[[3]]$results$beta, digits = 3)
knitr::kable(closed_results[[3]]$results$real, digits = 3)
knitr::kable(closed_results[[3]]$results$derived, digits = 3)

read.delim(".\\KenaiTrout\\data-raw\\URlevel_18.txt", 
           header = FALSE, 
           col.names = c("agency", "site", "datetime", "zone", "height", "P"), 
           skip = 28) %>%
  dplyr::mutate(date = as.Date(datetime, "%Y-%m-%d %H:%M")) %>%
  dplyr::filter(date <= as.Date("2018-08-08")) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(height = mean(height)) %>%
  ggplot(aes(x = date, y = height)) +
    geom_line() +
    annotate("rect", xmin = as.Date("2018-07-02"), xmax = as.Date("2018-07-05"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2018-07-09"), xmax = as.Date("2018-07-11"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2018-07-16"), xmax = as.Date("2018-07-18"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2018-07-23"), xmax = as.Date("2018-07-25"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2018-07-30"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2018-08-06"), xmax = as.Date("2018-08-08"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  labs(y = "Gauge Height", x = "Date") +
  scale_x_date(date_breaks = "week", labels = function(x){format.Date(x, "%B-%d")})

#Capture History
tab1 <-
  CH_UR18["ch"] %>% 
  tidyr::separate(ch, into = paste0("e", 1:6), sep = 1:5) %>%
  dplyr::mutate_all(.funs = as.numeric) %>%
  dplyr::mutate(n1 = e1,
                n2 = ifelse(e1 == 0, e2, 0),
                n3 = ifelse(e1 + e2 == 0, e3, 0),
                n4 = ifelse(e1 + e2 + e3 == 0, e4, 0),
                n5 = ifelse(e1 + e2 + e3 + e4 == 0, e5, 0),
                n6 = ifelse(e1 + e2 + e3 + e4 + e5 == 0, e6, 0),
                r1 = 0,
                r2 = ifelse(e1 == 1, e2, 0),
                r3 = ifelse(e1 + e2 >= 1, e3, 0),
                r4 = ifelse(e1 + e2 + e3 >= 1, e4, 0),
                r5 = ifelse(e1 + e2 + e3 + e4 >= 1, e5, 0),
                r6 = ifelse(e1 + e2 + e3 + e4 + e5 >= 1, e6, 0),
                a1 = 0,
                a2 = e1 + n2,
                a3 = e1 + n2 + n3,
                a4 = e1 + n2 + n3 + n4,
                a5 = e1 + n2 + n3 + n4 + n5,
                a6 = e1 + n2 + n3 + n4 + n5 + n6) %>%
  tidyr::gather(name, count) %>%
  dplyr::mutate(stat = gsub("^(.)\\d", "\\1", name),
                event = gsub("^.(\\d)", "\\1", name)) %>%
  dplyr::group_by(stat, event) %>%
  dplyr::summarise(count = sum(count)) %>%
  tidyr::spread(stat, count) %>%
  dplyr::select(capture = e, new = n, recap = r, at_large = a) %>%
  dplyr::mutate(pcap = recap / capture,
                plarge = recap / at_large,
                event = 1:6)

tab1
ggplot(tab1, aes(x = event, y = pcap)) +
  geom_point()  +
  ggplot2::geom_smooth(method=lm, se=TRUE)

temp <-
  CH_UR18 %>% 
  tidyr::separate(ch, into = paste0("e", 1:6), sep = 1:5) %>%
  tidyr::separate(lh, into = paste0("l", 1:6), sep = 1:5) %>%
  dplyr::mutate_all(.funs = as.numeric) %>%
  dplyr::mutate(n1 = l1,
                n2 = ifelse(e1 == 0, l2, 0),
                n3 = ifelse(e1 + e2 == 0, l3, 0),
                n4 = ifelse(e1 + e2 + e3 == 0, l4, 0),
                n5 = ifelse(e1 + e2 + e3 + e4 == 0, l5, 0),
                n6 = ifelse(e1 + e2 + e3 + e4 + e5 == 0, l6, 0),
                r1 = 0,
                r2 = ifelse(e1 == 1 & e2 == 1, l2, 0),
                r3 = ifelse(e1 + e2 >= 1 & e3 == 1, l3, 0),
                r4 = ifelse(e1 + e2 + e3 >= 1 & e4 == 1, l4, 0),
                r5 = ifelse(e1 + e2 + e3 + e4 >= 1 & e5 == 1, l5, 0),
                r6 = ifelse(e1 + e2 + e3 + e4 + e5 >= 1 & e6 == 1, l6, 0)) %>%
  dplyr::select(tag, dplyr::starts_with("n"), dplyr::starts_with("r")) %>%
  tidyr::gather(event, loc, - tag) %>%
  dplyr::filter(loc != 0) %>%
  dplyr::arrange(tag) %>%
  dplyr::mutate(class = ifelse(grepl("n", event), "cap", "recap"))

tab4 <- 
  dplyr::group_by(temp, class, loc) %>%
  dplyr::summarise(n = n()) %>%
  tidyr::spread(class, n) %>%
  dplyr::mutate(p1 = recap / (cap + recap),
                p2 = recap / sum(recap),
                p3 = cap / sum(cap))
tab4
chisq.test(tab4[, c("cap", "recap")])

temp2 <-
  temp %>%
  dplyr::mutate(class2 = ifelse(class == "cap", class, event)) %>%
  dplyr::select(-event, -class) %>%
  dplyr::group_by(tag) %>%
  tidyr::spread(class2, loc)

tab5 <-
  sapply(1:3, function(x){
    dat <- temp2[temp2$cap == x, ]
    table(c(dat$r2, dat$r3, dat$r4, dat$r5, dat$r6))
  }) %>%
  as.data.frame() %>%
  dplyr::mutate(total = V1 + V2 + V3,
                out = total - diag(matrix(c(V1, V2, V3), 3, 3)),
                p = out / total)
tab5
sum(c(13, 23, 8)) / sum(c(131, 86, 48))

lg <- 
  CH_UR18 %>% 
  tidyr::separate(ch, into = paste0("e", 1:6), sep = 1:5) %>%
  dplyr::select(dplyr::starts_with("e"), lg) %>%
  dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("e")), .funs = dplyr::funs(as.numeric(.) * lg)) %>%
  dplyr::select(-lg) %>%
  tidyr::gather(event, lg) %>%
  dplyr::filter(lg != 0)

ggplot(lg, aes(x = lg, color = event)) +
  stat_ecdf()

kSamples::ad.test(lapply(paste0("e", 1:6), function(x){lg$lg[lg$event == x]}))

#Weighted length comp
alpha <- closed_results[[3]]$results$beta$estimate[grepl("p:\\(|p:t", rownames(closed_results[[3]]$results$beta))]
beta <- closed_results[[3]]$results$beta$estimate[grepl("p:lg", rownames(closed_results[[3]]$results$beta))] 

num <- 
  CH_UR18 %>% 
  dplyr::mutate(event = regexpr("1", ch),
                c_ik = ifelse(event == 1,
                              exp(alpha[1] + beta[1] * lg)/(1 + exp(alpha[1] + beta[1] * lg)),
                              exp(alpha[1] + alpha[event] + beta[1] * lg + beta[event] * lg) /
                                 (1+ exp(alpha[1] + alpha[event] + beta[1] * lg + beta[event] * lg))),
                lg_bin = cut(lg, breaks = seq(200, 600, 50), right = FALSE)) %>%
  dplyr::group_by(event, lg_bin) %>%
  dplyr::summarize(num_p = sum(1 / c_ik),
                   n_ij = n())
dem <- 
  num %>% 
  dplyr::summarise(dem_p = sum(num_p),
                   n_i = sum(n_ij))

n <- sum(dem$n_i)

dplyr::left_join(num, dem, by = "event") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_ij = num_p/dem_p,
                w_i = n_i / n) %>%
  dplyr::group_by(lg_bin) %>%
  dplyr::summarise(n_j = sum(n_ij),
                   pj_raw = n_j / n,
                   se_pj_raw = sqrt(pj_raw * (1 - pj_raw) / (n - 1)),
                   p_j = sum(w_i * p_ij),
                   se_pj = se_pj_raw,
                   N_j = p_j * closed_results[[3]]$results$derived$'N Population Size'$estimate,
                   se_N_j = sqrt(closed_results[[3]]$results$derived$'N Population Size'$estimate^2 * se_pj^2 +
                                   p_j^2 * closed_results[[3]]$results$derived$'N Population Size'$se^2 -
                                   se_pj^2 * closed_results[[3]]$results$derived$'N Population Size'$se^2))

