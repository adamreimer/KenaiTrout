library(KenaiTrout)
library(RMark)
library(ggplot2)
CH <- 
  CH_UR18 %>%
  dplyr::mutate(lg_g = cut(lg, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE))
table(CH$lg_g, CH$recap)

#process dataset
dat_fl <- process.data(data.frame(ch = CH_UR18$ch, fl = CH_UR18$lg_g, stringsAsFactors = FALSE),
                       model = "POPAN", 
                       groups = "fl")

#create design data
ddl_fl = make.design.data(dat_fl)

rt_models <- function(){
  Phi.dot <- list(formula=~1, fixed = list(index = 1:10, value = 1))
  Phi.fl <- list(formula=~fl)
  Phi.t <- list(formula=~time)
  Phi.flt <- list(formula=~fl*time)

  p.dot <- list(formula=~1)
  p.fl <- list(formula=~fl)
  p.t <- list(formula=~time)
  p.flt <- list(formula=~fl*time)
  
  pent.dot <- list(formula=~1, fixed = list(index = 1:10, value = 0))
  pent.t <- list(formula=~time)
  pent.flt <- list(formula=~fl*time)
  
  N.fl <- list(formula=~fl)
  
  mod_list <- create.model.list("POPAN")
  mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
  mod_results
}
POPAN_mod_results <- rt_models()
POPAN_mod_results$model.table
#Closed population is a safe assumption, top three models are all fixed with Phi = 1 and pent = 0 or Phi is estimated very close to 1. 
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

covariate.predictions(closed_results[[3]], data = plot_dat, indices = c(1, 6))$estimates %>%
  ggplot(aes(x = lg, ymin = lcl, ymax = ucl)) +
    geom_ribbon(alpha = 0.25) +
    geom_line(aes(y = estimate)) +
    facet_grid(~as.factor(index))

knitr::kable(closed_results[[3]]$results$beta, digits = 3)
knitr::kable(closed_results[[3]]$results$real, digits = 3)
knitr::kable(closed_results[[3]]$results$derived, digits = 3)

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

