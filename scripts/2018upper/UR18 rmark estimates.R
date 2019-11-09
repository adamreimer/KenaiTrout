library(RMark)
library(magrittr)
library(ggplot2)
# get/prep data
CH_UR18 <- readRDS(".\\data\\CH_UR18.rds")
CH_UR18$lg[is.na(CH_UR18$lg)] <- mean(CH_UR18$lg, na.rm = TRUE)

#Look for patterns in recpature rates
#Capture History table
tab_ch <-
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
                a2 = e1,
                a3 = e1 + n2,
                a4 = e1 + n2 + n3,
                a5 = e1 + n2 + n3 + n4,
                a6 = e1 + n2 + n3 + n4 + n5) %>%
  tidyr::gather(name, count) %>%
  dplyr::mutate(stat = gsub("^(.)\\d", "\\1", name),
                event = gsub("^.(\\d)", "\\1", name)) %>%
  dplyr::group_by(stat, event) %>%
  dplyr::summarise(count = sum(count)) %>%
  tidyr::spread(stat, count) %>%
  dplyr::select(capture = e, new = n, recap = r, at_large = a) %>%
  dplyr::mutate(pcap = recap / capture,
                plarge = recap / at_large) %>%
  setNames(c("Captured", "New tags", "Recaptures", "At large", "Recaptures / Captures", "Recaptures / At Large"))
knitr::kable(t(tab_ch), col.names = paste0("Event ", 1:6), digits = 3)

#increasing recapture rate implies closure
ggplot(data.frame(event = 1:6, pcap = tab_ch$'Recaptures / Captures'), aes(x = event, y = pcap)) +
  geom_point()  +
  ggplot2::geom_smooth(method=lm, se=TRUE)

#Gauge height vrs event
read.delim(".\\data-raw\\URlevel_18.txt", 
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

#Location differences
dat_UR18 <- readRDS(".\\data\\dat_UR18.rds")
temp0 <-
  dplyr::mutate(dat_UR18, 
                recap = ifelse(recap == 0, "cap", "recap"),
                area = cut(loc, breaks = c(0, 5.5, 10.5, 17), labels = FALSE)) %>% 
  dplyr::mutate(recap2 = ifelse(recap == "cap", recap, paste0("r", week))) %>%
  dplyr::select(-week, -recap) %>%
  dplyr::group_by(tag) 
temp1 <- 
  temp0 %>%
  dplyr::select(-loc) %>%
  tidyr::spread(recap2, area)

#Movement by fishing area
tab_movearea <-
  lapply(1:3, function(x){
    dat <- temp1[temp1$cap == x, ]
    table(factor(c(dat$r2, dat$r3, dat$r4, dat$r5, dat$r6), levels = 1:3))
  }) %>% 
  do.call(rbind, .) %>%
  as.data.frame()
tab_movearea$total = apply(tab_movearea, 1, sum)
tab_movearea$out = tab_movearea$total - diag(as.matrix(tab_movearea[, 1:3]))
tab_movearea$p = tab_movearea$out / tab_movearea$total


#Recapture rate by location
tab_loc <- 
  data.frame(area = 1:3,
             new = as.vector(table(temp1$cap)),
             recaps = tab_movearea$total) %>%
  dplyr::mutate(total = new + recaps,
                p_recap = recaps / total,
                p_of_recaps = recaps / sum(recaps),
                p_of_caps = total / sum(total))
knitr::kable(tab_loc, 
             col.names = c("River Section", "New tags", "Recaptures", "Total", "Proportion Recaptured", "Proportion of all Recaptures", "Proportion of all Captures"))
chisq.test(tab_loc[, c("recaps", "new")])

#Movement table
rownames(tab_movearea) <- paste0("Captured in ", 1:3)
knitr::kable(tab_movearea, col.names = c(paste0("Recap in ", 1:3), "Total recaptures", "# out", "Proportion out"))

#Movement by fishing hole
temp2 <- 
  temp0 %>%
  dplyr::select(-area) %>%
  tidyr::spread(recap2, loc)
tab_moveloc <-
  lapply(1:16, function(x){
    dat <- temp2[temp2$cap == x, ]
    table(factor(c(dat$r2, dat$r3, dat$r4, dat$r5, dat$r6), levels = 1:16))
  }) %>% 
  do.call(rbind, .) %>%
  as.data.frame()
tab_moveloc$total = apply(tab_moveloc, 1, sum)
tab_moveloc$out = tab_moveloc$total - diag(as.matrix(tab_moveloc[, 1:16]))
tab_moveloc$p = tab_moveloc$out / tab_moveloc$total
rownames(tab_moveloc) <- paste0("Captured in ", 1:16)
knitr::kable(tab_moveloc, col.names = c(1:16, "Total recaptures", "# out", "Proportion out"))


#weak evidence of a trend in recapture rate by length group
#biggest differences associated with small fish
tab_lg <-
  dat_UR18 %>%
  dplyr::filter(!is.na(lg)) %>%
  dplyr::mutate(class = ifelse(recap == 0, "cap", "recap"),
                lg_group = cut(lg, breaks = c(0, 199, 299, 399, 499, 900))) %>% #cut(lg, breaks = c(0, 199, 249, 299, 349, 399, 449, 499, 900))) %>%
  dplyr::group_by(class, lg_group) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::spread(class, n) %>%
  dplyr::mutate(total = (cap + recap),
                p1 = recap / total)
knitr::kable(tab_lg, col.names = c("Length group (mm)", "New tags", "Recaptures", "Total", "Proportion Recaptured"))
chisq.test(tab_lg[, c("recap", "cap")])

#Size comp changes by week and area
lg_e <- 
  CH_UR18 %>% 
  tidyr::separate(ch, into = paste0("e", 1:6), sep = 1:5) %>%
  dplyr::select(dplyr::starts_with("e"), lg) %>%
  dplyr::mutate_at(.vars = dplyr::vars(dplyr::starts_with("e")), .funs = list(~ as.numeric(.) * lg)) %>%
  dplyr::select(-lg) %>%
  tidyr::gather(event, lg) %>%
  dplyr::filter(lg != 0)
ggplot(lg_e, aes(x = lg, color = event)) +
  stat_ecdf() +
  ylab("Cumulative Proportion") +
  scale_x_continuous(name = "Fork Length (mm)", breaks = seq(200, 600, 50))
kSamples::ad.test(lapply(paste0("e", 1:6), function(x){lg_e$lg[lg_e$event == x]}))

#Size comp does not change by area
lg_loc <- 
  CH_UR18 %>% 
  tidyr::separate(lh, into = paste0("e", 1:6), sep = 1:5) %>%
  dplyr::select(dplyr::starts_with("e"), lg) %>%
  dplyr::select(-lg) %>%
  tidyr::gather(event, loc) %>%
  dplyr::filter(loc != 0) %>%
  dplyr::select(-event) %>%
  cbind(lg_e)
loc_plot1 <- 
  ggplot(lg_loc, aes(x = lg, color = loc)) +
  stat_ecdf()
loc_plot1
kSamples::ad.test(lapply(1:3, function(x){lg_loc$lg[lg_loc$loc == x]}))
loc_plot1 + facet_grid(event~.)

##### Abundance estimation
#format data
loc = tidyr::separate(CH_UR18["lh"], lh, into = paste0("l", 1:6), sep = 1:5) %>% apply(1, function(x) min(as.numeric(x[x != 0])))
lg_g = cut(CH_UR18$lg, breaks = c(0, 299, 399, 900), labels = c("small", "medium", "large"))
CH <- 
  CH_UR18 %>%
  dplyr::mutate(lg_g = lg_g,
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE),
                loc2 = as.numeric(loc == 2),
                loc3 = as.numeric(loc == 3),
                lg2 = as.numeric(lg_g == 2),
                lg3 = as.numeric(lg_g == 3))


###Test closure assumption
#process dataset
dat_fl <- process.data(data.frame(ch = CH$ch, fl = CH$lg_g, stringsAsFactors = FALSE),
                       model = "POPAN",
                       groups = "fl")
#create design data
ddl_fl = make.design.data(dat_fl)
open_mods <- function(){
  Phi.one <- list(formula=~1, fixed = list(index = 1:15, value = 1))
  Phi.dot <- list(formula=~1)

  p.flt <- list(formula=~fl*time)

  pent.end <- list(formula=~fl, fixed = list(index = c(1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14), value = rep(0, 12)))
  pent.one <- list(formula=~1, fixed = list(index = 1:15, value = 0))
  pent.dot <- list(formula=~1)
  pent.t <- list(formula=~time)
  pent.flt <- list(formula=~fl*time)

  N.fl <- list(formula=~fl)

  mod_list <- create.model.list("POPAN")
  mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
  mod_results
}
POPAN_results <- open_mods()
saveRDS(POPAN_results, ".\\scripts\\2018upper\\POPAN_results.rds")
POPAN_results$model.table
cleanup(ask = FALSE)

#drop models w AIC > ~3
#Closed population is a safe assumption
#top three models (~90% of AIC weight) are all fixed with Phi = 1 and pent = 0 or Phi estimated very close to 1 and pent estimated close to zero.
POPAN_drop <- rownames(POPAN_results$model.table)[POPAN_results$model.table$DeltaAICc > 3.5]
POPAN_best <- remove.mark(POPAN_results, as.numeric(POPAN_drop))
lapply(rownames(POPAN_best$model.table), function(x) knitr::kable(POPAN_best[[as.numeric(x)]]$results$real, digits = 3))
### end closure test


### Estimate abundance with closed model
#process dataset
closed_dat <- process.data(CH[, c(2, 3, 7, 8, 9, 10)], model = "Huggins")
#create design data
closed_ddl <- make.design.data(closed_dat)

### Fit basic models
closed_mods1 <- function(dat, ddl, dat_het){
  # Define parameter models
  p.M0 <- list(formula=~1, share=TRUE)
  p.Mt <- list(formula=~time, share=TRUE)
  p.Mb <- list(formula=~1 + c, share=TRUE)
  p.Mh.lg <- list(formula=~lg, share=TRUE)
  p.Mh.lgg <- list(formula=~lg2 + lg3, share=TRUE)
  p.Mh.loc <- list(formula=~loc2 + loc3, share=TRUE)
  
  mod_list <- create.model.list("Huggins")
  mod_results <- mark.wrapper(mod_list, data = dat, ddl = ddl)
  
  #Mh.mix
  p.Mh.mix <- list(formula=~mixture)
  huggins.Mh.mix <- mark(data = dat_het, model="HugHet", model.parameters = list(p = p.Mh.mix))
  
  merge.mark(mod_results, huggins.Mh.mix)
}
closed_results1 <- closed_mods1(dat = closed_dat, ddl = closed_ddl, dat_het = CH_UR18[2])
saveRDS(closed_results1, ".\\scripts\\2018upper\\closed_results1.rds")
closed_results1$model.table
#-Mt best model by >30DeltaAIC
bestmod1 <- as.numeric(rownames(closed_results1$model.table)[1])
knitr::kable(closed_results1[[bestmod1]]$results$beta, digits = 3)
knitr::kable(closed_results1[[bestmod1]]$results$real, digits = 3)
knitr::kable(closed_results1[[bestmod1]]$results$derived, digits = 3)
cleanup(ask = FALSE)


### Fit model drivations of Mt
closed_mods2 <- function(dat, ddl){
  # Define parameter models
  p.Mtb <- list(formula=~time + c, share=TRUE)
  p.Mth.lg <- list(formula=~time*lg, share=TRUE)
  p.Mth.lg2 <- list(formula=~time + lg, share=TRUE)
  p.Mtbh.lg <- list(formula=~time*lg + c, share=TRUE)
  
  mod_list <- create.model.list("Huggins")
  mod_results <- mark.wrapper(mod_list, data = dat, ddl = ddl)
}
#Combined Models -Mth.lg best identifiable model (models with behavior non-sensical (c insignificant)
closed_results2 <- closed_mods2(dat = closed_dat, ddl = closed_ddl)
saveRDS(closed_results2, ".\\scripts\\2018upper\\closed_results2.rds")
closed_results2$model.table
bestmod2 <- as.numeric(rownames(closed_results2$model.table)[1:2])
knitr::kable(closed_results2[[bestmod2[1]]]$results$beta, digits = 3)
knitr::kable(closed_results2[[bestmod2[1]]]$results$real, digits = 3)
knitr::kable(closed_results2[[bestmod2[1]]]$results$derived, digits = 3)
# Behavior effect nonsensical
knitr::kable(closed_results2[[bestmod2[2]]]$results$beta, digits = 3)
knitr::kable(closed_results2[[bestmod2[2]]]$results$derived, digits = 3)
cleanup(ask = FALSE)


### Fit model drivations of Mt.lg
closed_ddl
closed_ddl$p$time113446 <- as.factor(c(1, 1, 3, 4, 4, 6)); closed_ddl$c$time113446 <- as.factor(c(1, 1, 3, 4, 4));
closed_ddl$p$time111456 <- as.factor(c(1, 1, 1, 4, 5, 6)); closed_ddl$c$time111456 <- as.factor(c(1, 1, 1, 4, 5));
closed_ddl$p$time111444 <- as.factor(c(1, 1, 1, 4, 4, 4)); closed_ddl$c$time111444 <- as.factor(c(1, 1, 1, 4, 4));
closed_ddl
### Fit models drivations of Mt
closed_mods3 <- function(dat, ddl){
  # Define parameter models
  p.Mth.lg <- list(formula=~time*lg, share=TRUE)
  p.Mth.lg113446 <- list(formula=~time113446*lg, share=TRUE)
  p.Mth.lg111456 <- list(formula=~time111456*lg, share=TRUE)
  p.Mth.lg111444 <- list(formula=~time111444*lg, share=TRUE)
  
  mod_list <- create.model.list("Huggins")
  mod_results <- mark.wrapper(mod_list, data = dat, ddl = ddl)
}
# No improvement from grouping events
closed_results3 <- closed_mods3(dat = closed_dat, ddl = closed_ddl)
saveRDS(closed_results3, ".\\scripts\\2018upper\\closed_results3.rds")
closed_results3$model.table


##### The same analsis with censored data leads to similar N
CH_UR18_censor <- readRDS(".\\data\\CH_UR18_censor.rds")
loc_censor = tidyr::separate(CH_UR18_censor["lh"], lh, into = paste0("l", 1:5), sep = 1:4) %>% apply(1, function(x) min(as.numeric(x[x != 0])))
CH_UR18_censor$lg[is.na(CH_UR18_censor$lg)] <- mean(CH_UR18_censor$lg, na.rm = TRUE)
lg_g_censor = cut(CH_UR18_censor$lg, breaks = c(0, 299, 399, 900), labels = c("small", "medium", "large"))
CH_censor <- 
  CH_UR18_censor %>%
  dplyr::mutate(lg_g = lg_g_censor,
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE),
                loc2 = as.numeric(loc_censor == 2),
                loc3 = as.numeric(loc_censor == 3),
                lg2 = as.numeric(lg_g_censor == 2),
                lg3 = as.numeric(lg_g_censor == 3))
closed_dat_censor <- process.data(CH_censor[, c(2, 3, 7, 8, 9, 10)], model = "Huggins")
closed_ddl_censor <- make.design.data(closed_dat_censor)
### Fit basic models
closed_results1_censor <- closed_mods1(dat = closed_dat_censor, ddl = closed_ddl_censor, dat_het = CH_UR18_censor[2])
closed_results1_censor$model.table
closed_results2_censor <- closed_mods2(dat = closed_dat_censor, ddl = closed_ddl_censor)
closed_results2_censor
saveRDS(closed_results2_censor, ".\\scripts\\2018upper\\closed_results2_censor.rds")
knitr::kable(closed_results2_censor[[3]]$results$beta, digits = 3)
knitr::kable(closed_results2_censor[[3]]$results$derived, digits = 3)
#####


###### display results from best model
knitr::kable(closed_results2[[bestmod2[1]]]$results$beta, digits = 3)
knitr::kable(closed_results2[[bestmod2[1]]]$results$real, digits = 3)
knitr::kable(closed_results2[[bestmod2[1]]]$results$derived, digits = 3)

# P(cap) by time and event
closed_ddl
plot_dat <-  expand.grid(lg = seq(200,600,25), index = 1:6)
event_labs <- c("July 2-5", "July 9-11", "July 16-18", "July 23-25", "July 30-Aug. 1", "Aug 6-8")
covariate.predictions(closed_results2[[bestmod2[1]]], data = plot_dat, indices = c(1, 6))$estimates %>%
  dplyr::mutate(event = factor(index, labels = event_labs)) %>%
  ggplot(aes(x = lg, ymin = lcl, ymax = ucl)) +
    geom_ribbon(alpha = 0.25, linetype = 0) +
    geom_line(aes(y = estimate)) +
    facet_grid(.~event) +
    labs(y = "Probability of Capture", x = "Total Length")

#Weighted length comp
alpha <- closed_results2[[bestmod2[1]]]$results$beta$estimate[grepl("p:\\(|p:time\\d$", rownames(closed_results2[[bestmod2[1]]]$results$beta))]
beta <- closed_results2[[bestmod2[1]]]$results$beta$estimate[grepl("p:lg|p:time\\d:lg", rownames(closed_results2[[bestmod2[1]]]$results$beta))] 
num <- 
  CH_UR18 %>% 
  dplyr::mutate(event = regexpr("1", ch),
                c_ik = ifelse(event == 1,
                              exp(alpha[1] + beta[1] * lg)/(1 + exp(alpha[1] + beta[1] * lg)),
                              exp(alpha[1] + alpha[event] + beta[1] * lg + beta[event] * lg) /
                                 (1+ exp(alpha[1] + alpha[event] + beta[1] * lg + beta[event]))),
                lg_bin = cut(lg, breaks = seq(200, 600, 50), right = FALSE)) %>%
  dplyr::group_by(event, lg_bin) %>%
  dplyr::summarize(num_p = sum(1 / c_ik), n_ij = dplyr::n())
dem <- 
  num %>% 
  dplyr::summarise(dem_p = sum(num_p),
                   n_i = sum(n_ij))
temp_age <- 
  dplyr::left_join(num, dem, by = "event") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_ij = num_p/dem_p,
                w_i = n_i / sum(dem$n_i),
                lg_bin2 = ifelse(lg_bin %in% c("[200,250)", "[250,300)"), "[200,300)", "[300,600)"))

temp_age %>%
  dplyr::group_by(lg_bin) %>%
  dplyr::summarise(n_j = sum(n_ij),
                   pj_raw = n_j / sum(dem$n_i),
                   se_pj_raw = sqrt(pj_raw * (1 - pj_raw) / (sum(dem$n_i) - 1)),
                   p_j = sum(w_i * p_ij),
                   se_pj = se_pj_raw,
                   N_j = p_j * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$estimate,
                   se_N_j = sqrt(closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$estimate^2 * se_pj^2 +
                                   p_j^2 * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$se^2 -
                                   se_pj^2 * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$se^2))

#again for >300mm
temp_age %>%
  dplyr::group_by(lg_bin2) %>%
  dplyr::summarise(n_j = sum(n_ij),
                   pj_raw = n_j / sum(dem$n_i),
                   se_pj_raw = sqrt(pj_raw * (1 - pj_raw) / (sum(dem$n_i) - 1)),
                   p_j = sum(w_i * p_ij),
                   se_pj = se_pj_raw,
                   N_j = p_j * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$estimate,
                   se_N_j = sqrt(closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$estimate^2 * se_pj^2 +
                                   p_j^2 * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$se^2 -
                                   se_pj^2 * closed_results2[[bestmod2[1]]]$results$derived$'N Population Size'$se^2))

