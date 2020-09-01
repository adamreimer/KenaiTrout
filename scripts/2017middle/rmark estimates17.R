CH_17 <- readRDS(".\\data\\CH_17.rds")
dat_17 <- readRDS(".\\data\\dat_17.rds")

#Look for patterns in recpature rates
#Capture History table
tab_ch <-
  CH_17[CH_17$fl >= 400, "ch"] %>% 
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
  dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
  tidyr::spread(stat, count) %>%
  dplyr::select(capture = e, new = n, recap = r, at_large = a) %>%
  dplyr::mutate(pcap = recap / capture,
                plarge = recap / at_large) %>%
  setNames(c("Captured", "New tags", "Recaptures", "At large", "Recaptures / Captures", "Recaptures / At Large"))
knitr::kable(t(tab_ch), col.names = paste0("Event ", 1:6), digits = 3)

#Recapture rate by length group
tab_lg <-
  dat_17 %>%
  dplyr::filter(!is.na(fl)) %>%
  dplyr::mutate(class = ifelse(recap == 0, "cap", "recap"),
                lg_group = cut(fl, breaks = c(0, 199, 399, 900))) %>% #breaks = c(0, 199, 349, 499, 900))) %>% #breaks = c(0, 199, 299, 399, 499, 900))) %>% 
  dplyr::group_by(class, lg_group) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::spread(class, n) %>%
  dplyr::mutate(total = (cap + recap),
                p1 = recap / total)
knitr::kable(tab_lg, col.names = c("Length group (mm)", "New tags", "Recaptures", "Total", "Proportion Recaptured"))
chisq.test(tab_lg[, c("recap", "cap")])
chisq.test(tab_lg[2:3, c("recap", "cap")])


#add fl, maturity and temperature at capture to CH
mean(CH_17$fl, na.rm = TRUE)
CH <- CH_17 %>%
  dplyr::mutate(fl = ifelse(is.na(fl), 412, fl),  #fill NA, fix in data?
                fl_100 = cut(fl, breaks = seq(200, 800, 100)),
                fl_g = cut(fl, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE)) %>%
  dplyr::left_join(dat_17[dat_17$recap != TRUE, c("tag", "mat")], by = "tag") %>%
  dplyr::left_join(dat_17[dat_17$recap != TRUE, c("tag", "rm")], by = "tag") %>%
  dplyr::mutate(rm45 = ifelse(rm == 45, 1, 0),
                rm46 = ifelse(rm == 46, 1, 0), 
                rm47 = ifelse(rm == 47, 1, 0))
table(CH$fl_g, CH$recap)

#temperature and water level
temp <- readRDS(".\\data\\MRtemp.rds") %>% dplyr::filter(event %in% 1:6) %>% dplyr::select(-event)
temp$time <- 1:6

level <- readRDS(".\\data\\MRlevel.rds") %>% dplyr::filter(event %in% 1:6) %>% dplyr::select(-event)
level$time <- 1:6

library(RMark)
#process dataset
dat_fl <- process.data(data.frame(ch = CH$ch, fl = CH$fl_g, rm46 = CH$rm46, rm47 = CH$rm47, stringsAsFactors = FALSE),
                              model = "POPAN", 
                              groups = "fl")

#create design data
ddl_fl = make.design.data(dat_fl)
ddl_fl$Phi = merge_design.covariates(ddl_fl$Phi, temp)
ddl_fl$p = merge_design.covariates(ddl_fl$p, temp)
ddl_fl$pent = merge_design.covariates(ddl_fl$pent, temp)
ddl_fl$Phi = merge_design.covariates(ddl_fl$Phi, level)
ddl_fl$p = merge_design.covariates(ddl_fl$p, level)
ddl_fl$pent = merge_design.covariates(ddl_fl$pent, level)

rt_models <- function(){
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  Phi.fl <- list(formula=~fl)
  Phi.level <- list(formula=~level)
  Phi.fllevel <- list(formula=~fl*level)

  p.dot <- list(formula=~1)
  p.time <- list(formula=~time)
  p.fl <- list(formula=~fl)
  p.level <- list(formula=~level)
  p.fllevel <- list(formula=~fl*level)
  
  pent.dot <- list(formula=~1)
  pent.time <- list(formula=~time)
  pent.fltemp <- list(formula=~fl*temp)
  pent.flTime <- list(formula=~fl*Time)
  
  N.fl <- list(formula=~fl)
  
  mod_list <- create.model.list("POPAN")
  mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
  mod_results
}
mod_results <- rt_models()
mod_results$model.table[, -5]

#drop models w AIC > ~2
mod_best <- remove.mark(mod_results, as.numeric(rownames(mod_results$model.table))[mod_results$model.table$DeltaAICc > 2.75])
mod_best$model.table[, -5]
lapply(rownames(mod_best$model.table), function(x) knitr::kable(mod_best[[as.numeric(x)]]$results$real, digits = 3))
lapply(rownames(mod_best$model.table), function(x) knitr::kable(mod_best[[as.numeric(x)]]$results$beta, digits = 3))
#saveRDS(mod_best, file = ".\\scripts\\2017middle\\mod_bestMR17both.rds")

ranks <- data.frame(rank = row(mod_best$model.table)[, 1], 
                    mod_n = rownames(mod_best$model.table),
                    mod_name = mod_best$model.table[, "model"], #gsub("(.*)pent.*", "\\1", mod_best$model.table[, "model"]),
                    weight = mod_best$model.table$weight)

ave_Phi <- 
  model.average(mod_best, "Phi", drop = FALSE) %>%
  dplyr::select(estimate, se, group, time) %>%
  dplyr::mutate(model = "Model average",
                lcl = estimate - 1.96 * se,
                ucl = ifelse(estimate + 1.96 * se > 1, 1, estimate + 1.96 * se)) %>%
  dplyr::mutate(model = factor(0, levels = 0:dim(ranks)[1], labels = c("Model average", as.character(ranks$mod_name)))) 

ave_p <- 
  model.average(mod_best, "p", vcv = TRUE, drop = FALSE)[[1]] %>%
  dplyr::select(estimate, lcl, ucl , group, time) %>%
  dplyr::mutate(model = "Model average")

ave_pent <- 
  model.average(mod_best, "pent", vcv = TRUE, drop = FALSE)[[1]] %>%
  dplyr::select(estimate, lcl, ucl , group, time) %>%
  dplyr::mutate(model = "Model average")

est_N <- 
  lapply(rownames(mod_best$model.table), function(x){
    get.real(mod_best[[as.numeric(x)]], "N", se = TRUE) %>%
      dplyr::mutate(model = gsub("(.*)pent.*", "\\1", mod_best$model.table[x, "model"])) %>%
      dplyr::select(model, group, time, estimate, se)}) %>%
  do.call(rbind, .)

#model.average.marklist does not give correct parameter average for N
#model average.list
ave_N <- 
  data.frame(model = "Model average", 
             small = unlist(model.average(list(estimate=est_N[est_N$group =="small", "estimate"], 
                                               weight=mod_best$model.table$weight, 
                                               se=est_N[est_N$group =="small", "se"]), mata = TRUE)),
             large = unlist(model.average(list(estimate=est_N[est_N$group =="large", "estimate"], 
                                               weight=mod_best$model.table$weight, 
                                               se=est_N[est_N$group =="large", "se"]), mata = TRUE))) %>%
  tibble::rownames_to_column() %>%
  tidyr::gather(key = group, param, -rowname, -model) %>%
  tidyr::spread(rowname, param) %>%
  dplyr::bind_rows(replicate(nrow(mod_best$model.table) + 1, ., simplify = FALSE)) %>%
  dplyr::mutate(mod_n = rep(0:(nrow(mod_best$model.table) + 1), each = 2))

# #manual calculation
# sum(est_N[est_N$group =="small", "estimate"] * mod_best$model.table$weight)
# sum(est_N[est_N$group =="large", "estimate"] * mod_best$model.table$weight)
# #model.average.marklist
# model.average(mod_best, "N")
# 
# #OK for Phi
# est_Phi <-
#   lapply(rownames(mod_best$model.table)[1:5], function(x){
#     get.real(mod_best[[as.numeric(x)]], "Phi", se = TRUE) %>%
#       dplyr::mutate(model = gsub("(.*)pent.*", "\\1", mod_best$model.table[x, "model"])) %>%
#       dplyr::select(model, group, time, estimate, se)}) %>%
#   do.call(rbind, .)
# #model.average.list
# model.average(list(estimate=est_Phi[est_Phi$group =="small" & est_Phi$time == "5", "estimate"],
#                    weight=mod_best$model.table$weight,
#                    se=est_Phi[est_Phi$group =="small" & est_Phi$time == "5", "se"]))
# #model.average.marklist does not give correct parameter average for N
# #manual calculation
# sum(est_Phi[est_Phi$group =="small" & est_Phi$time == "5", "estimate"] * mod_best$model.table$weight)
library(ggplot2)
plot_params <- function(param, ave, label, ylim = NULL){
  lapply(rownames(mod_best$model.table), function(x){
    get.real(mod_best[[as.numeric(x)]], param, se = TRUE) %>%
      dplyr::mutate(mod_n = x) %>%
      dplyr::select(mod_n, group, time, estimate, se)}) %>%
    do.call(rbind, .) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rank = ranks$rank[ranks$mod_n == mod_n],
                  model = factor(rank,
                                 levels = 0:dim(ranks)[1],
                                 labels = c("Model average", as.character(ranks$mod_name)))) %>%
    ggplot(aes(x = as.numeric(time), y = estimate, color = model)) +
    geom_line() +
    geom_line(data = ave, size = 1.25) +
    geom_ribbon(data = ave, aes(x = as.numeric(time), ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
    scale_y_continuous(limits = ylim) +
    facet_grid(.~group) +
    labs(title = paste0(label, " by length group"), x = "week", y = label)
}
plot_params("Phi", ave_Phi, "Apparent Survival")
plot_params("p", ave_p, "Probability of Capture", ylim = c(0, .15))
plot_params("pent", ave_pent, "Probability of entrance") 

lapply(rownames(mod_best$model.table), function(x){
  get.real(mod_best[[as.numeric(x)]], "N", se = TRUE) %>%
    dplyr::mutate(mod_n = x) %>%
    dplyr::select(mod_n, group, time, estimate, se, lcl, ucl)}) %>%
  do.call(rbind, .) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(rank = ranks$rank[ranks$mod_n == mod_n],
                model = factor(rank,
                               levels = 0:dim(ranks)[1],
                               labels = c("Model average", as.character(ranks$mod_name))),
                model2 = ifelse(grepl("p\\(~time.*", ranks$mod_name[ranks$mod_n == mod_n]), "time", "no time"))  %>%
  ggplot(aes(x = rank, y = estimate, ymin = lcl, ymax = ucl, color = model)) +
  geom_crossbar(width = 0.5) +
  geom_line(data = ave_N, aes(x = mod_n, y = estimate), size = 1.25, inherit.aes = FALSE) +
  geom_ribbon(data = ave_N, aes(x = mod_n, ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(.~group) +
  labs(title = "Abundance", x = "model rank", y = "N")

#N per time strata
est_Nt <- 
  lapply(rownames(mod_best$model.table), function(x){
    mod_best[[as.numeric(x)]]$results$derived$`N Population Size` %>%
      dplyr::mutate(model = mod_best$model.table[x, "model"], #gsub("(.*)pent.*", "\\1", mod_best$model.table[x, "model"]),
                    group = rep(c("small", "large"), each = 6), #dim(mod_best[[as.numeric(x)]]$results$derived$`N Population Size`)[1]/2),
                    time = rep(1:6, 2)) %>%
      dplyr::select(model, group, time, estimate, se)}) %>%
  do.call(rbind, .)

aveNt <- function(group, time){
  est <- est_Nt[est_Nt$group == group & est_Nt$time == time, ]
  model.average(
    list(estimate = est[, "estimate"], 
         weight = mod_best$model.table$weight, 
         se = est[, "se"]), 
    mata = TRUE)}
temp <- cbind(sapply(1:6, function(x) aveNt("small", x)), sapply(1:6, function(x) aveNt("large", x)))
plot_aveNt <- data.frame(
  model = "Model average",
  group = rep(c("small", "large"), each = 6), 
  time = rep(1:6, times = 2),
  estimate = unlist(temp[rownames(temp) == "estimate", ]),
  lcl = unlist(temp[rownames(temp) == "lcl", ]),
  ucl = unlist(temp[rownames(temp) == "ucl", ])
)
ggplot(est_Nt, aes(x = time, y = estimate, color = model)) +
  geom_line() +
  geom_line(data = plot_aveNt, aes(x = time, y = estimate), size = 1.25) +
  geom_ribbon(data = plot_aveNt, aes(x = time, ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(. ~ group)

