CH_MR <- readRDS(".\\data\\CH_MR.rds")

#add fl, maturity and temperature at capture to CH
mean(CH_MR$fl, na.rm = TRUE)
CH <- CH_MR %>%
  dplyr::mutate(fl = ifelse(is.na(fl), 412, fl),  #fill NA, fix in data?
                fl_100 = cut(fl, breaks = seq(200, 800, 100)),
                fl_g = cut(fl, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE))
# %>%
#   dplyr::left_join(dat_17[dat_17$recap != TRUE, c("tag", "mat")], by = "tag") %>%
#   dplyr::left_join(dat_17[dat_17$recap != TRUE, c("tag", "rm")], by = "tag") %>%
#   dplyr::mutate(rm45 = ifelse(rm == 45, 1, 0),
#                 rm46 = ifelse(rm == 46, 1, 0), 
#                 rm47 = ifelse(rm == 47, 1, 0))
table(CH$fl_g, CH$recap)

#temperature and water level
temp <- readRDS(".\\data\\MRtemp.rds") %>% dplyr::select(-event)
#temp$time <- round(cumsum(c(1, c(rep(1/52, 5), 47/52, rep(1/52, 5)))), 2)
temp$time <- cumsum(c(1, 1, 1, 1, 1, 1, 47, 1, 1, 1, 1, 1))


level <- readRDS(".\\data\\MRlevel.rds") %>% dplyr::select(-event)
#level$time <- round(cumsum(c(1, c(rep(1/52, 5), 47/52, rep(1/52, 5)))), 2)
level$time <- cumsum(c(1, 1, 1, 1, 1, 1, 47, 1, 1, 1, 1, 1))

library(RMark)
#process dataset
dat_fl <- process.data(data.frame(ch = CH$ch, fl = CH$fl_g, stringsAsFactors = FALSE), #rm46 = CH$rm46, rm47 = CH$rm47,
                       model = "POPAN", 
                       groups = "fl",
                       time.intervals = c(1, 1, 1, 1, 1, 47, 1, 1, 1, 1, 1))

#create design data
ddl_fl = make.design.data(dat_fl)
#add temp
ddl_fl$Phi = merge_design.covariates(ddl_fl$Phi, temp)
ddl_fl$p = merge_design.covariates(ddl_fl$p, temp)
ddl_fl$pent = merge_design.covariates(ddl_fl$pent, temp)
#add water level
ddl_fl$Phi = merge_design.covariates(ddl_fl$Phi, level)
ddl_fl$p = merge_design.covariates(ddl_fl$p, level)
ddl_fl$pent = merge_design.covariates(ddl_fl$pent, level)
#season = 1 for between years
ddl_fl$Phi$season = 0
ddl_fl$Phi$season[ddl_fl$Phi$time == 6] <- 1
#year
ddl_fl$p$year = 0
ddl_fl$p$year[ddl_fl$p$Time >= 5] = 1 
ddl_fl$Phi$year = 0
ddl_fl$Phi$year[ddl_fl$Phi$Time >= 5] = 1
ddl_fl$pent$year = 0
ddl_fl$pent$year[ddl_fl$pent$Time > 5] = 1
# ddl_fl$p$year = 0
# ddl_fl$p$year[ddl_fl$p$Time >= 1] = 1 
# ddl_fl$Phi$year = 0
# ddl_fl$Phi$year[ddl_fl$Phi$Time >= 1] = 1
#week
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(1, 53)] = "1"
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(2, 54)] = "2"
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(3, 55)] = "3"
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(4, 56)] = "4"
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(5, 57)] = "5"
ddl_fl$Phi$week[ddl_fl$Phi$time %in% c(6)] = "6"
ddl_fl$p$week[ddl_fl$p$time %in% c(1, 53)] = "1"
ddl_fl$p$week[ddl_fl$p$time %in% c(2, 54)] = "2"
ddl_fl$p$week[ddl_fl$p$time %in% c(3, 55)] = "3"
ddl_fl$p$week[ddl_fl$p$time %in% c(4, 56)] = "4"
ddl_fl$p$week[ddl_fl$p$time %in% c(5, 57)] = "5"
ddl_fl$p$week[ddl_fl$p$time %in% c(6, 58)] = "6"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(1, 53)] = "1"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(2, 54)] = "2"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(3, 55)] = "3"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(4, 56)] = "4"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(5, 57)] = "5"
ddl_fl$pent$week[ddl_fl$pent$time %in% c(6, 58)] = "6"
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(1, 53)] = 1
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(2, 54)] = 2
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(3, 55)] = 3
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(4, 56)] = 4
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(5, 57)] = 5
ddl_fl$Phi$Week[ddl_fl$Phi$time %in% c(6)] = 6
ddl_fl$p$Week[ddl_fl$p$time %in% c(1, 53)] = 1
ddl_fl$p$Week[ddl_fl$p$time %in% c(2, 54)] = 2
ddl_fl$p$Week[ddl_fl$p$time %in% c(3, 55)] = 3
ddl_fl$p$Week[ddl_fl$p$time %in% c(4, 56)] = 4
ddl_fl$p$Week[ddl_fl$p$time %in% c(5, 57)] = 5
ddl_fl$p$Week[ddl_fl$p$time %in% c(6, 58)] = 6
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(1, 53)] = 1
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(2, 54)] = 2
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(3, 55)] = 3
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(4, 56)] = 4
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(5, 57)] = 5
ddl_fl$pent$Week[ddl_fl$pent$time %in% c(6, 58)] = 6
#effort
ddl_fl$p$e <- rep(c(8, 8, 8, 8, 8, 8, 2, 9, 10, 10, 8, 10), 2)


rt_models <- function(){
  Phi.dot <- list(formula=~1)
  Phi.fl <- list(formula=~fl)
  Phi.time <- list(formula=~time)
#  Phi.year <- list(formula=~year)
#  Phi.yearfl <- list(formula=~year + fl)
  Phi.season <- list(formula=~season)
#  Phi.flseason <- list(formula=~fl * season)
#  Phi.flseawk <- list(formula=~fl * season * week)
  

#  p.dot <- list(formula=~1)
  p.time <- list(formula=~time)
#  p.year <- list(formula=~year)
  p.week <- list(formula=~week)
#  p.fl <- list(formula=~fl)
#  p.level <- list(formula=~level)
  p.e <- list(formula=~e)
  p.efl <- list(formula=~e * fl)
  p.efly <- list(formula=~e * fl * year)
#  p.weekfl <- list(formula=~week * fl)
  
#  pent.dot <- list(formula=~1)
#  pent.fl <- list(formula=~fl)
  pent.time <- list(formula=~time)
#  pent.temp <- list(formula=~temp)
#  pent.flweek <- list(formula=~fl * week)
#  pent.week <- list(formula=~week)
  pent.flwy <- list(formula=~fl * week * year)
  pent.flWy <- list(formula=~fl * Week * year)
  
  
  N.fl <- list(formula=~fl)
  
  mod_list <- create.model.list("POPAN")
  mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
  mod_results
}
mod_results <- rt_models()
mod_results$model.table[, -5]

#drop models w AIC > ~2
mod_best <- remove.mark(mod_results, as.numeric(rownames(mod_results$model.table))[mod_results$model.table$DeltaAICc > 2])
mod_best$model.table[, -c(1, 2, 3, 4)]
lapply(rownames(mod_best$model.table), function(x) knitr::kable(mod_best[[as.numeric(x)]]$results$real, digits = 3))
lapply(rownames(mod_best$model.table), function(x) knitr::kable(mod_best[[as.numeric(x)]]$results$beta, digits = 3))
lapply(rownames(mod_best$model.table), function(x) mod_best[[as.numeric(x)]]$results$derived)

#saveRDS(mod_best, file = ".\\scripts\\mod_best.rds")
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
plot_params <- function(param, ave, label){
  lapply(rownames(mod_best$model.table), function(x){
    get.real(mod_best[[as.numeric(x)]], param, se = TRUE) %>%
      dplyr::mutate(mod_n = x) %>%
      dplyr::select(mod_n, group, time, estimate, se)}) %>%
    do.call(rbind, .) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rank = ranks$rank[ranks$mod_n == mod_n],
                  model = factor(rank,
                                 levels = 0:dim(ranks)[1],
                                 labels = c("Model average", as.character(ranks$mod_name))))%>%
    ggplot(aes(x = as.numeric(time), y = estimate, color = model)) +
    geom_line() +
    geom_line(data = ave, size = 1.25) +
    geom_ribbon(data = ave, aes(x = as.numeric(time), ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
    facet_grid(. ~ group) +
    labs(title = paste0(label, " by length group"), x = "week", y = label)
}
plot_params("Phi", ave_Phi, "Apparent Survival")
plot_params("p", ave_p, "Probability of Capture")
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
                               labels = c("Model average", as.character(ranks$mod_name)))) %>%
  ggplot(aes(x = mod_n, y = estimate, ymin = lcl, ymax = ucl, color = model)) +
    geom_crossbar(width = 0.5) +
    geom_line(data = ave_N, aes(x = mod_n, y = estimate), size = 1.25, inherit.aes = FALSE) +
    geom_ribbon(data = ave_N, aes(x = mod_n, ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
    facet_grid(.~group) +
    labs(title = "Abundance", x = "model", y = "N")

#N per time strata
est_Nt <- 
  lapply(rownames(mod_best$model.table), function(x){
    mod_best[[as.numeric(x)]]$results$derived$`N Population Size` %>%
      dplyr::mutate(model = mod_best$model.table[x, "model"], #gsub("(.*)pent.*", "\\1", mod_best$model.table[x, "model"]),
                    group = rep(c(rep("small", 6), rep("large", 6)), each = 2), #dim(mod_best[[as.numeric(x)]]$results$derived$`N Population Size`)[1]/2),
                    time = rep(1:12, 2)) %>%
      dplyr::select(model, group, time, estimate, se)}) %>%
  do.call(rbind, .)

aveNt <- function(group, time){
  est <- est_Nt[est_Nt$group == group & est_Nt$time == time, ]
  model.average(
    list(estimate = est[, "estimate"], 
         weight = mod_best$model.table$weight, 
         se = est[, "se"]), 
    mata = TRUE)}
temp <- cbind(sapply(1:12, function(x) aveNt("small", x)), sapply(1:12, function(x) aveNt("large", x)))
plot_aveNt <- data.frame(
  model = "Model average",
  group = rep(c("small", "large"), each = 12), 
  time = rep(1:12, times = 2),
  estimate = unlist(temp[rownames(temp) == "estimate", ]),
  lcl = unlist(temp[rownames(temp) == "lcl", ]),
  ucl = unlist(temp[rownames(temp) == "ucl", ])
)
ggplot(est_Nt, aes(x = time, y = estimate, color = model)) +
  geom_line() +
  geom_line(data = plot_aveNt, aes(x = time, y = estimate), size = 1.25) +
  geom_ribbon(data = plot_aveNt, aes(x = time, ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
  facet_grid(. ~ group)

