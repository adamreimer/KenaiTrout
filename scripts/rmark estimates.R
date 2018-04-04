library(ggplot2)
library(magrittr)

data <- readRDS(".\\data\\dat_17")
#add fl, maturity and temperature at capture to CH
CH <- readRDS(".\\data\\CH_17") %>%
  dplyr::left_join(data[data$recap != TRUE, c("tag", "fl")], by = "tag") %>%
  dplyr::mutate(fl = ifelse(is.na(fl), 401, fl),  #fill NA, fix in data?
                fl_100 = cut(fl, breaks = seq(200, 800, 100)),
                fl_g = cut(fl, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE)) %>%
  dplyr::left_join(data[data$recap != TRUE, c("tag", "mat")], by = "tag") %>%
  dplyr::left_join(data[data$recap != TRUE, c("tag", "rm")], by = "tag") %>%
  dplyr::mutate(rm45 = ifelse(rm == 45, 1, 0),
                rm46 = ifelse(rm == 46, 1, 0), 
                rm47 = ifelse(rm == 47, 1, 0))
table(CH$fl_g, CH$recap)

#temperature by event data
temp <- aggregate(data$temp, list(data$event), mean) %>% setNames(c("time", "temp"))

level <- 
  readr::read_tsv(".\\data\\flow2017.txt", 
                  col_names = c("agency", "site", "date", "tz", "height", "A"), 
                  col_types = "ccccnc", 
                  skip = 29) %>%
  dplyr::mutate(time = factor(format(as.Date(date), "%W"),labels = as.character(1:6))) %>%
  dplyr::select(time, height) %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(level = mean(height))

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
  Phi.fl <- list(formula=~fl)
  Phi.level <- list(formula=~level)
  Phi.fllevel <- list(formula=~fl*level)

  p.level <- list(formula=~level)
  p.fllevel <- list(formula=~fl*level)
  
  pent.fltemp <- list(formula=~fl*temp)
  
  N.fl <- list(formula=~fl)
  
  mod_list <- create.model.list("POPAN")
  mod_results <- mark.wrapper(mod_list, data = dat_fl, ddl = ddl_fl)
  mod_results
}
mod_results <- rt_models()
mod_results$model.table

#real estiamtes (models ordered by AIC)
lapply(rownames(mod_results$model.table), function(x) knitr::kable(mod_results[[as.numeric(x)]]$results$real, digits = 3))

#drop models w AIC > ~2
mod_best <- remove.mark(mod_results, as.numeric(rownames(mod_results$model.table)[6:8]))
#saveRDS(mod_best, file = ".\\scripts\\mod_best.rds")
ranks <- data.frame(rank = row(mod_best$model.table)[, 1], 
                    mod_n = rownames(mod_best$model.table),
                    mod_name = gsub("(.*)pent.*", "\\1", mod_best$model.table[, "model"]), 
                    weight = mod_best$model.table$weight)

ave_Phi <- 
  model.average(mod_best, "Phi", drop = FALSE) %>%
  dplyr::select(estimate, se, group, time) %>%
  dplyr::mutate(model = "Model average",
                lcl = estimate - 1.96 * se,
                ucl = ifelse(estimate + 1.96 * se > 1, 1, estimate + 1.96 * se)) %>%
  dplyr::mutate(model = factor(0, levels = 0:5, labels = c("Model average", as.character(ranks$mod_name)))) 

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
  dplyr::mutate(mod_n = rep(0:6, each = 2))

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

plot_params <- function(param, ave, label){
  lapply(rownames(mod_best$model.table), function(x){
    get.real(mod_best[[as.numeric(x)]], param, se = TRUE) %>%
      dplyr::mutate(mod_n = x) %>%
      dplyr::select(mod_n, group, time, estimate, se)}) %>%
    do.call(rbind, .) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rank = ranks$rank[ranks$mod_n == mod_n],
                  model = factor(rank,
                                 levels = 0:5,
                                 labels = c("Model average", as.character(ranks$mod_name)))) %>%
    ggplot(aes(x = as.numeric(time), y = estimate, color = model)) +
    geom_line() +
    geom_line(data = ave, size = 1.25) +
    geom_ribbon(data = ave, aes(x = as.numeric(time), ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
    facet_grid(.~group) +
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
                               levels = 0:5,
                               labels = c("Model average", as.character(ranks$mod_name)))) %>%
  ggplot(aes(x = mod_n, y = estimate, ymin = lcl, ymax = ucl, color = model)) +
    geom_crossbar(width = 0.5) +
    geom_line(data = ave_N, aes(x = mod_n, y = estimate), size = 1.25, inherit.aes = FALSE) +
    geom_ribbon(data = ave_N, aes(x = mod_n, ymin = lcl, ymax = ucl), size = 1.25, alpha = 0.2, inherit.aes = FALSE) +
    facet_grid(.~group) +
    labs(title = "Abundance", x = "model", y = "N")
