library(KenaiTrout)
library(RMark)
CH <- 
  CH_UR18 %>%
  dplyr::mutate(lg_g = cut(lg, breaks = c(0, 400, 900), labels = c("small", "large")),
                recap = ifelse(nchar(gsub("0", "", ch)) >= 2, TRUE, FALSE))
table(CH$lg_g, CH$recap)

#process dataset
dat_fl <- process.data(data.frame(ch = CH_UR18$ch, fl = CH$lg_g, stringsAsFactors = FALSE),
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
saveRDS(POPAN_mod_results, file = ".\\scripts\\2018upper\\POPAN_mod_results.rds")
# 
# #real estimates (models ordered by AIC)
# lapply(rownames(POPAN_mod_results$model.table), function(x) knitr::kable(POPAN_mod_results[[as.numeric(x)]]$results$real, digits = 3))
# 
# #drop models w AIC > ~2
# POPAN_drop_mod <- rownames(POPAN_mod_results$model.table)[POPAN_mod_results$model.table$DeltaAICc > 2]
# POPAN_mod_best <- remove.mark(mod_results, as.numeric(drop_mod))
# POPAN_mod_best$model.table
# lapply(rownames(POPAN_mod_best$model.table), function(x) knitr::kable(POPAN_mod_best[[as.numeric(x)]]$results$real, digits = 3))

