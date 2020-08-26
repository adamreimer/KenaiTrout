load(".\\data\\dat_17.rda")
temp_17 <- aggregate(dat_17$temp, list(dat_17$event), mean) %>% setNames(c("event", "temp"))

temp_18 <- 
  readxl::read_excel(".\\data-raw\\2018 Middle River trout water temp.xlsx",
                    range = "A3:D1213",
                    col_names = c("date", "skip", "skip", "temp"),
                    col_types = c("date", "skip", "skip", "numeric")) %>%
  dplyr::filter(date >= as.Date("2018-05-04") & date <= as.Date("2018-06-08")) %>%
  dplyr::mutate(event = factor(format(date, "%W"), labels = as.character(7:12))) %>%
  dplyr::select(event, temp) %>%
  dplyr::group_by(event) %>%
  dplyr::summarise(temp = mean(temp))

MRtemp <- rbind(temp_17, temp_18)
saveRDS(MRtemp, ".\\data\\MRtemp.rds")
