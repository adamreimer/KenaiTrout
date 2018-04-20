level_17 <- 
  readr::read_tsv(".\\KenaiTrout\\data-raw\\level_2017.txt", 
                  col_names = c("agency", "site", "date", "tz", "height", "A"), 
                  col_types = "ccccnc", 
                  skip = 29) %>%
  dplyr::mutate(time = factor(format(as.Date(date), "%W"),labels = as.character(1:6))) %>%
  dplyr::select(time, height) %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(level = mean(height))
devtools::use_data(level_17, pkg = ".\\KenaiTrout", overwrite = TRUE)
