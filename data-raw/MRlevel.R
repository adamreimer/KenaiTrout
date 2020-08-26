level_17 <- 
  readr::read_tsv(".\\data-raw\\level_2017.txt", 
                  col_names = c("agency", "site", "date", "tz", "height", "A"), 
                  col_types = "ccccnc", 
                  skip = 29) %>%
  dplyr::mutate(event = factor(format(as.Date(date), "%W"),labels = as.character(1:6))) %>%
  dplyr::select(event, height) %>%
  dplyr::group_by(event) %>%
  dplyr::summarise(level = mean(height))

level_18 <- 
  readr::read_tsv(".\\data-raw\\MRlevel_2018.txt", 
                  col_names = c("agency", "site", "date", "tz", "height", "A", "flow", "A2"), 
                  col_types = "ccccncnc", 
                  skip = 32) %>%
  dplyr::mutate(event = factor(format(as.Date(date), "%W"), labels = as.character(7:12))) %>%
  dplyr::select(event, height) %>%
  dplyr::group_by(event) %>%
  dplyr::summarise(level = mean(height))

MRlevel <- rbind(level_17, level_18)
saveRDS(MRlevel, ".\\data\\MRlevel.rds")
