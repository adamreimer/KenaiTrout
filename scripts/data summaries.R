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

ggplot(CH, aes(x = fl)) +
  geom_histogram() +
  facet_wrap(~ mat, ncol = 1, scales = "free_y") +
  labs(title = "Fork Length vrs. Maturity")

ggplot(data, aes(x = fl)) +
  geom_histogram() +
  facet_wrap(~ recap, ncol = 1, scales = "free_y") +
  scale_x_continuous(name = "Fork Length(mm)", breaks = seq(200, 700, 25)) +
  labs(title = "Fork Length by recapture status")
length(data$fl[data$recap ==  TRUE])

#recapture rate by event
data %>% 
  dplyr::group_by(event) %>%
  dplyr::summarize(captures = n(),
                   new = sum(recap == FALSE),
                   recaps = sum(recap == TRUE)) %>%
  dplyr::mutate(atlarge = cumsum(ifelse(is.na(dplyr::lag(new)), 0, dplyr::lag(new))),
                'recap/cap' = recaps / captures,
                'recap/atlarge' = recaps / atlarge)

#recapture rate by 100mm length group
CH %>% 
  dplyr::group_by(fl_100) %>%
  dplyr::summarize(captures = n(),
                   recaps = sum(recap == TRUE)) %>%
  dplyr::mutate('recap/cap' = recaps / captures)

#recapture rate by 400mm boundary
CH %>% 
  dplyr::group_by(fl_g) %>%
  dplyr::summarize(captures = n(),
                   recaps = sum(recap == TRUE)) %>%
  dplyr::mutate('recap/cap' = recaps / captures)

#maturity by 400mm boundary
CH %>% 
  dplyr::group_by(fl_g) %>%
  dplyr::summarize(captures = n(),
                   mat = sum(mat == TRUE)) %>%
  dplyr::mutate(proportion = mat / captures)

#recapture rate by rm
data %>% 
  dplyr::group_by(rm) %>%
  dplyr::summarize(captures = n(),
                   recaps = sum(recap == TRUE)) %>%
  dplyr::mutate(proportion = recaps / captures)

#recapture rate by rm and week
data %>% 
  dplyr::group_by(rm, event) %>%
  dplyr::summarize(captures = n(),
                   recaps = sum(recap == TRUE)) %>%
  dplyr::mutate(proportion = recaps / captures)

#tag movement
tag_loc <- dplyr::left_join(data[data$recap == TRUE, c("tag", "rm")] %>% setNames(c("tag", "recap_loc")), 
                            data[data$recap == FALSE, c("tag", "rm", "event")] %>% setNames(c("tag", "tag_loc", "event")), 
                            by = "tag")
table(tag_loc$tag_loc, tag_loc$recap_loc)
table(tag_loc$tag_loc, tag_loc$recap_loc, tag_loc$event) 


ggplot(data, aes(x = fl, color = event)) +
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "Fork Length(mm)", breaks = seq(200, 700, 25)) +
  labs(title = "Fork Length by event")

ggplot(data, aes(x = fl, color = as.character(rm))) +
  stat_ecdf(size = 1) + 
  scale_x_continuous(name = "Fork Length(mm)", breaks = seq(200, 700, 25)) +
  labs(title = "Fork Length by rivermile")

ggplot(data, aes(x = fl, color = event)) +
  stat_ecdf(size = 1) + 
  facet_wrap(~ rm, ncol = 1) +
  scale_x_continuous(name = "Fork Length(mm)", breaks = seq(200, 700, 25)) +
  labs(title = "Fork Length by event and rivermile")

ggplot(data, aes(x = fl, color = as.character(rm))) +
  stat_ecdf(size = 1) + 
  facet_wrap(~ event, ncol = 1) +
  scale_x_continuous(name = "Fork Length(mm)", breaks = seq(200, 700, 25)) +
  labs(title = "Fork Length by event and rivermile")
