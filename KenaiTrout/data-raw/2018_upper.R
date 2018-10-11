library(magrittr)
library(ggplot2)

dat_raw <- 
  readxl::read_excel(".\\KenaiTrout\\data-raw\\Copy of UPRRBT18_to Adam_round 2.xlsx",
                     sheet = "UPRRBT18",
                     col_names = c("date0", "boat", "crew", "fishnum", "loc", "tag", "recap", "lg", "ad", "fate0", "comment1", "comment2", 
                                   "week", "comment3"),
                     col_types = c(rep("numeric", 2), "text", rep("numeric", 3), "logical", "numeric", "logical", "numeric", rep("text", 2),
                                   "numeric", "text"),
                     skip = 1) %>%
  dplyr::arrange(date0, boat, fishnum)

dat_raw
#Check each variable and correct as needed
table(dat_raw$date0, useNA = "ifany")
  dat_raw$date <- as.Date(as.character(paste0("0", dat_raw$date0)), "%m%d%y")
  table(dat_raw$date, dat_raw$week, useNA = "ifany")
    dat_raw$week[is.na(dat_raw$week)] <- 2
    table(dat_raw$date, dat_raw$week, useNA = "ifany") #OK
table(dat_raw$boat, useNA = "ifany")
table(dat_raw$crew, useNA = "ifany")
plot(dat_raw$fishnum)
table(dat_raw$loc, useNA = "ifany")
table(dat_raw$tag == 0, useNA = "ifany") #60 tag=0
  dat_raw[dat_raw$tag == 0, ] %>% print(n = 100) # all tag  0 are fate = 0, delete (60)
  dat_raw <- dat_raw[dat_raw$tag != 0, ] #2729 records before, 2669 after
  dat_raw[is.na(dat_raw$tag), ] #OK
  table(dat_raw$tag == 0, useNA = "ifany") #OK 
table(dat_raw$recap, useNA = "ifany")
  dat_raw[is.na(dat_raw$recap), ]
  dat_raw[dat_raw$tag == 7429, ] #change to recap == 0
  dat_raw$recap[dat_raw$tag == 7429] <- 0
  dat_raw[is.na(dat_raw$recap), ] #OK
hist(dat_raw$lg)
  table(dat_raw$lg[dat_raw$lg <= 150], useNA = "ifany") #some fl = 0, one NA
  dat_raw$lg[dat_raw$lg == 0] <- NA
  hist(dat_raw$lg) #OK
table(dat_raw$ad, useNA = "ifany")
table(dat_raw$fate0, useNA = "ifany")
  dat_raw$fate <- plyr::mapvalues(dat_raw$fate0, from = c(0, 1), to = c("censor", "rel"))
  table(dat_raw$fate, useNA = "ifany")
dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] %>% print(n = 100) # save the recaps, delete the rest (14)
  dat_raw <- dat_raw[!(dat_raw$fate == "censor" & dat_raw$recap == 0), ] #2669 records before, 2655 after
  dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] #tag 5537 not a recap for this study
  dat_raw$recap[dat_raw$tag == 5537] <- 0
  dat_raw <- dat_raw[!(dat_raw$fate == "censor" & dat_raw$recap == 0), ] # now delete (capture mort w no prior history) #2655 records before, 2654 after
  dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] #OK
table(dat_raw$comment1)
table(dat_raw$comment2)
table(dat_raw$comment3)
table(dat_raw$comment1, dat_raw$comment2)
table(dat_raw$comment1, dat_raw$comment3)
comment0 <- paste0(ifelse(!is.na(dat_raw$comment1), paste0(dat_raw$comment1, ifelse(is.na(dat_raw$comment2) & is.na(dat_raw$comment3), "", ", ")), ""), 
                   ifelse(!is.na(dat_raw$comment2), paste0(dat_raw$comment2, ifelse(is.na(dat_raw$comment3), "", ", ")), ""),
                   ifelse(is.na(dat_raw$comment3), "", dat_raw$comment3))
dat_raw$comment = ifelse(comment0 == "", NA, comment0)
  table(dat_raw$comment, dat_raw$fate)
  dat_raw[dat_raw$comment %in% "FORGOT AD CLIP", ] #OK, recap crew noticed capture crew tag but forgot to adclip
    dat_raw[dat_raw$tag == 2756, ] 
    dat_raw[dat_raw$date == "2018-07-17" & dat_raw$boat == 2 & dat_raw$fishnum %in% 66:72, ]
    dat_raw[dat_raw$date == "2018-07-31" & dat_raw$boat == 1 & dat_raw$fishnum %in% 43:49, ]
    dat_raw[dat_raw$date == "2018-08-08" & dat_raw$boat == 1 & dat_raw$fishnum %in% 43:49, ]
  dat_raw[grepl("LOST", dat_raw$comment),]
    dat_raw[dat_raw$tag == 7418, ] #delete, tag# uncertain at recap and large lg difference
    dat_raw <- dat_raw[!(dat_raw$tag == 7418 & dat_raw$week == 6), ] #2654 records before, 2653 after
  dat_raw[dat_raw$comment %in% "MAY LIVE?",]
    dat_raw[dat_raw$tag == 2607, ] #OK, possible mort after recap
  dat_raw[grepl("VOID", dat_raw$comment), ] #OK, voided tags were not used
    dat_raw[dat_raw$tag == 2861, ]
    dat_raw[dat_raw$tag== 3860, ]
    dat_raw[dat_raw$tag %in% 3227:3233, ] #void 3231?
    dat_raw[dat_raw$tag == 3231, ]
    dat_raw[dat_raw$tag == 7213, ]

#Look for fish captured twice in the same week
same_event <-
  dat_raw %>%
  dplyr::group_by(tag, week) %>%
  dplyr::summarise(recap = max(recap), num = n()) %>%
  dplyr::filter(recap == 1 & num > 1)
same_event
  dat_raw[dat_raw$tag == 3093, ]
  dat_raw[dat_raw$date0 == 71118 & dat_raw$fishnum %in% 45:51 & dat_raw$boat == 2, ] #delete second record
  dat_raw <- dat_raw[!(dat_raw$tag == 3093 & dat_raw$recap == 1), ] #2653 records before, 2652 after

#Find and correct tags recorded more than once but without a recap flag
dup_norecap <-
  dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = sum(recap), lg_range = range(lg)[2] - range(lg)[1]) %>%
  dplyr::filter(recap < (n - 1) & n > 1)
dup_norecap #OK
# #duplicates with 2 records
# dup2_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 2], function(x) which(dat_raw$tag == x)),
#                     function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)))
# )
# dup2_dat <- lapply(dup2_rows, function(x) list(first = dat_raw[x$first, ], second = dat_raw[x$second, ]))
# names(dup2_dat) <- dup_norecap$tag[dup_norecap$n == 2]
# dup2_dat
# 
# #duplicates with 3 records
# dup3_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 3], function(x) which(dat_raw$tag == x)), 
#                     function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)), third = c((x[3]-3):(x[3]+3)))
# )
# dup3_dat <- lapply(dup3_rows, function(x) list(first = dat_raw[x$first, ], second = dat_raw[x$second, ], third = dat_raw[x$third, ]))
# names(dup3_dat) <- dup_norecap$tag[dup_norecap$n == 3]
# dup3_dat

#find and correct tags recorded once but marked as a recap
recap_nodup <-
  dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)
recap_nodup
nodup_rows <- lapply(lapply(recap_nodup$tag, 
                            function(x) which(dat_raw$tag == x)), 
                     function(x) c((x[1]-3):(x[1]+3)))
nodup_dat <- lapply(nodup_rows, function(x) list(dat_raw[x, ]))
names(nodup_dat) <- recap_nodup$tag
nodup_dat
dat_raw[dat_raw$tag %in% recap_nodup$tag, ] #3 middle river tags and 7 fish retagged after tag loss
  dat_raw$recap[dat_raw$tag %in% recap_nodup$tag] <- FALSE #not useful as recaps
  dat_raw$tagloss <- ifelse(dat_raw$tag %in% setdiff(recap_nodup$tag, c(202029, 202030, 202068)), TRUE, FALSE) #Lost tags and retagged
  table(dat_raw$tagloss)
#check
dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)

#one more check
dat_raw[is.na(dat_raw$tag) | (dat_raw$fate %in% "censor" & dat_raw$recap == FALSE), ]

dat_mr <- dat_raw %>% dplyr::select(tag, week, loc, recap, lg)

recaps <- unique(dat_mr$tag[dat_mr$recap %in% TRUE])
recap_list <- lapply(recaps, function(x) dat_mr[dat_mr$tag == x, ])
table(sapply(recap_list, nrow))

fill <-
  dat_mr[dat_mr$tag %in% recaps, ] %>%
    dplyr::arrange(tag, week) %>%
    dplyr::group_by(tag) %>%
    dplyr::summarise(lg_range = max(lg, na.rm = TRUE) - min(lg, na.rm = TRUE),
                     lg = mean(lg, na.rm = TRUE))
data.frame(lg_range = fill$lg_range) %>% dplyr::group_by(lg_range) %>% dplyr::summarise(num = n()) %>% print(n = 100)
fill[fill$lg_range >= 12.5, ] %>% print(n = 100)

dat_recaps <-
  dplyr::left_join(dat_mr[dat_mr$tag %in% recaps, ], fill, by = "tag") %>%
  dplyr::arrange(tag, week) %>%
  dplyr::mutate(lg = ifelse(!is.na(lg.x) & abs(lg.y - lg.x) >= 12.5, NA, lg.y)) %>%
  dplyr::select(tag, week, loc, recap, lg)

CH_UR18 <-
  dplyr::bind_rows(dat_mr[!(dat_mr$tag %in% recaps), ], dat_recaps) %>%
  dplyr::select(week, tag) %>%
  dplyr::mutate(cap = 1) %>%
  tidyr::spread(week, cap, fill = 0, sep = "") %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::mutate(ch = paste0(week1, week2, week3, week4, week5, week6)) %>%
  dplyr::select(-dplyr::starts_with("week")) %>%
  dplyr::left_join(dat_UR18[, c("tag", "lg")] %>%
                     dplyr::group_by(tag) %>%
                     dplyr::summarise(lg = as.integer(mean(lg))), 
                   by = "tag")

devtools::use_data(CH_UR18, pkg = ".\\KenaiTrout", overwrite = TRUE)
