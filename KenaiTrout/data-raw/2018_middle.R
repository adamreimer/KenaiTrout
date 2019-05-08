library(magrittr)
library(ggplot2)

dat_raw <- 
  readxl::read_excel(".\\KenaiTrout\\data-raw\\Copy of All fish 2018 Mid River RBT.xlsx",
                     sheet = "All fish 2018 Mid River RBT",
                     col_names = c("date0", "boat", "crew", "method", "time0", "fishnum", "loc", "lat", "long", "lg", "recap", "tag", "ad", "sex0", 
                                   "mat0", "fate0", "comment1_0", "comment2_0", "comment3_0"),
                     col_types = c(rep("text", 5), "numeric", "text", rep("skip", 2), "numeric", "logical", "numeric", "logical", "numeric", 
                                   "text", "numeric", rep("text", 3)),
                     skip = 1) %>%
  dplyr::mutate(date = as.Date(as.character(paste0("0", date0)), "%m%d%Y"), 
                week = as.character(as.character(date, format = "%W")),
                nchar0 = nchar(time0),
                time00 = ifelse(nchar0 == 5, paste0(0, time0), time0),
                #dt = as.POSIXct(as.POSIXlt(time00, format = "%H%M%S"), tz = "America/Anchorage"),
                dt = as.POSIXct(paste0(0, date0, ":", time00), format = "%m%d%Y:%H%M%S", tz = "America/Anchorage"),
                fate = plyr::mapvalues(fate0, from = c(0, 1), to = c("censor", "rel")),
                sex = plyr::mapvalues(sex0, from = c(0, 1, 2), to = c("U", "M", "F")),
                mat = plyr::mapvalues(mat0, from = c(0, 1, 2, 3), to = c(NA, "pre-spawn", "spawning", "post-spawn")),
                comment0 = paste0(ifelse(!is.na(comment1_0), paste0(comment1_0, ifelse(is.na(comment2_0) & is.na(comment3_0), "", ", ")), ""), 
                                 ifelse(!is.na(comment2_0), paste0(comment2_0, ifelse(is.na(comment3_0), "", ", ")), ""),
                                 ifelse(is.na(comment3_0), "", comment3_0)),
                comment = ifelse(comment0 == "", NA, comment0)) %>%
  dplyr::select(-dplyr::ends_with("0")) %>%
  dplyr::arrange(date, boat, fishnum)

dup_id <- duplicated(dat_raw) 
sum(dup_id) #36 duplicate rows!
dup <- dat_raw[dup_id, ]
dat_raw <- dat_raw[!dup_id, ] # 2495 before, 2459 after


dat_raw
#Check each variable and correct as needed
table(dat_raw$date, dat_raw$week, useNA = "ifany")
table(dat_raw$boat, useNA = "ifany")
table(dat_raw$crew, useNA = "ifany")
table(dat_raw$method, useNA = "ifany")
table(is.na(dat_raw$dt))
  hist(as.numeric(format(dat_raw$dt, format = "%H")), main = "hour") #lots of bad times!
plot(dat_raw$fishnum)
table(dat_raw$loc, useNA = "ifany")
  dat_raw[dat_raw$loc == 0, ] #3 loc = 0
hist(dat_raw$lg)
  table(dat_raw$lg[dat_raw$lg <= 150], useNA = "ifany") #some fl = 0, two very small
  dat_raw$lg[dat_raw$lg <= 39] <- NA
  hist(dat_raw$lg) #OK
table(dat_raw$recap, useNA = "ifany")
  dat_raw[is.na(dat_raw$recap), ]
  dat_raw[dat_raw$tag == 1113, ] #change to recap == 0
  dat_raw$recap[dat_raw$tag == 1113] <- 0
  dat_raw[is.na(dat_raw$recap), ] #OK
table(dat_raw$tag == 0, useNA = "ifany") #21 tag=0
  dat_raw[dat_raw$tag == 0, ] %>% print(n = 100) # 20 have questionable fate, one missing delete (21)
  dat_raw[dat_raw$tag == 0 & dat_raw$fate == "rel" & dat_raw$fishnum == 47, ] #this one looks OK but no tag number
  dat_raw <- dat_raw[dat_raw$tag != 0, ] #2459 records before, 2438 after
  dat_raw[is.na(dat_raw$tag), ] #OK
  table(dat_raw$tag == 0, useNA = "ifany") #OK 
table(dat_raw$ad, useNA = "ifany")
table(dat_raw$sex, useNA = "ifany")
  dat_raw[is.na(dat_raw$sex), ] #change to sex == 0
  dat_raw$sex[is.na(dat_raw$sex)] <- 0
  dat_raw[is.na(dat_raw$sex), ] #OK 
table(dat_raw$mat, useNA = "ifany")
table(dat_raw$fate, useNA = "ifany")
dat_raw[grepl("TE COMMENT", dat_raw$comment), ] #Tony's comments likely helpful later
  dat_raw[grepl("VOID", dat_raw$comment), ]
    dat_raw[dat_raw$tag == 36, ] #?
    dat_raw[dat_raw$tag %in% 1026:1028, ] #OK
    dat_raw[dat_raw$tag %in% 1294:1296, ] #OK
    dat_raw[dat_raw$tag %in% 1038:1041, ] #OK void on tag = 1040 in reference to 1039?
    dat_raw[dat_raw$tag %in% 1450:1453, ] #? void on tag = 1452 could have refered to 1451 but it was deployed a few day later??
      dat_raw[dat_raw$date == "2018-06-04" & dat_raw$boat == 1 & dat_raw$fishnum %in% 11:15, ]

#check combinations of vars
dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] %>% print(n = 100)
  dat_raw[dat_raw$tag %in% c("9999", "99999", "999999"), ] # looks like 9999, 99999, and 999999 are dummy tag numbers, delete (7) note 2 are marked recap
  dat_raw <- dat_raw[!(dat_raw$tag %in% c("9999", "99999", "999999")), ] #2431 remain
    dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] %>% print(n = 100) # save the recaps, delete the rest (12)
      dat_raw <- dat_raw[!(dat_raw$fate == "censor" & dat_raw$recap == 0), ] #2419 remain
  dat_raw[dat_raw$tag != 0 & dat_raw$fate == "censor", ] %>% print(n = 100) # OK mort after recapture
table(dat_raw$comment, dat_raw$fate)
    
#Look for fish captured twice in the same week
same_event <-
  dat_raw %>%
  dplyr::group_by(tag, week) %>%
  dplyr::summarise(recap = max(recap), num = n()) %>%
  dplyr::filter(recap == 1 & num > 1)
print(same_event, n = 50)
sum(same_event$num - 1) #42 duplicate rows

del_same_event <- 
  dat_raw[dat_raw$tag %in% same_event$tag, ] %>% 
  dplyr::group_by(tag, week) %>%
  dplyr::arrange(tag, week, dt) %>%
  dplyr::filter(dplyr::row_number() != 1) %>%
  dplyr::select(tag, week, dt, boat)
dat_raw <- dplyr::anti_join(dat_raw, del_same_event, by = c("tag", "week", "dt", "boat")) #2419 before, 2377 remain

#Find and correct tags recorded more than once but without a recap flag
dup_norecap <-
  dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = sum(recap), lg_range = range(lg)[2] - range(lg)[1]) %>%
  dplyr::filter(recap < (n - 1) & n > 1)
dup_norecap #OK
#duplicates with 2 records
dup2_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 2], function(x) which(dat_raw$tag == x)),
                    function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)))
)
dup2_dat <- lapply(dup2_rows, function(x) list(first = dat_raw[x$first, ], second = dat_raw[x$second, ]))
names(dup2_dat) <- dup_norecap$tag[dup_norecap$n == 2]
dup2_dat
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
recap_nodup #177 tags marked recap without finding a duplicate
in17dat <- intersect(recap_nodup$tag, KenaiTrout::CH_17$tag)
notin17dat <- setdiff(recap_nodup$tag, KenaiTrout::CH_17$tag)
# 
# 
# nodup_rows <- lapply(lapply(recap_nodup$tag, 
#                             function(x) which(dat_raw$tag == x)), 
#                      function(x) c((x[1]-3):(x[1]+3)))
# nodup_dat <- lapply(nodup_rows, function(x) list(dat_raw[x, ]))
# names(nodup_dat) <- recap_nodup$tag
# nodup_dat
# dat_raw[dat_raw$tag %in% recap_nodup$tag, ] #3 middle river tags and 7 fish retagged after tag loss
#   dat_raw$recap[dat_raw$tag %in% recap_nodup$tag] <- FALSE #not useful as recaps
#   dat_raw$tagloss <- ifelse(dat_raw$tag %in% setdiff(recap_nodup$tag, c(202029, 202030, 202068)), TRUE, FALSE) #Lost tags and retagged
#   table(dat_raw$tagloss)
# #check
# dat_raw %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise(n = n(), recap = max(recap)) %>%
#   dplyr::filter(recap == 1 & n == 1)
# 
# #one more check
# dat_raw[is.na(dat_raw$tag) | (dat_raw$fate %in% "censor" & dat_raw$recap == FALSE), ]
# 
# dat_mr <- dat_raw %>% dplyr::select(tag, week, loc, recap, lg)
# 
# recaps <- unique(dat_mr$tag[dat_mr$recap %in% TRUE])
# recap_list <- lapply(recaps, function(x) dat_mr[dat_mr$tag == x, ])
# table(sapply(recap_list, nrow))
# 
# fill <-
#   dat_mr[dat_mr$tag %in% recaps, ] %>%
#     dplyr::arrange(tag, week) %>%
#     dplyr::group_by(tag) %>%
#     dplyr::summarise(lg_range = max(lg, na.rm = TRUE) - min(lg, na.rm = TRUE),
#                      lg = mean(lg, na.rm = TRUE))
# data.frame(lg_range = fill$lg_range) %>% dplyr::group_by(lg_range) %>% dplyr::summarise(num = n()) %>% print(n = 100)
# fill[fill$lg_range >= 12.5, ] %>% print(n = 100)
# 
# dat_recaps <-
#   dplyr::left_join(dat_mr[dat_mr$tag %in% recaps, ], fill, by = "tag") %>%
#   dplyr::arrange(tag, week) %>%
#   dplyr::mutate(lg = ifelse(!is.na(lg.x) & abs(lg.y - lg.x) >= 12.5, NA, lg.y)) %>%
#   dplyr::select(tag, week, loc, recap, lg)
# 
# dat_UR18 <- dplyr::bind_rows(dat_mr[!(dat_mr$tag %in% recaps), ], dat_recaps)
# 
# LH <-
#   dat_UR18 %>%
#   dplyr::select(week, tag, loc) %>%
#   dplyr::mutate(loc = cut(loc, breaks = c(0, 5.5, 10.5, 17), labels = FALSE)) %>%
#   tidyr::spread(week, loc, fill = 0, sep = "") %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise_all(sum) %>%
#   dplyr::mutate(lh = paste0(week1, week2, week3, week4, week5, week6)) %>%
#   dplyr::select(-dplyr::starts_with("week"))
# 
# CH_UR18 <-
#   dat_UR18 %>%
#   dplyr::select(week, tag) %>%
#   dplyr::mutate(cap = 1) %>%
#   tidyr::spread(week, cap, fill = 0, sep = "") %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise_all(sum) %>%
#   dplyr::mutate(ch = paste0(week1, week2, week3, week4, week5, week6)) %>%
#   dplyr::select(-dplyr::starts_with("week")) %>%
#   dplyr::left_join(dat_UR18[, c("tag", "lg")] %>%
#                      dplyr::group_by(tag) %>%
#                      dplyr::summarise(lg = as.integer(mean(lg))), 
#                    by = "tag") %>%
#   dplyr::left_join(LH, by = "tag")
# 
# devtools::use_data(CH_UR18, pkg = ".\\KenaiTrout", overwrite = TRUE)
