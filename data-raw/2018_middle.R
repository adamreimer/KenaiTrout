library(magrittr)
library(ggplot2)

dat_raw <- 
  readxl::read_excel(".\\data-raw\\All fish 2018 Mid River RBT_2.xlsx",
                     sheet = "All fish 2018 Mid River RBT",
                     col_names = c("date0", "boat", "crew", "method", "time0", "fishnum", "loc", "lat", "long", "lg", "recap", "tag", "ad", "sex0", 
                                   "mat0", "fate0", "comment1_0", "comment2_0", "comment3_0"),
                     col_types = c(rep("text", 5), "numeric", "text", rep("skip", 2), "numeric", "logical", "numeric", "logical", "numeric", 
                                   "text", "numeric", rep("text", 3)),
                     skip = 1) %>%
  dplyr::mutate(date = as.Date(as.character(paste0("0", date0)), "%m%d%Y"), 
                event = factor(format(date, format = "%W"), labels = as.character(1:6)),
                nchar0 = nchar(time0),
                time00 = ifelse(nchar0 == 5, paste0(0, time0), time0),
                #dt = as.POSIXct(as.POSIXlt(time00, format = "%H%M%S"), tz = "America/Anchorage"),
                dt = as.POSIXct(paste0(0, date0, ":", time00), format = "%m%d%Y:%H%M%S", tz = "America/Anchorage"),
                fate = plyr::mapvalues(fate0, from = c(0, 1), to = c("censor", "rel")),
                sex = plyr::mapvalues(sex0, from = c(0, 1, 2), to = c(NA, "M", "F")),
                mat = plyr::mapvalues(mat0, from = c(0, 1, 2, 3), to = c(NA, 0, 1, 1)),
                comment0 = paste0(ifelse(!is.na(comment1_0), paste0(comment1_0, ifelse(is.na(comment2_0) & is.na(comment3_0), "", ", ")), ""), 
                                 ifelse(!is.na(comment2_0), paste0(comment2_0, ifelse(is.na(comment3_0), "", ", ")), ""),
                                 ifelse(is.na(comment3_0), "", comment3_0)),
                comment = ifelse(comment0 == "", NA, comment0)) %>%
  dplyr::select(-dplyr::ends_with("0")) %>%
  dplyr::arrange(date, boat, fishnum)

dup_id <- duplicated(dat_raw) 
sum(dup_id) #0 duplicate rows

#Note get effort data here
e_time <- 
  dplyr::left_join(aggregate(dt ~ date + crew, dat_raw, max), 
                 aggregate(dt ~ date + crew, dat_raw, min),
                 by = c("date", "crew")) %>% 
  dplyr::left_join(aggregate(method ~ date + crew, dat_raw, unique),
                   by = c("date", "crew")) %>% 
  dplyr::mutate(diff = difftime(dt.x, dt.y, units = "hours"))
e_time$event <- factor(format(e_time$date, format = "%W"), labels = as.character(1:6))
#e_time$event <- ifelse(e_time$event == 1 , 2, e_time$event)
aggregate(diff ~ event, data = e_time, sum)
table(dat_raw$method, dat_raw$crew, dat_raw$date)[, , 10:25]
aggregate(crew ~ date, dat_raw, unique)

dat_raw
#Check each variable and correct as needed
table(dat_raw$date, dat_raw$event, useNA = "ifany") #first event too short use an effort covariate
  #dat_raw$event <- ifelse(dat_raw$date == "2018-05-04", 2, dat_raw$event)
  table(dat_raw$date, dat_raw$event, useNA = "ifany") #first event too short, merge w second
table(dat_raw$boat, useNA = "ifany")
table(dat_raw$crew, useNA = "ifany") # Note effort changes between events
  effort0 <- aggregate(crew ~ date, data = dat_raw, unique)
  effort0$event <- factor(format(effort0$date, format = "%W"), labels = as.character(1:6))
#  effort0$event <- ifelse(effort0$event == 1 , 2, effort0$event)
  effort0$effort <- sapply(effort0$crew, function(x) length(strsplit(x, " ")))
  aggregate(effort ~ event, data = effort0, sum)

table(dat_raw$method, useNA = "ifany")
table(is.na(dat_raw$dt))
  hist(as.numeric(format(dat_raw$dt, format = "%H")), main = "hour") #lots of bad times!
  table(format(dat_raw$dt, format = "%H"), dat_raw$date) #limited to a few days
  #Assume early times just shifted 8 hours
  #minor relevence, only use is to keep earliest record when multiple recaps occur during the same event
  lubridate::hour(dat_raw$dt) <- ifelse(as.numeric(format(dat_raw$dt, format = "%H")) < 8, lubridate::hour(dat_raw$dt) + 7, lubridate::hour(dat_raw$dt))
  hist(as.numeric(format(dat_raw$dt, format = "%H")), main = "hour") #lots of bad times!
  table(lubridate::hour(dat_raw$dt)) #Ok
plot(dat_raw$fishnum)
table(dat_raw$loc, useNA = "ifany")
  dat_raw[dat_raw$loc == 0, ] #3 loc = 0
hist(dat_raw$lg)
  table(dat_raw$lg[dat_raw$lg <= 150], useNA = "ifany") #some fl = 0, two very small
  dat_raw$lg[dat_raw$lg <= 4] <- NA
  hist(dat_raw$lg) #OK
table(dat_raw$recap, useNA = "ifany")
table(dat_raw$tag == 0, useNA = "ifany") #21 tag=0 Robert verified all these fish should be censored
  dat_raw[dat_raw$tag %in% 0, ] %>% print(n = 45)
  #Change to tag = NA
  dat_raw$tag[dat_raw$tag == 0] <- NA
  table(dat_raw$tag == 0, useNA = "ifany") # OK
  #Change fate to censor
  table(dat_raw$fate)
  dat_raw$fate[is.na(dat_raw$tag)] <- "censor";
  table(dat_raw$fate) #OK
table(dat_raw$ad, useNA = "ifany")
table(dat_raw$sex, useNA = "ifany")
table(dat_raw$mat, useNA = "ifany")
table(dat_raw$fate, useNA = "ifany")
dat_raw[grepl("TE COMMENT", dat_raw$comment), ] #Tony's comments likely helpful later
  dat_raw[grepl("VOID", dat_raw$comment), ] #These comments refer to seqence breaks in tab #, OK to keep
    dat_raw[dat_raw$tag %in% 1025:1029, ] #OK
    dat_raw[dat_raw$tag %in% 1294:1296, ] #OK
    dat_raw[dat_raw$tag %in% 1038:1041, ] #OK void on tag = 1040 in reference to 1039?
    dat_raw[dat_raw$tag %in% 1450:1453, ] #? void on tag = 1452 could have refered to 1451 but it was deployed a few day later??
      dat_raw[dat_raw$date == "2018-06-04" & dat_raw$boat == 1 & dat_raw$fishnum %in% 11:15, ]

#Deep dive into tag numbers and fates
dat_raw[is.na(dat_raw$tag), ] %>% print(n = 100) #OK
dat_raw[dat_raw$fate == "censor", ] %>% print(n = 100)
  dat_raw[dat_raw$tag %in% c("9999", "99999", "999999"), ] #9999, 99999, and 999999 are dummy tag numbers (2 of which say recap but are not per Robert)
  dat_raw$recap[dat_raw$tag %in% c("9999", "99999", "999999")] <- FALSE 
  dat_raw$tag[dat_raw$tag %in% c("9999", "99999", "999999")] <- NA 
  dat_raw[dat_raw$tag %in% c("9999", "99999", "999999"), ] #ok
  #RECHECK
  dat_raw[dat_raw$fate == "censor", ] %>% print(n = 100)
  dat_raw[dat_raw$fate == "censor" & dat_raw$recap == TRUE, ] %>% print(n = 100) #keep these, censored after recap
  dat_raw[dat_raw$fate == "censor" & dat_raw$recap != TRUE & is.na(dat_raw$tag), ] %>% print(n = 100) #delete, never tagged
  tag_censor <- dat_raw[dat_raw$fate == "censor" & dat_raw$recap != TRUE & !is.na(dat_raw$tag), ] %>% print(n = 100) 
  tag_censor #delete
  dat_raw[dat_raw$tag %in% tag_censor$tag, ] #delete tagged fish which were censored were never recaptured
    delete_rows <- dat_raw$fate == "censor" & dat_raw$recap != TRUE
    dat_raw <- dat_raw[!delete_rows, ] #2457 before, 2418 after
  #RECHECK
  dat_raw[dat_raw$fate == "censor", ] %>% print(n = 100)
table(dat_raw$comment, dat_raw$fate) #Ok
 

#Find and correct tags recorded more than once but without a recap flag
dup_norecap <-
  dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = dplyr::n(), recap = sum(recap), lg_range = range(lg)[2] - range(lg)[1]) %>%
  dplyr::filter(recap < (n - 1) & n > 1)
dup_norecap #OK
#duplicates with 2 records
dup2_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 2], function(x) which(dat_raw$tag == x)),
                    function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)))
)
dup2_dat <- lapply(dup2_rows, function(x) list(first = dat_raw[x$first, ], second = dat_raw[x$second, ]))
names(dup2_dat) <- dup_norecap$tag[dup_norecap$n == 2]
dup2_dat
dat_raw$recap[dat_raw$tag == 1529 & dat_raw$fishnum == 81] <- TRUE #set second capture to recap = T
# 
# #duplicates with 3 records
# dup3_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 3], function(x) which(dat_raw$tag == x)), 
#                     function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)), third = c((x[3]-3):(x[3]+3)))
# )
# dup3_dat <- lapply(dup3_rows, function(x) list(first = dat_raw[x$first, ], second = dat_raw[x$second, ], third = dat_raw[x$third, ]))
# names(dup3_dat) <- dup_norecap$tag[dup_norecap$n == 3]
# dup3_dat

   
#Look for fish captured twice in the same week
same_event <-
  dat_raw %>%
  dplyr::group_by(tag, event) %>%
  dplyr::summarise(recap = max(recap), num = dplyr::n()) %>%
  dplyr::filter(recap == 1 & num > 1)
print(same_event, n = 50)
sum(same_event$num - 1) #45 duplicate rows

del_same_event <- 
  dat_raw[dat_raw$tag %in% same_event$tag, ] %>% 
  dplyr::group_by(tag, event) %>%
  dplyr::arrange(tag, event, dt) %>%
  dplyr::filter(dplyr::row_number() != 1) %>%
  dplyr::select(tag, event, dt, boat)
dat_raw <- dplyr::anti_join(dat_raw, del_same_event, by = c("tag", "event", "dt", "boat")) #2418 before, 2376 after



#find and correct tags recorded once but marked as a recap
recap_nodup <-
  dat_raw %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = dplyr::n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)
recap_nodup #163 tags marked recap without finding a duplicate
#124 of those are 2017 recaps
load(".\\data\\dat_17.rda")
in17dat <- intersect(recap_nodup$tag, dat_17$tag)
dat_raw[dat_raw$tag %in% in17dat, ] %>% print(n = 150)
dat_raw <- dat_raw[!(dat_raw$tag %in% in17dat), ] # delete these 2373 before, 2249 after


# nodup_rows <- lapply(lapply(recap_nodup$tag,
#                             function(x) which(dat_raw$tag == x)),
#                      function(x) c((x[1]-3):(x[1]+3)))
# nodup_dat <- lapply(nodup_rows, function(x) list(dat[x, ]))
# names(nodup_dat) <- recap_nodup$tag
# nodup_dat

#39 tag loss but retagged (1 censored). Crew estimated 6 from 2018 33 from 2017.
#38 of these were retagged
notin17dat <- setdiff(recap_nodup$tag, dat_17$tag)
#Duplicate the 38 released records; one for the recap but lost tag, another for the retag.
#new data rows for fish that lost the tag
#match format of 2017 dataset
dat_raw <- 
  dat_raw %>%
  dplyr::mutate(tloss = NA, hook = NA, parasite = NA, temp = NA, tl = NA) %>%
  dplyr::select(date, rm = loc, fl = lg, tag, sex, mat, ad, tloss, recap, hook, parasite, fate, temp, tl, event, method)
tloss <- 
    dat_raw[dat_raw$tag %in% notin17dat, ] %>%
    dplyr::mutate(tloss = TRUE, tag = NA) %>% 
  print(n = 100)

table(dat_raw$tloss, useNA = "always")
dat_raw$tloss <- ifelse(is.na(dat_raw$tloss), FALSE, dat_raw$tloss) 
table(dat_raw$tloss, useNA = "always") #Ok

table(dat_raw$recap, useNA = "always")
dat_raw$recap <- ifelse(dat_raw$tag %in% notin17dat, FALSE, dat_raw$recap)
table(dat_raw$recap, useNA = "always") #ok

dat_raw[dat_raw$tag %in% notin17dat, ] %>% dplyr::arrange(tag) %>% print(n = 100) #OK Look like new captured fish

#new datset. Add back in the tag loss fish
dat <- dplyr::bind_rows(dat_raw, tloss) #2253 before, 2292 after
dat[dat$tag %in% notin17dat, ] %>% dplyr::arrange(tag) %>% print(n = 100) #OK
table(dat$tloss) #ok
table(dat_raw$recap, useNA = "always") #ok

#no more single recap rows
dat %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = dplyr::n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)




#None of these lines are useful in CH
dat[is.na(dat$tag) | (dat$fate %in% "censor" & dat$recap == FALSE), ] %>% print(n = 100)
dat_mr <- dat[!(is.na(dat$tag) | (dat$fate %in% "censor" & dat$recap == FALSE)), ] %>% 
  dplyr::mutate(week = format(date, "%W"),
                year = format(date, "%Y")) %>%
  dplyr::select(tag, event, method, rm, recap, fl, year, week)

recaps <- unique(dat_mr$tag[dat_mr$recap %in% TRUE])
recap_list <- lapply(recaps, function(x) dat_mr[dat_mr$tag == x, ])
table(sapply(recap_list, nrow))

sum(is.na(dat_mr$fl))
fill <-
  dat_mr[dat_mr$tag %in% recaps, ] %>%
    dplyr::arrange(tag, event) %>%
    dplyr::group_by(tag) %>%
    dplyr::summarise(fl_range = max(fl, na.rm = TRUE) - min(fl, na.rm = TRUE),
                     fl = mean(fl, na.rm = TRUE))
data.frame(fl_range = fill$fl_range) %>% dplyr::group_by(fl_range) %>% dplyr::summarise(num = dplyr::n()) %>% print(n = 100) #-Inf = both fl missing
fill[fill$fl_range >= 12.5, ] %>% print(n = 150)

dat_recaps <-
  dplyr::left_join(dat_mr[dat_mr$tag %in% recaps, ], fill, by = "tag") %>%
  dplyr::arrange(tag, event) %>%
  dplyr::mutate(fl = ifelse(!is.na(fl.x) & abs(fl.y - fl.x) >= 12.5, NA, fl.y)) %>%
  dplyr::select(tag, event, method, rm, recap, fl, year, week)

dat_18 <- dplyr::bind_rows(dat_mr[!(dat_mr$tag %in% recaps), ], dat_recaps)
saveRDS(dat_18, ".\\data\\dat_18.rds")

#non consistent location between years
# LH <-
#   dat_MR18 %>%
#   dplyr::select(event, tag, rm) %>%
#   dplyr::mutate(loc = cut(loc, breaks = c(0, 5.5, 10.5, 17), labels = FALSE)) %>%
#   tidyr::spread(event, loc, fill = 0, sep = "") %>%
#   dplyr::group_by(tag) %>%
#   dplyr::summarise_all(sum) %>%
#   dplyr::mutate(lh = paste0(event1, event2, event3, event4, event5, event6)) %>%
#   dplyr::select(-dplyr::starts_with("event"))

CH_18 <-
  dat_18 %>%
  dplyr::select(event, tag) %>%
  dplyr::mutate(cap = 1) %>%
  tidyr::spread(event, cap, fill = 0, sep = "") %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::mutate(ch = paste0(event1, event2, event3, event4, event5, event6)) %>%
  dplyr::select(-dplyr::starts_with("event")) %>%
  dplyr::left_join(dat_18[, c("tag", "fl")] %>%
                     dplyr::group_by(tag) %>%
                     dplyr::summarise(fl = as.integer(mean(fl))),
                   by = "tag") # %>%
#  dplyr::left_join(LH, by = "tag")

saveRDS(CH_18, ".\\data\\CH_18.rds")

