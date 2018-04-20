library(magrittr)

rawdat <- readr::read_csv("H:\\My Documents\\Kenai Trout\\Trout2017_MIDDLE RIVER_dec12.csv",
                          col_names = c("date", "rm", "fl", "tag", "sex", "mat", "ad", "tloss", "recap", "hook", "parasite", "fate", "temp", "tl"),
                          col_types = c("ciiciiiiiiiini")) #text", rep("numeric", 2), "text",  rep("numeric", 10)))
rawdat

table(as.Date(rawdat$date, "%Y%m%d"), useNA = "ifany") #OK
table(rawdat$rm, useNA = "ifany") #OK

#clean lengths
hist(rawdat$fl)
table(rawdat$fl[rawdat$fl <= 150], useNA = "ifany") #43 fl = 0
hist(rawdat$tl)
plot(rawdat$fl, rawdat$tl) #fl/tl pairs OK
rawdat[rawdat$tag %in% rawdat$tag[which((rawdat$fl >= rawdat$tl) %in% TRUE)], ] #0 fl >= tl
tail(sort(rawdat$tl/rawdat$fl), 10) # no large discrepancies between some fl and tl

table(rawdat$tag == 0, useNA = "ifany") #25 tag=0
rawdat[rawdat$tag == 0, ] %>% print(n = 100)
dup_norecap <-
  rawdat[rawdat$tag != 0, ] %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap), fl_range = range(fl)[2] - range(fl)[1]) %>%
  dplyr::filter(recap == 0 & n > 1)
dup_norecap # 5 duplicate entries without a recap flag
dup_rows <- lapply(lapply(dup_norecap$tag, function(x) which(rawdat$tag == x)), function(x) list(first = c((x[1]-4):(x[1]+4)), second = c((x[2]-4):(x[2]+4))))
dup_dat <- lapply(dup_rows, function(x) list(first = rawdat[x$first, ], second = rawdat[x$second, ]))
names(dup_dat) <- dup_norecap$tag
dup_dat

recap_nodup <-
  rawdat[rawdat$tag != 0, ] %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)
recap_nodup # 2 marked as recap with no duplicate record (tag lost but adclipped, retagged)
nodup_rows <- lapply(lapply(recap_nodup$tag, function(x) which(rawdat$tag == x)), function(x) list(first = c((x[1]-4):(x[1]+4))))
nodup_dat <- lapply(nodup_rows, function(x) list(first = rawdat[x$first, ]))
names(nodup_dat) <- recap_nodup$tag
nodup_dat

#Break 2 records where tag lost but adclipped, retagged into 4 records, one for the recap and one for the new tagging event.
tag_loss <- dplyr::bind_rows(rawdat[rawdat$tag %in% recap_nodup$tag, ] %>% dplyr::mutate(tag = NA),
                             rawdat[rawdat$tag %in% recap_nodup$tag, ] %>% dplyr::mutate(tloss = 0, recap = 0))

lapply(rawdat[, c(5:12)], table, useNA = "ifany") #ok
rawdat[rawdat$tloss %in% TRUE,] #2 lost tags, see nodup_dat above.

hist(rawdat$temp) #ok
plot(as.Date(rawdat$date, "%Y%m%d"), rawdat$temp)

dat <- rawdat %>%
        dplyr::filter(!(tag %in% recap_nodup$tag)) %>% #delete tag loss records
        dplyr::bind_rows(tag_loss) %>% #add back in as tag loss and remarked fish
        dplyr::mutate(date = as.Date(date, "%Y%m%d"),
                      fl = ifelse(fl == 0, NA, fl),
                      tag = as.numeric(ifelse(tag == "0", NA, tag)),
                      sex = plyr::mapvalues(sex, from = c(0, 1, 2), to = c("U", "M", "F")),
                      mat = as.logical(mat),
                      ad = as.logical(ad),
                      tloss = as.logical(tloss),
                      recap = as.logical(recap),
                      hook = as.logical(hook),
                      parasite = as.logical(parasite),
                      fate = plyr::mapvalues(fate, from = c(0, 1), to = c("rel", "mort"))
                      )

table(dat$fl[dat$fl <= 150], useNA = "ifany") #43 + 1

table(dat$tag == 0, useNA = "ifany") #25 + 2 (2 from lag loss)
dat[is.na(dat$tag), ] %>% print(n = 100)

lapply(dat[, c(5:12)], table, useNA = "ifany")

#define events
table(format(dat$date, "%W"))
dat$event <- factor(format(dat$date, "%W"),labels = as.character(1:6))

#examine recap data
badmrdat <- is.na(dat$tag) | (dat$fate %in% "mort" & dat$recap == FALSE) # not usable in CH
dat[badmrdat, ] %>% print(n = 100)
dat[dat$tag %in% dat$tag[dat$fate %in% "mort" & dat$recap %in% TRUE], ] #1 recapture deemed a mort

dat_mr <- dat[!badmrdat, ]
recaps <- unique(dat_mr$tag[dat_mr$recap == TRUE])
lapply(recaps, function(x) dat_mr[dat_mr$tag == x, ])

fill <-
dat_mr[dat_mr$tag %in% recaps, c(1, 3:6, 10:11, 14:15)] %>%
  dplyr::arrange(tag, date) %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(fl_range = max(fl) - min(fl),
                   fl = mean(fl, na.rm = TRUE),
                   sex = as.character(list(unique(sex[mat == TRUE])[!is.na(unique(sex[mat == TRUE]))])),
                   hook = as.logical(ifelse(is.na(dplyr::last(hook)), max(hook, na.rm = TRUE), dplyr::last(hook))),
                   parasite = as.logical(ifelse(is.na(dplyr::last(parasite)), max(parasite, na.rm = TRUE), dplyr::last(parasite))))
data.frame(fl_range = fill$fl_range) %>% dplyr::group_by(fl_range) %>% dplyr::summarise(num = n())


dat_recaps <-
  dplyr::left_join(dat_mr[dat_mr$tag %in% recaps, ], fill, by = "tag") %>%
  dplyr::arrange(tag, event) %>%
  dplyr::mutate(fl = ifelse(!is.na(fl.x) & abs(fl.y - fl.x) >= 12.5, NA, fl.y),
                sex = ifelse(mat %in% c(NA, FALSE),
                             ifelse(sex.y %in% c("M", "F"), sex.y, "U"),
                             sex.x)) %>%
  dplyr::select(date, rm, fl, tag, sex, mat, ad, tloss, recap, hook = hook.y, parasite = parasite.y, temp, event) %>%
  dplyr::distinct(dat_recaps, tag, event, .keep_all = TRUE)

CH_tag <-
dplyr::bind_rows(dat_mr[!(dat_mr$tag %in% recaps), ], dat_recaps) %>%
  dplyr::distinct(tag, event, .keep_all =TRUE) %>% #allows records from different strata
  dplyr::mutate(cap = 1) %>%
  tidyr::spread(event, cap, fill = 0) %>%
  dplyr::select(tag, '1', '2', '3', '4', '5', '6') %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise_all(sum)

CH <-
  CH_tag %>%
  dplyr::select(-tag) %>%
  as.matrix()

table(apply(CH, 1, sum))

dup_norecap
#In CH not marked recaps (two entries neither marked recap, only 2 tag numbers had entries from different events)
setdiff(CH_tag[(apply(CH, 1, sum) > 1), ]$tag, recaps)

same_event <-
  dat_mr %>%
  dplyr::group_by(tag, event) %>%
  dplyr::summarise(recap = max(recap), num = n()) %>%
  dplyr::filter(recap == 1 & num > 1)
same_event
setdiff(recaps, CH_tag[(apply(CH, 1, sum) > 1), ]$tag) #in recaps not in CH (tagged in same event)

#57 recaps in CH
#60 in recaps
#60(recap) + 2(dup_norecap) - 5(same event) = 57

