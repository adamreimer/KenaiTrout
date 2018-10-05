library(magrittr)
library(ggplot2)

rawdat <- 
  readxl::read_excel(".\\KenaiTrout\\data-raw\\Copy of UPRRBT18_to Adam 9.10.19.xlsx",
                     sheet = "UPRRBT18",
                     col_names = c("date0", "boat", "crew", "fishnum", "loc", "tag", "recap", "lg", "ad", "fate", "comment1", "comment2", 
                                   "week", "comment3"),
                     skip = 1) %>%
  dplyr::arrange(date0, boat, fishnum)
rawdat
table(as.Date(as.character(paste0("0", rawdat$date0)), "%m%d%y"), useNA = "ifany")
table(rawdat$boat)
table(rawdat$crew)
plot(rawdat$fishnum)
table(rawdat$loc)
table(rawdat$tag == 0, useNA = "ifany") #some tag=0
rawdat[rawdat$tag == 0, ] %>% print(n = 100) # all tag  0 are fate = 0, delete (60)
table(rawdat$recap)
hist(rawdat$lg)
table(rawdat$lg[rawdat$lg <= 150], useNA = "ifany") #some fl = 0
table(rawdat$ad)
table(rawdat$fate)
rawdat[rawdat$tag != 0 & rawdat$fate == 0, ] %>% print(n = 100) # save the recaps, delete the rest (11)
table(rawdat$comment1)
table(rawdat$comment2)
table(rawdat$comment3)
table(rawdat$comment1, rawdat$comment2)
table(rawdat$comment1, rawdat$comment3)
table(rawdat$week, rawdat$date0)

dat <- 
  rawdat %>%
  dplyr::filter(tag != 0, !(fate == 0 & recap == 0)) %>%
  dplyr::mutate(date = as.Date(as.character(paste0("0", date0)), "%m%d%y"),
                ad = as.logical(ad),
                recap = as.logical(recap),
                fate = plyr::mapvalues(fate, from = c(0, 1), to = c("censor", "rel")),
                tl = ifelse(lg == 0, NA, lg),
                week = ifelse(week == 2 & date0 == 70518, 1, week),
                comment0 = paste0(ifelse(!is.na(comment1), paste0(comment1, ifelse(is.na(comment2) & is.na(comment3), "", ", ")), ""), 
                                 ifelse(!is.na(comment2), paste0(comment2, ifelse(is.na(comment3), "", ", ")), ""),
                                 ifelse(is.na(comment3), "", comment3)),
                comment = ifelse(comment0 == "", NA, comment0)) %>%
  dplyr::select(tag, date, week, loc, recap, ad, fate, tl, boat, crew, fishnum, comment)

dat
table(dat$tag == 0, useNA = "ifany")
table(dat$date, dat$week)
table(dat$loc)
table(dat$recap)
table(dat$ad)
table(dat$fate)
dat[dat$fate == "censor", ] %>% print(n = 100)
hist(dat$tl)
table(dat$comment, dat$fate)
  dat[dat$comment %in% "BLEEDING NO TAG", ] #have tag numbers
  dat[dat$comment %in% "FORGOT AD CLIP", ]
    dat[dat$tag == 2756, ] 
    dat[dat$date == "2018-07-17" & dat$boat == 2 & dat$fishnum %in% 66:72, ]
    dat[dat$date == "2018-07-31" & dat$boat == 1 & dat$fishnum %in% 43:49, ] #recap??
    dat[dat$date == "2018-08-08" & dat$boat == 1 & dat$fishnum %in% 43:49, ]
  dat[dat$tag == 7071, ] #ad clip but not a recap?
  dat[grepl("LOST", dat$comment),]
    dat[dat$date == "2018-08-06" & dat$boat == 1 & dat$fishnum %in% 123:129, ]
  dat[dat$comment %in% "MAY LIVE?",]
    dat[dat$tag == 2607, ]
  dat[grepl("VOID", dat$comment), ]
    dat[dat$tag %in% 2858:2865, ]
    dat[dat$tag %in% 3444:3452, ] #censor 3448?
    dat[dat$tag %in% 3857:3863, ]
    dat[dat$tag %in% 3227:3233, ] #void 3231?
      dat[dat$tag %in% 3231, ]
    dat[dat$tag %in% 7210:7216, ]

#Find and tags recorded twice without a recap flag
dup_norecap <-
  dat %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = sum(recap), fl_range = range(tl)[2] - range(tl)[1]) %>%
  dplyr::filter(recap < (n - 1) & n > 1)
dup_norecap
#2 records
dup2_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 2], function(x) which(dat$tag == x)), 
                   function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)))
                   )
dup2_dat <- lapply(dup2_rows, function(x) list(first = dat[x$first, ], second = dat[x$second, ]))
names(dup2_dat) <- dup_norecap$tag[dup_norecap$n == 2]
dup2_dat
# Change second record to recap (Note large FL diff for 3209, 3729, and 7220)
dat$recap[dat$tag == 2732 & dat$week == 4] <- TRUE
dat$recap[dat$tag == 3048 & dat$week == 2] <- TRUE
dat$recap[dat$tag == 3099 & dat$week == 3] <- TRUE
dat$recap[dat$tag == 3209 & dat$week == 6] <- TRUE
dat$recap[dat$tag == 3413 & dat$week == 4] <- TRUE
dat$recap[dat$tag == 3729 & dat$week == 5] <- TRUE
dat$recap[dat$tag == 7059 & dat$week == 5] <- TRUE
dat$recap[dat$tag == 7220 & dat$week == 5] <- TRUE
#3 records
dup3_rows <- lapply(lapply(dup_norecap$tag[dup_norecap$n == 3], function(x) which(dat$tag == x)), 
                   function(x) list(first = c((x[1]-3):(x[1]+3)), second = c((x[2]-3):(x[2]+3)), third = c((x[3]-3):(x[3]+3)))
)
dup3_dat <- lapply(dup3_rows, function(x) list(first = dat[x$first, ], second = dat[x$second, ], third = dat[x$third, ]))
names(dup3_dat) <- dup_norecap$tag[dup_norecap$n == 3]
dup3_dat
# Change second record to recap (Note large FL diff for 3209, 3729, and 7220)
dat$recap[dat$tag == 2756 & dat$week == 5] <- TRUE
#check
dat %>% 
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = sum(recap), fl_range = range(tl)[2] - range(tl)[1]) %>%
  dplyr::filter(recap != (n - 1) & n > 1)

#find and correct tags recrded once but marked as a recap
recap_nodup <-
  dat %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)
recap_nodup
nodup_rows <- lapply(lapply(recap_nodup$tag, 
                           function(x) which(dat$tag == x)), 
                    function(x) c((x[1]-3):(x[1]+3)))
nodup_dat <- lapply(nodup_rows, function(x) list(dat[x, ]))
names(nodup_dat) <- recap_nodup$tag
nodup_dat
#Change records
dat$recap[dat$tag %in% recap_nodup$tag] <- FALSE
#check
dat %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(n = n(), recap = max(recap)) %>%
  dplyr::filter(recap == 1 & n == 1)

#drop tag that was recaped with a clip but no tag, retagged, then censored. i.e. no history to retain
dat[dat$fate == "censor", ] %>% print(n = 100)
dat <- dat %>% dplyr::filter(tag != 7374)


#examine recap data
badmrdat <- is.na(dat$tag) | (dat$fate %in% "censor" & dat$recap == FALSE)
dat[badmrdat, ] %>% print(n = 100)
dat$tag[dat$fate %in% "censor" & dat$recap %in% TRUE] #tags that dies after recapture

recaps <- unique(dat$tag[dat$recap == TRUE])
lapply(recaps, function(x) dat[dat$tag == x, ])

fill <-
dat[dat$tag %in% recaps, ] %>%
  dplyr::arrange(tag, date) %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(tl_range = max(tl) - min(tl),
                   tl = mean(tl, na.rm = TRUE))
data.frame(tl_range = fill$tl_range) %>% dplyr::group_by(tl_range) %>% dplyr::summarise(num = n()) %>% print(n = 100)
fill[fill$tl_range >= 10, ] %>% print(n = 100)

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
setdiff(CH_tag[(apply(CH, 1, sum) > 1), ]$tag, recaps)

same_event <-
  dat_mr %>%
  dplyr::group_by(tag, event) %>%
  dplyr::summarise(recap = max(recap), num = n()) %>%
  dplyr::filter(recap == 1 & num > 1)
same_event
recap_nodup
setdiff(recaps, CH_tag[(apply(CH, 1, sum) > 1), ]$tag)


