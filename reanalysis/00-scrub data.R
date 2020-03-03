library(dplyr)
idkey <- read.csv('raw data not scrubbed/participant ids.csv')

recursion_full<- read.csv('raw data not scrubbed/recursion_full.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant)
write.csv(recursion_full,'raw data/recursion_full.csv')


recursion_hc_wide <- read.csv('raw data not scrubbed/HC-datawide-forcoding - hc.datawide.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant) %>%
  dplyr::rename(ihc_jc=junyi.ihc, prod_jc=productivity_junyi,
         notes_jc = notes_junyi, prod_rms = productivity_rms)
write.csv(recursion_hc_wide,'raw data/recursion_hc_coding_wide.csv')


recursion_hc_reminders <- read.csv('raw data not scrubbed/sara-HC-datawide-forcoding - hc.datawide.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant) %>%
  dplyr::select(LadlabID, prod_tomerge, ihc_tomerge, fhc_tomerge, dce, sup.raw, reminders.total, reminders.recovered, notes_SL)
write.csv(recursion_hc_reminders,'raw data/recursion_hc_reminders_wide.csv')


recursion_hc_errors<- read.csv('raw data not scrubbed/HCdata_jccoding.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant)
write.csv(recursion_hc_errors,'raw data/recursion_hc_errors_long.csv')


