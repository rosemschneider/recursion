## This script takes in original coding spreadsheets, selects final columns for analysis,
## and replaces original participant IDs with sequential ones.
## To run this script we need the folder "raw data not scrubbed"
## and the following files which contain participant-identifying information:
## - "participant ids.csv" -- This contains key of participant IDs as initially defined ("MMDDYY-XX") and sequential IDs reported in manuscript
## - "recursion_full.csv" -- This contains long-format full dataset
## - "HC-datawide-forcoding - hc.datawide.csv" -- This contains wide-format initial and final coding for Highest Count task
## - "sara-HC-datawide-forcoding - hc.datawide.csv" -- This contains additional coding for number of reminders during Highest Count task
## - "HCdata_jccoding.csv" -- This contains additional coding for error sequences during Highest Count task

rm(list = ls())
library(dplyr)
library(tidylog)

idkey <- read.csv('raw data not scrubbed/participant ids.csv')

recursion_full<- read.csv('raw data not scrubbed/recursion_full.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant)
write.csv(recursion_full,'data/raw data/recursion_full.csv')


recursion_hc_wide <- read.csv('raw data not scrubbed/HC-datawide-forcoding - hc.datawide.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant) %>%
  dplyr::rename(ihc_jc=junyi.ihc, prod_jc=productivity_junyi,
         notes_jc = notes_junyi, prod_rms = productivity_rms)
write.csv(recursion_hc_wide,'data/raw data/recursion_hc_coding_wide.csv')


recursion_hc_reminders <- read.csv('raw data not scrubbed/sara-HC-datawide-forcoding - hc.datawide.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant) %>%
  dplyr::select(LadlabID, prod_tomerge, ihc_tomerge, fhc_tomerge, dce, sup.raw, reminders.total, reminders.recovered, notes_SL)
write.csv(recursion_hc_reminders,'data/raw data/recursion_hc_reminders_wide.csv')


recursion_hc_errors<- read.csv('raw data not scrubbed/HCdata_jccoding.csv') %>%
  left_join(idkey, by="LadlabID") %>%
  dplyr::select(-LadlabID) %>% dplyr::rename(LadlabID=participant)
write.csv(recursion_hc_errors,'data/raw data/recursion_hc_errors_long.csv')


