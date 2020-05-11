## This script takes in deidentified coding spreadsheets from "data/raw data"
## and converts to 
## Outputs: 
## - data/CountingToInfinity.RData --- used for all our analysis scripts
## Also outputs csv versions of the dataframe objects inside CountingToInfinity.RData:
## - data/data-full.csv
## - data/data-hc-unique.csv
## - data/data-hc-errors.csv
## - data/data-nn-long.csv
## - data/data-nn-wide.csv

## To run this script we need the folder "raw data"
## and the following data files:
## - "participant ids.csv" -- This contains key of participant IDs as initially defined ("MMDDYY-XX") and sequential IDs reported in manuscript
## - "recursion_full.csv" -- This contains long-format full dataset
## - "HC-datawide-forcoding - hc.datawide.csv" -- This contains wide-format initial and final coding for Highest Count task
## - "sara-HC-datawide-forcoding - hc.datawide.csv" -- This contains additional coding for number of reminders during Highest Count task
## - "HCdata_jccoding.csv" -- This contains additional coding for error sequences during Highest Count task


rm(list = ls())
library(tidyverse)
library(magrittr)

##adding tidylog for transparency
library(tidylog)

# Read in data ====
# original data
full.data <- read.csv('data/raw data/recursion_full.csv', na.strings=c(""," ","NA", "NA "))

# Exclusions ====

## Removing individual participants for failing next number task
# final decision: remove kids who fail 1 trial out of 3 trials, with no additional information about whether E reran the failed trial. Removing 8 kids in total
# added 022516-ML on 2018-09-03 to the exclusion list. kid had NA data for wcn practice trials and 0 correct on test. assume kid failed wcn.
full.data %<>%
  mutate(ExclusionGroup = ifelse(LadlabID == "infinity-032"| LadlabID == "infinity-047"|
                                   LadlabID == "infinity-060" | LadlabID == "infinity-089"|
                                   LadlabID == "infinity-093" | LadlabID == "infinity-069"|
                                   LadlabID == "infinity-009"| LadlabID == "infinity-091"|
                                   LadlabID == "infinity-025", "fail wcn", levels(ExclusionGroup)[ExclusionGroup]),
         ExclusionGroup = as.factor(ExclusionGroup))

## Exclusion ns and reasons
# Subject counts
table(distinct(full.data, LadlabID, ExclusionGroup)$ExclusionGroup)

## Filter out exclusions
# Final data set
full.data %<>%
  dplyr::filter(ExclusionGroup == "include")

# Clean up variables ====
full.data %<>% mutate(Sex = tolower(Sex),
                      Lang1 = tolower(Lang1),
                      Bilingual=tolower(str_sub(Bilingual, 1,1)),
                      Location=tolower(Location),
                      AudioRecorded=tolower(str_sub(AudioRecorded, 1, 1)),
                      AgeGroup = droplevels(AgeGroup),
                      InfinityInterviewDone=tolower(str_sub(InfinityInterviewDone,1,1)),
                      TaskType=tolower(TaskType))


# Highest Count Coding ====
## productivity, fhc, ihc coding from pc, jc, and rms

## Read in data and keep final coding decisions
hc.data <- read.csv('data/raw data/recursion_hc_coding_wide.csv') %>%
  dplyr::select(LadlabID, prod_tomerge, ihc_final, fhc_final, dce, why_productive, suptimes.final=suptimes_final,
                ProductivityStrict)

## add to full dataframe
full.data <- dplyr::left_join(full.data, hc.data, by = "LadlabID")

## Rename variables and factor levels
full.data %<>%
  dplyr::rename(Productivity = prod_tomerge, 
                IHC = ihc_final, 
                FHC = fhc_final, 
                DCE = dce)%>%
  dplyr::mutate(Productivity = factor(Productivity, levels = c("nonprod", "prod"), 
                                      labels = c("Nonproductive", "Productive")),
                ProductivityStrict = factor(ProductivityStrict, levels = c("nonprod", "prod"), 
                                      labels = c("Nonproductive", "Productive")),
                IHC = ifelse(IHC > 99, 99, IHC), # max is 99
                FHC = ifelse(FHC > 99, 99, FHC),
                delta.hc = FHC-IHC) %>%
  mutate(Productivity.tertiary = ifelse(IHC >= 99, "Productive (IHC \u2265 99)", 
                                        ifelse(Productivity == "Productive", "Productive (IHC < 99)", "Nonproductive")),
         ProductivityStrict.tertiary = ifelse(IHC >= 99, "Productive (IHC \u2265 99)", 
                                        ifelse(ProductivityStrict == "Productive", "Productive (IHC < 99)", "Nonproductive"))) %>%
  mutate(Productivity.tertiary = as.factor(Productivity.tertiary),
         ProductivityStrict.tertiary = as.factor(ProductivityStrict.tertiary),
         AgeMonths = floor(Age*12))

## do a quick check to make sure we have the same SIDs in each
unique.full <- as.vector(unique(full.data$LadlabID))
unique.hc <- as.vector(unique(hc.data$LadlabID))
tmp <- hc.data %>%
  dplyr::filter(LadlabID %in% unique.full)
### good to go - the only kids who are not included in full data are the ones who we excluded
subjectlist <- unique(full.data$LadlabID)

## Add in coding for reminder prompts and recovery from reminders
reminders.data <- read.csv('data/raw data/recursion_hc_reminders_wide.csv') %>%
  dplyr::select(LadlabID, reminders.total, reminders.recovered)
full.data <- dplyr::left_join(full.data, reminders.data, by="LadlabID")

## Read in data file with counting errors individually labeled
### This is wide format separate long data frame for highst count errors, for analysis and Fig 2
hc.errorscoded <- read.csv('data/raw data/recursion_hc_errors_long.csv', stringsAsFactors = F)
hc.errorscoded %<>% filter(LadlabID %in% subjectlist)

### Replace subject IDs
hc.errorscoded %<>% mutate(SortedID = fct_reorder(LadlabID, prod_tomerge, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, fhc, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, dce, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, ihc, min)) %>%
  mutate(subID = factor(as.numeric(SortedID), ordered = F))

## Add subject IDs to full.data
full.data %<>% left_join(dplyr::select(hc.errorscoded, LadlabID, subID), by='LadlabID') %>%
  unique()

## Next Number ----
wcn.data <- full.data %>%
  filter(Task == "WCN", TaskType == "immediate") %>%
  dplyr::select(LadlabID, Task, TaskType, TaskItem, Response, Accuracy, 
         Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary,
         Age, AgeGroup, AgeMonths, IHC, FHC, DCE) %>%
  mutate(Response_num = as.numeric(as.character(Response)), 
         TaskItem_num = as.numeric(as.character(TaskItem))) %>%
  mutate(Response_num =  replace_na(Response_num, 0),
         Accuracy = replace_na(Accuracy, 0)) %>%
  mutate(Accuracy_check = ifelse(Response_num == (TaskItem_num + 1), 1, 0), 
         Accuracy_valid = ifelse(Accuracy == Accuracy_check, TRUE, FALSE))

# + Validate
validate <- function(){
  validation <- wcn.data %>%
    filter(Accuracy_valid == FALSE)
  if(length(validation$subID) > 0) {
    print("WARNING: CHECK CODING")
  } else {
    print("All coding correct")
  }
}
validate() # ok

# Add overall accuracy to the dataframe.
wcn.accuracy <- wcn.data %>% 
  group_by(LadlabID) %>%
  mutate(wcnscore=sum(Accuracy)) %>%
  dplyr::select(LadlabID, wcnscore) %>%
  unique()

wcn.data <- left_join(wcn.data, wcn.accuracy, by="LadlabID")

# Code if WCN trial is Within / beyond IHC
wcn.data %<>%
  mutate(WithinOutsideIHC = ifelse(TaskItem_num < IHC, "within", "outside"))

# Filter out just the trials for analysis (without momentum)
# And recode item type as decade transition or mid decade
data.wcn <- wcn.data %>%
  filter(TaskType == "immediate")%>%
  mutate(TaskItem_type= ifelse(mod(TaskItem_num,10)==9, "Item Type: Decade transition", "Item Type: Mid-decade")) %>%
  mutate(TaskItem_type_ordered = ordered(TaskItem_type, levels=c("Item Type: Mid-decade", "Item Type: Decade transition")))

# Infinity categories ----
full.data %<>% 
  mutate(Category = case_when(
    SuccessorKnower==0 & EndlessKnower==0 ~ "A Non-knower",
    SuccessorKnower==1 & EndlessKnower==0 ~ "B Successor-only",
    SuccessorKnower==0 & EndlessKnower==1 ~ "C Endless-only",
    SuccessorKnower==1 & EndlessKnower==1 ~ "D Full-knower"),
    InfinityKnower = factor(ifelse(SuccessorKnower==1 & EndlessKnower==1, 1, 0), levels=c(0,1), labels=c('None or partial', 'Full Infinity')),
    NonKnower = factor(ifelse(SuccessorKnower==0 & EndlessKnower==0, 1, 0), levels=c(0,1), labels=c('Some knowledge', 'No knowledge')))

# Final checks ====
# Remove identifiable info, unused/duplicate variables, exploratory variables, and rename
data.full <- full.data %>% 
  dplyr::select(-c("DOT", "ExclusionGroup", "PGroup.final", "InfinityStage", 
                   "EndlessPartial","IsProductive.noerror","IsProductive.final",
                   "HCGroup.spontaneous.noerror", "HCGroup.final", "HCGroup.best",
                   "DecadeCounter", "DecadeKnowledge"))
data.hcerrors <- hc.errorscoded %>% dplyr::select(-"SortedID")
# Make dataframe for subject-level highest count measurements (without errors)
data.hcunique <- full.data %>%
  dplyr::distinct(LadlabID, Sex, Age, AgeGroup, AgeMonths, HCReceivedSupport, IHC, DCE, FHC, Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary)
# Make long dataframe for item-level Next Number data
data.wcn.long <- data.wcn %>%
  dplyr::select(c(LadlabID, IHC, FHC, Age, 
                  Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary,
                  TaskItem_num, TaskItem_type, Accuracy, WithinOutsideIHC)) %>%
  mutate(IHC = as.integer(IHC), LadlabID=as.factor(LadlabID), TaskItem_type=as.factor(TaskItem_type))
data.wcn.wide <- data.wcn.long %>% group_by(LadlabID, IHC, Age, Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary) %>%
  summarise(score=sum(Accuracy), perc=mean(Accuracy)) %>% ungroup()

## Reorder variables to make more sense ====
data.full %<>%
  select(LadlabID, subID, Age, AgeMonths, AgeGroup, Sex, Bilingual, Lang1, Lang2, Location, Experimenter, Enterer, AudioRecorded,
           Notes, Task, TaskType, TaskItem, Response,Accuracy,
           HCReceivedSupport, InfinityInterviewDone, 
           IHC, FHC, DCE, suptimes.final, delta.hc, reminders.total, reminders.recovered,
           Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary,
           SuccessorKnower, EndlessKnower, Category, InfinityKnower, NonKnower)
data.hcerrors %<>%
  dplyr::select(LadlabID, subID, prod_tomerge:notes) %>%
  dplyr::rename(productivity=prod_tomerge)
  


# datamaid checks =====
# library(dataMaid)
# makeDataReport(data.full, replace=T, file="data/dataMaid reports/full-dataMaid.Rmd")
# makeDataReport(data.wcn.long, replace=T, file="data/dataMaid reports/nn-long-dataMaid.Rmd")
# makeDataReport(data.wcn.wide, replace=T, file="data/dataMaid reports/nn-wide-dataMaid.Rmd")
# makeDataReport(data.hcunique, replace=T, file="data/dataMaid reports/hc-unique-dataMaid.Rmd")
# makeDataReport(data.hcerrors, replace=T, file="data/dataMaid reports/hc-errors-dataMaid.Rmd")


# Export ====
# Export to .RData
save(data.full, data.hcunique, data.hcerrors, 
     data.wcn, data.wcn.long, data.wcn.wide, 
     file="data/CountingToInfinity-data.RData")

# Export to csv
write.csv(data.full, "data/data-full.csv")
write.csv(data.hcunique, "data/data-hc-unique.csv")
write.csv(data.hcerrors, "data/data-hc-errors.csv")
write.csv(data.wcn.long, "data/data-nn-long.csv")
write.csv(data.wcn.wide, "data/data-nn-wide.csv")
