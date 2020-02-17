rm(list = ls())
library(tidyverse)
library(magrittr)

# Read in data ====
# original data
full.data <- read.csv('raw data/recursion_full.csv', na.strings=c(""," ","NA", "NA "))
# final decision: remove kids who fail 1 trial out of 3 trials, with no additional information about whether E reran the failed trial. Removing 8 kids in total
# added 033516-ML on 3018-09-03 to the exclusion list. kid had NA data for wcn practice trials and 0 correct on test. assume kid failed wcn.
full.data %<>%
  mutate(ExclusionGroup = ifelse(LadlabID == "022616-JM"| LadlabID == "030216-AD"|
                                   LadlabID == "031516-A" | LadlabID == "041316-AR"|
                                   LadlabID == "041316-VN" | LadlabID == "032216-RC"|
                                   LadlabID == "012316-BO"| LadlabID == "041316-NC"|
                                   LadlabID == "022516-ML", "fail wcn", levels(ExclusionGroup)[ExclusionGroup]),
         ExclusionGroup = as.factor(ExclusionGroup))
full.data %>%
  filter(LadlabID == "022616-JM")
# Subject counts
table(distinct(full.data, LadlabID, ExclusionGroup)$ExclusionGroup)
# Final data set
full.data %<>%
  dplyr::filter(ExclusionGroup == "include")

# Highest Count Coding ====
# productivity, fhc, ihc coding from pc, jc, and rms

# Read in data and keep final coding decisions
hc.data <- read.csv('raw data/HC-datawide-forcoding - hc.datawide.csv') %>%
  dplyr::select(LadlabID, prod_tomerge, ihc_final, fhc_final, dce, why_productive, suptimes.final=suptimes_final,
                ProductivityStrict)
# add to full dataframe
full.data <- dplyr::left_join(full.data, hc.data, by = "LadlabID")
# Rename variables and factor levels
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
                FHC = ifelse(FHC > 99, 99, FHC)) %>%
  mutate(Productivity.tertiary = ifelse(IHC >= 99, "Productive (IHC \u2265 99)", 
                                        ifelse(Productivity == "Productive", "Productive (IHC < 99)", "Nonproductive")),
         ProductivityStrict.tertiary = ifelse(IHC >= 99, "Productive (IHC \u2265 99)", 
                                        ifelse(ProductivityStrict == "Productive", "Productive (IHC < 99)", "Nonproductive"))) %>%
  mutate(Productivity.tertiary = as.factor(Productivity.tertiary),
         ProductivityStrict.tertiary = as.factor(ProductivityStrict.tertiary),
         AgeMonths = floor(Age*12))

# do a quick check to make sure we have the same SIDs in each
unique.full <- as.vector(unique(full.data$LadlabID))
unique.hc <- as.vector(unique(hc.data$LadlabID))
tmp <- hc.data %>%
  dplyr::filter(LadlabID %in% unique.full)
# good to go - the only kids who are not included in full data are the ones who we excluded
subjectlist <- unique(full.data$LadlabID)

# Add in coding for reminder prompts and recovery from reminders
reminders.data <- read.csv('raw data/sara-HC-datawide-forcoding - hc.datawide.csv') %>%
  dplyr::select(LadlabID, reminders.total, reminders.recovered)
full.data <- dplyr::left_join(full.data, reminders.data, by="LadlabID")

# Add in productivity gradient calculation
full.data %<>%
  mutate(delta.hc = FHC-IHC, 
         prod.gradient = case_when(
           IHC>=99 ~1,
           IHC <99 & FHC>=100 ~ 1,
           IHC <99 & FHC<100 ~delta.hc/(99-IHC)))

# Read in data file with counting errors individually labeled
# This is wide format separate long data frame for highst count errors, for analysis and Fig 2
hc.errorscoded <- read.csv('raw data/HCdata_jccoding.csv', stringsAsFactors = F)
hc.errorscoded %<>% filter(LadlabID %in% subjectlist)

# Replace subject IDs
hc.errorscoded %<>% mutate(SortedID = fct_reorder(LadlabID, prod_tomerge, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, fhc, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, dce, min)) %>%
  mutate(SortedID = fct_reorder(SortedID, ihc, min)) %>%
  mutate(subID = factor(as.numeric(SortedID), ordered = F))

# Add subject IDs to full.data
full.data %<>% left_join(dplyr::select(hc.errorscoded, LadlabID, subID), by='LadlabID') %>%
  unique()

## Next Number ----
wcn.data <- full.data %>%
  filter(Task == "WCN", TaskType == "immediate") %>%
  dplyr::select(subID, Task, TaskType, TaskItem, Response, Accuracy, 
         Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary,
         prod.gradient, Age, AgeGroup, AgeMonths, IHC, FHC, DCE) %>%
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
  group_by(subID) %>%
  mutate(wcnscore=sum(Accuracy)) %>%
  dplyr::select(subID, wcnscore) %>%
  unique()

wcn.data <- left_join(wcn.data, wcn.accuracy, by="subID")

# + Code if WCN trial is Within / beyond IHC ----
# first, get initial highest count for each kiddo
# Make a lookup table with SID and initial highest count
lookup <- full.data %>%
  distinct(subID, IHC)

# For each trial, check the number queried.
# If number queried is above the child's initial highest count, marks that trial as beyond count range.
determine_count_range <- function(df) {
  tmp <- df
  for (row in 1:nrow(tmp)) {
    sub = as.character(tmp[row, "subID"])
    count_range = as.numeric(as.character(subset(lookup, subID == sub)$IHC))
    tmp[row, "IHC"] = as.numeric(as.character(count_range))
    if (tmp[row, "TaskItem_num"] > count_range) {
      tmp[row, "WithinOutsideIHC"] = "outside"
    } else {
      tmp[row, "WithinOutsideIHC"] = "within"
    }
  }
  return(tmp)
}

# Run for wcn data
wcn.data <- determine_count_range(wcn.data)
summary(wcn.data$Accuracy_valid) # ok

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

# Save and Export ----
# Remove identifiable info & unnecessary variables & Rename
data.full <- full.data %>% dplyr::select(-c("LadlabID", "DOT", "ExclusionGroup"))
data.hcerrors <- hc.errorscoded %>% dplyr::select(-c("LadlabID", "SortedID"))
# Make dataframe for subject-level highest count measurements (without errors)
data.hcunique <- full.data %>%
  dplyr::distinct(subID, Gender, Age, AgeGroup, AgeMonths, HCReceivedSupport, IHC, DCE, FHC, Productivity, Productivity.tertiary)
# Make long dataframe for item-level Next Number data
data.wcn.long <- data.wcn %>%
  dplyr::select(c(subID, IHC, FHC, Age, 
                  Productivity, Productivity.tertiary, ProductivityStrict, ProductivityStrict.tertiary,
                  TaskItem_num, TaskItem_type, Accuracy, WithinOutsideIHC)) %>%
  mutate(IHC = as.integer(IHC), subID=as.factor(subID), TaskItem_type=as.factor(TaskItem_type))
data.wcn.wide <- data.wcn.long %>% group_by(subID, IHC, Age, Productivity, Productivity.tertiary) %>%
  summarise(score=sum(Accuracy), perc=mean(Accuracy)) %>% ungroup()
# Export
save(data.full, data.hcunique, data.hcerrors, 
     data.wcn, data.wcn.long, data.wcn.wide, 
     file="CountingToInfinity-data.RData")
