---
title: "RecursionAnalysis"
author: "Junyi Chu, Rose M. Schneider, Pierina Cheung"
date: "2019-05-06"
output: 
  html_document:
    toc: true
    toc_depth: 4
    toc_float: true
    keep_md: true
---

# Setup


R Version for citation is: R version 3.5.1 (2018-07-02).

## Loading data

```r
# original data
full.data <- read.csv("data/recursion_full.csv", na.strings = c("", " ", "NA", "NA "))
```


RMS original code for checking who failed practice - do not need to run. EVAL set to FALSE

```r
# check how many failed both practice trials
x <- full.data %>% filter(Task == "WCN" & (TaskItem == 1 | TaskItem == 5)) %>% group_by(LadlabID) %>% 
    summarise(sum = sum(Accuracy)) %>% filter(sum != 3)

# just hardcoding kids because it's easier than going back to the full data frame
# These kids got 1 right, 5 wrong:
one.corr <- as.vector(c("013316-BO", "033616-JM", "030316-ED", "030817-ZI", "031516-A", 
    "033316-JH", "033316-RC", "040317-AL", "040317-SL", "041316-AR", "041316-NC", 
    "041316-VN", "063416-MC"))

five.corr <- as.vector("050617-Z1")

zero.corr <- as.vector(c("030316-AD", "040616-K"))
```

Exclude those who failed the practice trials on What Comes Next Task

```r
# final decision: remove kids who fail 1 trial out of 3 trials, with no
# additional information about whether E reran the failed trial. Removing 8 kids
# in total

# added 033516-ML on 3018-09-03 to the exclusion list. kid had NA data for wcn
# practice trials and 0 correct on test. assume kid failed wcn.

full.data %<>% mutate(ExclusionGroup = ifelse(LadlabID == "022616-JM" | LadlabID == 
    "030216-AD" | LadlabID == "031516-A" | LadlabID == "041316-AR" | LadlabID == 
    "041316-VN" | LadlabID == "032216-RC" | LadlabID == "012316-BO" | LadlabID == 
    "041316-NC" | LadlabID == "022516-ML", "fail wcn", levels(ExclusionGroup)[ExclusionGroup]), 
    ExclusionGroup = as.factor(ExclusionGroup))

# check. good full.data %>% filter(LadlabID == '033616-JM')
```


Reasons for exclusion and their numbers

```r
full.data %>% dplyr::filter(ExclusionGroup != "include") %>% dplyr::distinct(LadlabID, 
    .keep_all = TRUE) %>% dplyr::group_by(ExclusionGroup) %>% dplyr::summarize(countN = dplyr::n_distinct(LadlabID)) %>% 
    kable()
```



ExclusionGroup         countN
--------------------  -------
age                         5
dnf infinity                4
dnf WCN                     6
experimenter error          3
fail wcn                    9
L1 not english              4
parent interference         1
pilot order                 3

Let's remove anyone who should not be included in the final dataset.

```r
full.data %<>% dplyr::filter(ExclusionGroup == "include")
```


Now, add in the Productivity classification, IHC, and FHC from PC, JC, and RMS coding


```r
# productivity, fhc, ihc coding from pc, jc, and rms
hc.data <- read.csv("data/HC-datawide-forcoding - hc.datawide.csv") %>% dplyr::select(LadlabID, 
    prod_tomerge, ihc_tomerge, fhc_tomerge, dce, why_productive, sup.times)

full.data <- dplyr::left_join(full.data, hc.data, by = "LadlabID")

full.data %<>% dplyr::rename(Productivity = prod_tomerge, IHC = ihc_tomerge, FHC = fhc_tomerge, 
    DCE = dce) %>% dplyr::mutate(Productivity = factor(Productivity, levels = c("nonprod", 
    "prod"), labels = c("Nonproductive", "Productive")), IHC = ifelse(IHC > 100, 
    100, IHC), FHC = ifelse(FHC > 100, 100, FHC)) %>% mutate(Productivity.tertiary = ifelse(IHC >= 
    99, "Productive (IHC ≥ 99)", ifelse(Productivity == "Productive", "Productive (IHC < 99)", 
    "Nonproductive")))


# do a quick check to make sure we have the same SIDs in each
unique.full <- as.vector(unique(full.data$LadlabID))
unique.hc <- as.vector(unique(hc.data$LadlabID))

tmp <- hc.data %>% dplyr::filter(LadlabID %!in% unique.full)

# good to go - the only kids who are not included in full data are the ones who
# we excluded
```

Add in coding for reminder prompts and recovery from reminders

```r
reminders.data <- read.csv("data/sara-HC-datawide-forcoding - hc.datawide.csv") %>% 
    dplyr::select(LadlabID, reminders.total, reminders.recovered)
full.data <- dplyr::left_join(full.data, reminders.data, by = "LadlabID")
```

## Post-exclusion summary
Number of kids by age group and average age

```r
full.data %>% dplyr::group_by(AgeGroup) %>% dplyr::summarize(sumAge = n_distinct(LadlabID)) %>% 
    kable()
```



AgeGroup    sumAge
---------  -------
4-4.5y          32
4.5-5y          29
5-5.5y          32
5.5-6y          29

```r
full.data %>% dplyr::distinct(LadlabID, .keep_all = TRUE) %>% dplyr::summarize(minAge = min(Age), 
    maxAge = max(Age), meanAge = mean(Age), sdAge = sd(Age)) %>% kable(digits = 3)
```



 minAge   maxAge   meanAge   sdAge
-------  -------  --------  ------
      4     5.99     4.998   0.571

Number of kids who were classified as decade productive & nonproductive

```r
full.data %>% dplyr::distinct(LadlabID, Productivity, Age) %>% dplyr::group_by(Productivity) %>% 
    dplyr::summarise(n = n(), meanage = mean(Age, na.rm = TRUE), sdage = sd(Age, 
        na.rm = TRUE), minage = min(Age, na.rm = TRUE), maxage = max(Age, na.rm = TRUE)) %>% 
    kable(digits = 3)
```



Productivity      n   meanage   sdage   minage   maxage
--------------  ---  --------  ------  -------  -------
Nonproductive    49     4.604   0.419     4.00     5.61
Productive       73     5.262   0.505     4.18     5.99

Number of kids for the 3-way productivity classification

```r
full.data %>% dplyr::distinct(LadlabID, Productivity.tertiary, Age) %>% dplyr::group_by(Productivity.tertiary) %>% 
    dplyr::summarise(n = n(), meanage = mean(Age, na.rm = TRUE), sdage = sd(Age, 
        na.rm = TRUE), minage = min(Age, na.rm = TRUE), maxage = max(Age, na.rm = TRUE)) %>% 
    kable(digits = 3)
```



Productivity.tertiary     n   meanage   sdage   minage   maxage
----------------------  ---  --------  ------  -------  -------
Nonproductive            49     4.604   0.419     4.00     5.61
Productive (IHC < 99)    41     5.221   0.482     4.25     5.99
Productive (IHC ≥ 99)    32     5.316   0.536     4.18     5.99

Reasons for productivity classification

```r
full.data %>% dplyr::distinct(LadlabID, Productivity.tertiary, why_productive) %>% 
    dplyr::group_by(Productivity.tertiary, why_productive) %>% tally() %>% spread(Productivity.tertiary, 
    n)
```

```
## # A tibble: 7 x 4
##   why_productive          Nonproductive `Productive (IHC… `Productive (IHC…
##   <fct>                           <int>             <int>             <int>
## 1 count at least two dec…            NA                32                NA
## 2 count to 99 on their o…            NA                 9                32
## 3 more than 3 errors bef…             1                NA                NA
## 4 multiple errors before…             5                NA                NA
## 5 no improvement from dce            11                NA                NA
## 6 not enough improvement…             6                NA                NA
## 7 stopped mid decade                 26                NA                NA
```


Just for reference, this is the number of kids who switched classifications from PC, JC, RMS recode

```r
full.data %>% dplyr::filter(TaskType == "productivity") %>% droplevels() %>% dplyr::distinct(LadlabID, 
    Response, Productivity) %>% dplyr::mutate(Response = factor(Response, levels = c("nonprod", 
    "prod"), labels = c("Nonproductive", "Productive"))) %>% dplyr::mutate(changed_classification = ifelse((is.na(Response) & 
    Productivity == "Nonproductive"), "NA_toNonprod", ifelse((is.na(Response) & Productivity == 
    "Productive"), "NA_toProd", ifelse((Response == "Nonproductive" & Productivity == 
    "Productive"), "Nonprod_toProd", ifelse((Response == "Productive" & Productivity == 
    "Nonproductive"), "Prod_toNonprod", "no_change"))))) %>% dplyr::group_by(changed_classification) %>% 
    dplyr::summarise(n = n())
```

```
## # A tibble: 4 x 2
##   changed_classification     n
##   <chr>                  <int>
## 1 NA_toNonprod              35
## 2 NA_toProd                  1
## 3 no_change                 85
## 4 Nonprod_toProd             1
```

## Calculate Productivity gradient

```r
full.data %<>% mutate(delta.hc = FHC - IHC, prod.gradient = case_when(IHC >= 99 ~ 
    1, IHC < 99 & FHC >= 100 ~ 1, IHC < 99 & FHC < 100 ~ delta.hc/(99 - IHC)))
```

***
# Highest Count Descriptives

## Summary descriptives 
Average of IHC, DCE, and FHC for all kids

```r
full.data %>% dplyr::distinct(LadlabID, IHC, DCE, FHC, delta.hc, prod.gradient, sup.times) %>% 
    dplyr::summarise_at(c("IHC", "DCE", "FHC", "delta.hc", "prod.gradient", "sup.times"), 
        list(~mean(., na.rm = T), ~sd(., na.rm = T), ~median(., na.rm = T), ~min(., 
            na.rm = T), ~max(., na.rm = T), ~sum(!is.na(.)))) %>% gather(stat, val) %>% 
    separate(stat, into = c("var", "stat"), sep = "_") %>% spread(stat, val) %>% 
    dplyr::select(var, n = sum, mean, sd, median, min, max) %>% kable(digits = 3)
```



var                n     mean       sd   median   min   max
--------------  ----  -------  -------  -------  ----  ----
DCE               54   43.815   17.017     39.0    19    99
delta.hc         122   20.738   24.881      9.0     0    86
FHC              122   71.156   34.620     99.0     5   100
IHC              122   50.418   33.806     39.5     5   100
prod.gradient    122    0.624    0.443      1.0     0     1
sup.times         52    2.635    1.704      2.0     1     7

Similar data by decade productivity

```r
full.data %>% dplyr::distinct(LadlabID, Productivity, IHC, DCE, FHC, delta.hc, prod.gradient, 
    sup.times) %>% group_by(Productivity) %>% dplyr::summarise_at(c("IHC", "DCE", 
    "FHC", "delta.hc", "prod.gradient", "sup.times"), list(~mean(., na.rm = T), ~sd(., 
    na.rm = T), ~median(., na.rm = T), ~min(., na.rm = T), ~max(., na.rm = T), ~sum(!is.na(.)))) %>% 
    gather(stat, val, -Productivity) %>% separate(stat, into = c("var", "stat"), 
    sep = "_") %>% spread(stat, val) %>% dplyr::select(Productivity, var, n = sum, 
    mean, sd, median, min, max) %>% kable(digits = 3)
```



Productivity    var               n     mean       sd   median      min   max
--------------  --------------  ---  -------  -------  -------  -------  ----
Nonproductive   DCE              19   30.579    8.342       29   19.000    49
Nonproductive   delta.hc         49   10.000   15.967        0    0.000    86
Nonproductive   FHC              49   32.388   17.764       29    5.000   100
Nonproductive   IHC              49   22.388   14.704       15    5.000    77
Nonproductive   prod.gradient    49    0.119    0.185        0    0.000     1
Nonproductive   sup.times        20    1.350    0.587        1    1.000     3
Productive      DCE              35   51.000   16.234       49   29.000    99
Productive      delta.hc         73   27.945   27.190       31    0.000    78
Productive      FHC              73   97.178    9.719      100   49.000   100
Productive      IHC              73   69.233   29.712       65   14.000   100
Productive      prod.gradient    73    0.963    0.130        1    0.286     1
Productive      sup.times        32    3.438    1.684        3    1.000     7

Again by 3-way decade productivity

```r
full.data %>% dplyr::distinct(LadlabID, Productivity.tertiary, IHC, DCE, FHC, delta.hc, 
    prod.gradient, sup.times) %>% group_by(Productivity.tertiary) %>% dplyr::summarise_at(c("IHC", 
    "DCE", "FHC", "delta.hc", "prod.gradient", "sup.times"), list(~mean(., na.rm = T), 
    ~sd(., na.rm = T), ~median(., na.rm = T), ~min(., na.rm = T), ~max(., na.rm = T), 
    ~sum(!is.na(.)))) %>% gather(stat, val, -Productivity.tertiary) %>% separate(stat, 
    into = c("var", "stat"), sep = "_") %>% spread(stat, val) %>% dplyr::select(Productivity.tertiary, 
    var, n = sum, mean, median, sd, min, max) %>% kable(digits = 3)
```



Productivity.tertiary   var               n     mean   median       sd      min    max
----------------------  --------------  ---  -------  -------  -------  -------  -----
Nonproductive           DCE              19   30.579       29    8.342   19.000     49
Nonproductive           delta.hc         49   10.000        0   15.967    0.000     86
Nonproductive           FHC              49   32.388       29   17.764    5.000    100
Nonproductive           IHC              49   22.388       15   14.704    5.000     77
Nonproductive           prod.gradient    49    0.119        0    0.185    0.000      1
Nonproductive           sup.times        20    1.350        1    0.587    1.000      3
Productive (IHC < 99)   DCE              34   49.588       49   14.130   29.000     89
Productive (IHC < 99)   delta.hc         41   49.756       51   14.778   20.000     78
Productive (IHC < 99)   FHC              41   95.000      100   12.606   49.000    100
Productive (IHC < 99)   IHC              41   45.244       49   15.603   14.000     79
Productive (IHC < 99)   prod.gradient    41    0.934        1    0.168    0.286      1
Productive (IHC < 99)   sup.times        32    3.438        3    1.684    1.000      7
Productive (IHC ≥ 99)   DCE               1   99.000       99       NA   99.000     99
Productive (IHC ≥ 99)   delta.hc         32    0.000        0    0.000    0.000      0
Productive (IHC ≥ 99)   FHC              32   99.969      100    0.177   99.000    100
Productive (IHC ≥ 99)   IHC              32   99.969      100    0.177   99.000    100
Productive (IHC ≥ 99)   prod.gradient    32    1.000        1    0.000    1.000      1
Productive (IHC ≥ 99)   sup.times         0      NaN       NA       NA      Inf   -Inf

## Histogram

### Fig 1

Plotting distribution of IHC, as a function of productivity (~ junyi's graph)

![](recursionAnalysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```
## Saving 7 x 5 in image
```

### by age
Plotting productivity as a function of age in months

![](recursionAnalysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```
## Saving 7 x 5 in image
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

## Distance between IHC and FHC

Restructure data to plot distance between IHC, DCE, and FHC

```r
hc.dev.data <- full.data %>% dplyr::select(LadlabID, Age, Productivity, IHC, DCE, 
    FHC, prod.gradient) %>% gather(hcprogression, hc, IHC:FHC) %>% mutate(hcprogression = factor(hcprogression, 
    levels = c("IHC", "DCE", "FHC"))) %>% dplyr::rename(`Highest Count Coding` = hcprogression)

hc.dev.prod <- subset(hc.dev.data, Productivity == "Productive")
hc.dev.nonprod <- subset(hc.dev.data, Productivity == "Nonproductive")
```

### Fig 3a/b
Separate graphs for productivity groups, sorted by ascending IHC. One participant was coded as non-productive despite having an FHC of 100 because they made more than 3 errors.
![](recursionAnalysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](recursionAnalysis_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

Number of kids who counted to 99+ spontaneously on IHC plus those whose FHC = 99+ without prompting

```r
# full.data %>% filter(IHC > 98) %>% distinct(LadlabID, IHC, FHC,
# HCReceivedSupport) %>% count() #n=33 but some kids made errors past IHC but < 3
# so need to account for that
full.data %>% filter(FHC > 98 & (is.na(HCReceivedSupport) | HCReceivedSupport != 
    1)) %>% distinct(LadlabID, IHC, FHC, HCReceivedSupport) %>% count()  #n =43
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1    42
```

## Decade prompts

Check coding sheet against 'supported.times' coding. Print conflicts. Use "sup.times" instead.
- 030116-PW & 041416-BF: Experimenter should not have provided prompts, multiple errors before first DCE
- 063416-MC: Experimenter's prompts are 100 and 110, should not count.

```r
full.data %>% filter(TaskItem == "times") %>% distinct(LadlabID, HCReceivedSupport, 
    TaskItem, Response, sup.times, Productivity) %>% mutate(Response = as.integer(levels(Response)[Response])) %>% 
    filter(is.na(sup.times) & !is.na(Response) | is.na(Response) & !is.na(sup.times) | 
        sup.times > Response | sup.times < Response) %>% kable()
```



LadlabID    HCReceivedSupport   TaskItem    Response   sup.times  Productivity  
----------  ------------------  ---------  ---------  ----------  --------------
030116-PW   1                   times              6          NA  Nonproductive 
041416-BF   0                   times              2          NA  Nonproductive 
062416-MC   0                   times              2          NA  Productive    

Average number of decade prompts provided. Productive counters first. Analyze from raw data:

```r
full.data %>% distinct(LadlabID, sup.times, Productivity.tertiary) %>% group_by(Productivity.tertiary) %>% 
    summarise(n = n(), n.sup = sum(sup.times > 0, na.rm = TRUE), mean = mean(sup.times, 
        na.rm = TRUE), sd = sd(sup.times, na.rm = TRUE), min = min(sup.times, na.rm = TRUE), 
        max = max(sup.times, na.rm = TRUE)) %>% kable(digits = 3)
```



Productivity.tertiary     n   n.sup    mean      sd   min    max
----------------------  ---  ------  ------  ------  ----  -----
Nonproductive            49      20   1.350   0.587     1      3
Productive (IHC < 99)    41      32   3.438   1.684     1      7
Productive (IHC ≥ 99)    32       0     NaN     NaN   Inf   -Inf

```r
# assume 0 = NA error in supported.times coding, should only count to 90 but one
# kid got prompted with 100 and 110 and times should be 0
```

## Reminder prompts
How many kids have coded data for receiving reminders?

```r
full.data %>% dplyr::select(LadlabID, Productivity, reminders.total, reminders.recovered) %>% 
    unique() %>% na.omit() %>% summarise(Ncoded = n(), total.min = min(reminders.total), 
    total.max = max(reminders.total), total.mean = mean(reminders.total), total.median = median(reminders.total))
```

```
##   Ncoded total.min total.max total.mean total.median
## 1     84         0         5    1.27381            1
```

How often did kids miss a reminder? Remember that the experiment ended when kids failed to recover from a reminder, unless that failure occured at a decade transition in which case the experimenter would provide a decade prompt and continue the experiment. 

```r
full.data %>% dplyr::select(LadlabID, Productivity, reminders.total, reminders.recovered) %>% 
    unique() %>% na.omit() %>% mutate(missed = reminders.total - reminders.recovered) %>% 
    summarise(Ncoded = n(), missed.min = min(missed), missed.max = max(missed), missed.mean = mean(missed), 
        missed.median = median(missed)) %>% kable(digits = 3)
```



 Ncoded   missed.min   missed.max   missed.mean   missed.median
-------  -----------  -----------  ------------  --------------
     84            0            1         0.179               0

```r
# look at proportion of kids who failed to recover from reminders.
full.data %>% dplyr::select(LadlabID, Productivity, reminders.total, reminders.recovered) %>% 
    unique() %>% na.omit() %>% mutate(missed = reminders.total - reminders.recovered) %>% 
    group_by(reminders.total) %>% summarise(Nkids = n(), N.kids.missed = sum(missed), 
    Perc.kids.missed = sum(missed)/n()) %>% kable(digits = 3)
```



 reminders.total   Nkids   N.kids.missed   Perc.kids.missed
----------------  ------  --------------  -----------------
               0      25               0              0.000
               1      31              11              0.355
               2      16               3              0.188
               3       7               0              0.000
               4       2               0              0.000
               5       3               1              0.333


# What Comes Next Descriptives

First check if Accuracy column in full.data is coded correctly. Good to go.

```r
wcn.data <- full.data %>% filter(Task == "WCN")

wcn.data %<>% mutate(Response_num = as.numeric(as.character(Response)), TaskItem_num = as.numeric(as.character(TaskItem)), 
    Accuracy_check = ifelse(Response_num == (TaskItem_num + 1), 1, 0), Accuracy_valid = ifelse(Accuracy == 
        Accuracy_check, TRUE, FALSE))

validate <- function() {
    validation <- wcn.data %>% filter(Accuracy_valid == FALSE)
    if (length(validation$LadlabID) > 0) {
        print("WARNING: CHECK CODING")
    } else {
        print("All coding correct")
    }
}

validate()
```

```
## [1] "All coding correct"
```

Add overall accuracy to the dataframe.

```r
wcn.accuracy <- wcn.data %>% filter(TaskType != "practice") %>% filter(TaskType == 
    "immediate") %>% group_by(LadlabID) %>% mutate(wcnscore = sum(Accuracy, na.rm = TRUE)) %>% 
    dplyr::select(LadlabID, wcnscore) %>% unique()

wcn.data <- left_join(wcn.data, wcn.accuracy, by = "LadlabID")
```

Immediate vs. Momentum trials: Children were provided with momentum trials if they got wrong on immediate trials. Check %trials where immediate = wrong, momentum = right

```r
wcn.wide <- wcn.data %>%
  filter(TaskType != "practice") %>%
  filter(TaskItem != 3) %>% # a trial on 3 for momentum that doesn't exist for immediate
  droplevels()%>%
  dplyr::select(LadlabID, Age, AgeGroup, TaskType, TaskItem, Accuracy, Productivity, prod.gradient) %>%
  spread(TaskType, Accuracy)

# data check: some kids got 1 for immediate but 0 for momentum or 1 for immediate and 1 for momentum (N = 5).Keeping them. 
## for reference, pulling out these kids below
full.data %>%
  filter(Task == "WCN", 
         TaskType == "momentum" | TaskType == "immediate")%>%
  dplyr::select(LadlabID, Age, AgeGroup, TaskType, TaskItem, Accuracy) %>%
  spread(TaskType, Accuracy)%>%
  mutate(issue_immediate1Momentum0 = ifelse(immediate == 1 & momentum == 0, TRUE, FALSE), 
         issue_immediate1Momentum1 = ifelse(immediate == 1 & momentum == 1, TRUE, FALSE))%>%
  filter(issue_immediate1Momentum0 == TRUE | 
           issue_immediate1Momentum1 == TRUE)
```

```
##    LadlabID  Age AgeGroup TaskItem immediate momentum
## 1 011216-WB 4.44   4-4.5y       59         1        1
## 2 022616-AG 4.32   4-4.5y       37         1        1
## 3 031616-RP 4.84   4.5-5y       23         1        1
## 4 041316-CC 4.36   4-4.5y       62         1        0
## 5 111117-VK 5.87   5.5-6y       29         1        1
##   issue_immediate1Momentum0 issue_immediate1Momentum1
## 1                     FALSE                      TRUE
## 2                     FALSE                      TRUE
## 3                     FALSE                      TRUE
## 4                      TRUE                     FALSE
## 5                     FALSE                      TRUE
```

```r
# how many kids show improved performance
xtabs(~immediate + momentum, data = wcn.wide, na.action = na.pass, exclude = NULL)
```

```
##          momentum
## immediate   0   1 <NA>
##      0    263 174   13
##      1      1   4  520
##      <NA>   1   0    0
```

```r
# 191 / 1048 trials = ~ 18%. NOTE % not by kids but by trials.
```

## Percent Correct on WCN 

```r
wcn.data %>% dplyr::distinct(LadlabID, wcnscore) %>% dplyr::summarise(n = n(), avg.wcn = mean(wcnscore)/8, 
    sd.wcn = sd(wcnscore)/8) %>% kable(digits = 3)
```



   n   avg.wcn   sd.wcn
----  --------  -------
 122     0.538    0.342

```r
# Productivity
wcn.data %>% dplyr::distinct(LadlabID, Productivity, wcnscore) %>% group_by(Productivity) %>% 
    dplyr::summarise(n = n(), avg.wcn = mean(wcnscore)/8, sd.wcn = sd(wcnscore)/8) %>% 
    kable(digits = 3)
```



Productivity      n   avg.wcn   sd.wcn
--------------  ---  --------  -------
Nonproductive    49     0.278    0.261
Productive       73     0.712    0.274

```r
# Productivity 3-ways
wcn.data %>% dplyr::distinct(LadlabID, Productivity.tertiary, wcnscore) %>% group_by(Productivity.tertiary) %>% 
    dplyr::summarise(n = n(), avg.wcn = mean(wcnscore)/8, sd.wcn = sd(wcnscore)/8) %>% 
    kable(digits = 3)
```



Productivity.tertiary     n   avg.wcn   sd.wcn
----------------------  ---  --------  -------
Nonproductive            49     0.278    0.261
Productive (IHC < 99)    41     0.561    0.255
Productive (IHC ≥ 99)    32     0.906    0.145

T-test

```r
wcn.data %>% dplyr::distinct(LadlabID, Productivity, wcnscore) %>% t.test(wcnscore ~ 
    Productivity, data = .)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  wcnscore by Productivity
## t = -8.8423, df = 106.51, p-value = 2.173e-14
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -4.253060 -2.695221
## sample estimates:
## mean in group Nonproductive    mean in group Productive 
##                     2.22449                     5.69863
```


Plotting %corr on WCN as function of productivity

```r
wcn.data %>% dplyr::filter(TaskType == "immediate") %>% dplyr::group_by(LadlabID, 
    Productivity, prod.gradient) %>% dplyr::summarize(avg.wcn = mean(Accuracy, na.rm = TRUE), 
    sd.wcn = sd(Accuracy, na.rm = TRUE)) %>% ggplot(aes(x = Productivity, y = avg.wcn, 
    fill = factor(Productivity))) + stat_summary(fun.y = mean, position = position_dodge(width = 0.95), 
    geom = "bar", alpha = 0.8, colour = "black") + geom_violin(alpha = 0.3) + stat_summary(fun.data = mean_se, 
    geom = "errorbar", position = position_dodge(width = 0.9), width = 0.3) + # scale_fill_discrete(name = 'Productivity') +
scale_fill_manual(name = "Productivity", values = mypalette, guide = "none") + scale_colour_brewer(palette = "Greys") + 
    ylab("Proportion Correct") + xlab("Productivity") + theme_bw(base_size = 13) + 
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(text = element_text(size = 13)) + ylim(0, 1)
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
ggsave("graphs/wcn-percentcorr.png")
```

```
## Saving 7 x 5 in image
```

### Fig 3
Jess graph

```r
fig3.data <- wcn.data %>%
  filter(TaskType == "immediate")%>%
  mutate(TaskItem_type= ifelse(mod(TaskItem_num,10)==9, "Decade transition", "Mid-decade")) %>%
  mutate(TaskItem_type_ordered = ordered(TaskItem_type, levels=c("Mid-decade", "Decade transition")))

fig3.data %>%
  group_by(TaskItem_type_ordered, TaskItem, Productivity.tertiary)%>%
   summarise(mean = mean(Accuracy, na.rm = TRUE), 
            n = n(), 
            sd = sd(Accuracy, na.rm = TRUE), 
            se = sd/sqrt(n)) %>%
  ggplot(aes(x = factor(TaskItem), y = mean, colour = Productivity.tertiary, group= Productivity.tertiary)) +
  geom_point(size = 3.5) + 
  geom_line(size = .7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0, size = .5) +
  facet_grid(~TaskItem_type_ordered, scales = "free_x") +#, space = "free_x") +
  theme_bw(base_size = 13) + 
  scale_colour_manual(values = mypalette) +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(x = "Task item", y = "Mean performance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](recursionAnalysis_files/figure-html/fig3-1.png)<!-- -->

```r
        #, strip.text=element_text(margin=margin(t=5, b=5, l=30, r=30)))
ggsave('graphs/wcn-trial-accuracy.png',
       width=7, height=4)
```

Get summary statistics by productivity and item type

Productivity.tertiary     mean      sd    n
----------------------  ------  ------  ---
Nonproductive            0.278   0.261   49
Productive (IHC < 99)    0.562   0.254   41
Productive (IHC ≥ 99)    0.906   0.145   32



Productivity.tertiary   TaskItem_type         mean      sd    n
----------------------  ------------------  ------  ------  ---
Nonproductive           Decade transition    0.102   0.228   49
Nonproductive           Mid-decade           0.337   0.324   49
Productive (IHC < 99)   Decade transition    0.244   0.356   41
Productive (IHC < 99)   Mid-decade           0.669   0.278   41
Productive (IHC ≥ 99)   Decade transition    0.812   0.354   32
Productive (IHC ≥ 99)   Mid-decade           0.938   0.118   32

## Item magnitude
### Remove IHC=99

Build dataframe and mean-center IHC and age, and use weighted effect coding for Productivity.

```r
# Only for productive IHC < 99 and nonproductive
fig3.modelA.df <- fig3.data %>% filter(Productivity.tertiary != "Productive (IHC ≥ 99)") %>% 
    dplyr::select(c(LadlabID, IHC, Age, Productivity.tertiary, TaskItem_num, TaskItem_type, 
        Accuracy)) %>% mutate(Productivity.tertiary = as.factor(Productivity.tertiary), 
    IHC = as.integer(IHC), LadlabID = as.factor(LadlabID), TaskItem_type = as.factor(TaskItem_type)) %>% 
    mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale = TRUE)), age.c = as.vector(scale(Age, 
        center = TRUE, scale = TRUE)))
# weighted effect coding
wec <- mean(as.numeric(fig3.modelA.df$Productivity.tertiary) - 1)
contrasts(fig3.modelA.df$Productivity.tertiary) <- c(-wec, 1 - wec)
wec <- mean(as.numeric(fig3.modelA.df$TaskItem_type) - 1)
contrasts(fig3.modelA.df$TaskItem_type) <- c(-wec, 1 - wec)
# structure
str(fig3.modelA.df)
```

```
## 'data.frame':	720 obs. of  9 variables:
##  $ LadlabID             : Factor w/ 90 levels "010516-K4","011216-KD1",..: 1 1 1 1 1 1 1 1 2 2 ...
##  $ IHC                  : int  13 13 13 13 13 13 13 13 5 5 ...
##  $ Age                  : num  4.17 4.17 4.17 4.17 4.17 4.17 4.17 4.17 4 4 ...
##  $ Productivity.tertiary: Factor w/ 2 levels "Nonproductive",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.456 0.544
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Nonproductive" "Productive (IHC < 99)"
##   .. .. ..$ : NULL
##  $ TaskItem_num         : num  23 40 62 70 37 29 86 59 23 40 ...
##  $ TaskItem_type        : Factor w/ 2 levels "Decade transition",..: 2 2 2 2 2 1 2 1 2 2 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.75 0.25
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Decade transition" "Mid-decade"
##   .. .. ..$ : NULL
##  $ Accuracy             : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ IHC.c                : num  -1.05 -1.05 -1.05 -1.05 -1.05 ...
##  $ age.c                : num  -1.32 -1.32 -1.32 -1.32 -1.32 ...
```

#### Productivity effect

```r
fig3.modelA.ihc <- glmer(Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = fig3.modelA.df)
fig3.modelA.prod <- glmer(Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | 
    TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelA.df)
# LRT tests for productivity effect vs. base
anova(fig3.modelA.ihc, fig3.modelA.prod, test = "LRT")
```

```
## Data: fig3.modelA.df
## Models:
## fig3.modelA.ihc: Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
## fig3.modelA.prod: Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | TaskItem_num) + 
## fig3.modelA.prod:     (1 | LadlabID)
##                  Df    AIC    BIC  logLik deviance  Chisq Chi Df
## fig3.modelA.ihc   5 770.98 793.87 -380.49   760.98              
## fig3.modelA.prod  6 769.67 797.14 -378.84   757.67 3.3058      1
##                  Pr(>Chisq)  
## fig3.modelA.ihc              
## fig3.modelA.prod    0.06904 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


#### Item type (mid or cross decade)

```r
fig3.modelA.main <- glmer(Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + 
    age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelA.df)
# LRT test for itemtype main effect
anova(fig3.modelA.main, fig3.modelA.prod, test = "LRT")
```

```
## Data: fig3.modelA.df
## Models:
## fig3.modelA.prod: Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | TaskItem_num) + 
## fig3.modelA.prod:     (1 | LadlabID)
## fig3.modelA.main: Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c + 
## fig3.modelA.main:     (1 | TaskItem_num) + (1 | LadlabID)
##                  Df    AIC    BIC  logLik deviance  Chisq Chi Df
## fig3.modelA.prod  6 769.67 797.14 -378.84   757.67              
## fig3.modelA.main  7 759.09 791.14 -372.55   745.09 12.579      1
##                  Pr(>Chisq)    
## fig3.modelA.prod               
## fig3.modelA.main  0.0003901 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Test of interaction (n.s.): Regression testing for interaction of productivity and decade/non-decade item type on WCN accuracy

```r
fig3.modelA.full <- glmer(Accuracy ~ Productivity.tertiary + TaskItem_type + Productivity.tertiary:TaskItem_type + 
    IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelA.df)
# LRT test for interaction effect
anova(fig3.modelA.full, fig3.modelA.main, test = "LRT")
```

```
## Data: fig3.modelA.df
## Models:
## fig3.modelA.main: Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c + 
## fig3.modelA.main:     (1 | TaskItem_num) + (1 | LadlabID)
## fig3.modelA.full: Accuracy ~ Productivity.tertiary + TaskItem_type + Productivity.tertiary:TaskItem_type + 
## fig3.modelA.full:     IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##                  Df    AIC    BIC  logLik deviance  Chisq Chi Df
## fig3.modelA.main  7 759.09 791.14 -372.55   745.09              
## fig3.modelA.full  8 760.38 797.00 -372.19   744.38 0.7168      1
##                  Pr(>Chisq)
## fig3.modelA.main           
## fig3.modelA.full     0.3972
```

```r
# summary(fig3.modelA.full)
```

Confidence intervals

```r
confint(fig3.modelA.full)
```

```
## Computing profile confidence intervals ...
```

```
##                                            2.5 %     97.5 %
## .sig01                                 0.8988781  1.5949125
## .sig02                                 0.1848748  0.8740616
## (Intercept)                           -1.1307126 -0.1840882
## Productivity.tertiary1                -0.1000250  1.6365765
## TaskItem_type1                         1.2672071  3.2169834
## IHC.c                                  0.4167696  1.2908733
## age.c                                 -0.4060058  0.4383386
## Productivity.tertiary1:TaskItem_type1 -0.6167524  1.4952390
```

Final model

```r
summary(fig3.modelA.main)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c +  
##     (1 | TaskItem_num) + (1 | LadlabID)
##    Data: fig3.modelA.df
## 
##      AIC      BIC   logLik deviance df.resid 
##    759.1    791.1   -372.5    745.1      712 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8365 -0.5224 -0.2430  0.5505  7.2107 
## 
## Random effects:
##  Groups       Name        Variance Std.Dev.
##  LadlabID     (Intercept) 1.4616   1.2089  
##  TaskItem_num (Intercept) 0.1799   0.4241  
## Number of obs: 719, groups:  LadlabID, 90; TaskItem_num, 8
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -0.66566    0.22516  -2.956 0.003112 ** 
## Productivity.tertiary1  0.78611    0.43254   1.817 0.069152 .  
## TaskItem_type1          2.28454    0.44528   5.131 2.89e-07 ***
## IHC.c                   0.84583    0.21879   3.866 0.000111 ***
## age.c                   0.01438    0.21060   0.068 0.945569    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Prdc.1 TskI_1 IHC.c 
## Prdctvty.t1 -0.034                     
## TskItm_typ1 -0.084  0.035              
## IHC.c       -0.052 -0.363  0.078       
## age.c        0.002 -0.361  0.002 -0.333
```

```r
plot_model(fig3.modelA.full, type = "est", transform = NULL, show.intercept = T, 
    show.p = T, show.values = T, title = "What Comes Next Accuracy")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

```r
plot_model(fig3.modelA.full, type = "est", show.intercept = T, show.p = T, show.values = T, 
    title = "What Comes Next Accuracy")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-38-2.png)<!-- -->

Excluding IHC>=99, report averages by each factor

```r
# means by item type
fig3.df %>% filter(Productivity.tertiary != "Productive (IHC ≥ 99)") %>% group_by(LadlabID, 
    TaskItem_type) %>% summarise(score = mean(Accuracy, na.rm = T)) %>% ungroup() %>% 
    group_by(TaskItem_type) %>% summarise(mean = mean(score, na.rm = T), sd = sd(score, 
    na.rm = T), n = n()) %>% kable(digits = 3)
```



TaskItem_type         mean      sd    n
------------------  ------  ------  ---
Decade transition    0.167   0.300   90
Mid-decade           0.488   0.345   90

```r
# means by productivity
fig3.df %>% filter(Productivity.tertiary != "Productive (IHC ≥ 99)") %>% group_by(LadlabID, 
    Productivity.tertiary) %>% summarise(score = mean(Accuracy, na.rm = T)) %>% ungroup() %>% 
    group_by(Productivity.tertiary) %>% summarise(mean = mean(score, na.rm = T), 
    sd = sd(score, na.rm = T), n = n()) %>% kable(digits = 3)
```



Productivity.tertiary     mean      sd    n
----------------------  ------  ------  ---
Nonproductive            0.278   0.261   49
Productive (IHC < 99)    0.562   0.254   41


### Remove nonproductive
Compare both productive groups

Build dataframe and mean-center IHC, FHC and age.

```r
fig3.modelB.df <- fig3.data %>% filter(Productivity.tertiary != "Nonproductive") %>% 
    dplyr::select(c(LadlabID, IHC, Age, Productivity.tertiary, TaskItem_num, TaskItem_type, 
        Accuracy)) %>% mutate(Productivity.tertiary = as.factor(Productivity.tertiary), 
    IHC = as.integer(IHC), LadlabID = as.factor(LadlabID), TaskItem_type = as.factor(TaskItem_type)) %>% 
    mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale = TRUE)), age.c = as.vector(scale(Age, 
        center = TRUE, scale = TRUE)))
# weighted effects coding
wec <- mean(as.numeric(fig3.modelB.df$Productivity.tertiary) - 1)
contrasts(fig3.modelB.df$Productivity.tertiary) <- c(-wec, 1 - wec)
wec <- mean(as.numeric(fig3.modelB.df$TaskItem_type) - 1)
contrasts(fig3.modelB.df$TaskItem_type) <- c(-wec, 1 - wec)
str(fig3.modelB.df)
```

```
## 'data.frame':	584 obs. of  9 variables:
##  $ LadlabID             : Factor w/ 73 levels "010916-D5","012016-AD",..: 1 1 1 1 1 1 1 1 2 2 ...
##  $ IHC                  : int  100 100 100 100 100 100 100 100 29 29 ...
##  $ Age                  : num  4.78 4.78 4.78 4.78 4.78 4.78 4.78 4.78 4.41 4.41 ...
##  $ Productivity.tertiary: Factor w/ 2 levels "Productive (IHC < 99)",..: 2 2 2 2 2 2 2 2 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.438 0.562
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Productive (IHC < 99)" "Productive (IHC ≥ 99)"
##   .. .. ..$ : NULL
##  $ TaskItem_num         : num  23 40 62 70 37 29 86 59 23 40 ...
##  $ TaskItem_type        : Factor w/ 2 levels "Decade transition",..: 2 2 2 2 2 1 2 1 2 2 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.75 0.25
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Decade transition" "Mid-decade"
##   .. .. ..$ : NULL
##  $ Accuracy             : int  1 1 1 1 1 1 1 1 0 0 ...
##  $ IHC.c                : num  1.04 1.04 1.04 1.04 1.04 ...
##  $ age.c                : num  -0.961 -0.961 -0.961 -0.961 -0.961 ...
```

Regression testing for interaction of productivity and decade/non-decade item type on WCN accuracy

```r
## WCN model looking at interaction between productivity and decade/non-decade
## item accuracy
fig3.modelB.base <- glmer(Accuracy ~ age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = fig3.modelB.df)
fig3.modelB.ihc <- glmer(Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = fig3.modelB.df)
fig3.modelB.prod <- glmer(Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | 
    TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelB.df)
fig3.modelB.itemtype <- glmer(Accuracy ~ TaskItem_type + IHC.c + age.c + (1 | TaskItem_num) + 
    (1 | LadlabID), family = "binomial", data = fig3.modelB.df)
fig3.modelB.main <- glmer(Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + 
    age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelB.df)
fig3.modelB.full <- glmer(Accuracy ~ Productivity.tertiary + TaskItem_type + Productivity.tertiary:TaskItem_type + 
    IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelB.df)

# LRT test for interaction effect
anova(fig3.modelB.full, fig3.modelB.main, test = "LRT")
```

```
## Data: fig3.modelB.df
## Models:
## fig3.modelB.main: Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c + 
## fig3.modelB.main:     (1 | TaskItem_num) + (1 | LadlabID)
## fig3.modelB.full: Accuracy ~ Productivity.tertiary + TaskItem_type + Productivity.tertiary:TaskItem_type + 
## fig3.modelB.full:     IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##                  Df    AIC    BIC  logLik deviance  Chisq Chi Df
## fig3.modelB.main  7 529.14 559.72 -257.57   515.14              
## fig3.modelB.full  8 528.58 563.52 -256.29   512.58 2.5631      1
##                  Pr(>Chisq)
## fig3.modelB.main           
## fig3.modelB.full     0.1094
```

Main effect model

```r
# LRT tests for main effects
drop1(fig3.modelB.main, test = "Chisq")
```

```
## Single term deletions
## 
## Model:
## Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c + 
##     (1 | TaskItem_num) + (1 | LadlabID)
##                       Df    AIC     LRT  Pr(Chi)    
## <none>                   529.14                     
## Productivity.tertiary  1 531.63  4.4885 0.034124 *  
## TaskItem_type          1 539.66 12.5183 0.000403 ***
## IHC.c                  1 528.10  0.9553 0.328383    
## age.c                  1 528.42  1.2753 0.258781    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# summary of final model
summary(fig3.modelB.main)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## Accuracy ~ Productivity.tertiary + TaskItem_type + IHC.c + age.c +  
##     (1 | TaskItem_num) + (1 | LadlabID)
##    Data: fig3.modelB.df
## 
##      AIC      BIC   logLik deviance df.resid 
##    529.1    559.7   -257.6    515.1      576 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.0869 -0.3663  0.1842  0.4655  2.0476 
## 
## Random effects:
##  Groups       Name        Variance Std.Dev.
##  LadlabID     (Intercept) 1.4452   1.2022  
##  TaskItem_num (Intercept) 0.1116   0.3341  
## Number of obs: 583, groups:  LadlabID, 73; TaskItem_num, 8
## 
## Fixed effects:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              1.5807     0.2498   6.327 2.49e-10 ***
## Productivity.tertiary1   2.0008     0.9480   2.111   0.0348 *  
## TaskItem_type1           2.1135     0.3982   5.308 1.11e-07 ***
## IHC.c                    0.4474     0.4567   0.980   0.3272    
## age.c                    0.2272     0.2008   1.132   0.2578    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Prdc.1 TskI_1 IHC.c 
## Prdctvty.t1  0.149                     
## TskItm_typ1  0.187  0.079              
## IHC.c        0.013 -0.878  0.029       
## age.c        0.049  0.192  0.033 -0.227
```

### All kids
Analyses including all participants

```r
fig3.modelC.df <- fig3.data %>% dplyr::select(c(LadlabID, IHC, FHC, Age, Productivity, 
    TaskItem_num, TaskItem_type, Accuracy)) %>% mutate(Productivity = as.factor(Productivity), 
    IHC = as.integer(IHC), LadlabID = as.factor(LadlabID), TaskItem_type = as.factor(TaskItem_type)) %>% 
    mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale = TRUE)), age.c = as.vector(scale(Age, 
        center = TRUE, scale = TRUE)))
# weighted effects coding
wec <- mean(as.numeric(fig3.modelC.df$Productivity) - 1)
contrasts(fig3.modelC.df$Productivity) <- c(-wec, 1 - wec)
wec <- mean(as.numeric(fig3.modelC.df$TaskItem_type) - 1)
contrasts(fig3.modelC.df$TaskItem_type) <- c(-wec, 1 - wec)

## regressions
fig3.modelC.noint <- glmer(Accuracy ~ Productivity + TaskItem_type + IHC.c + age.c + 
    (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelC.df)
# LRT tests
drop1(fig3.modelC.noint, test = "Chisq")
```

```
## Single term deletions
## 
## Model:
## Accuracy ~ Productivity + TaskItem_type + IHC.c + age.c + (1 | 
##     TaskItem_num) + (1 | LadlabID)
##               Df    AIC    LRT   Pr(Chi)    
## <none>           899.88                     
## Productivity   1 900.63  2.747 0.0974175 .  
## TaskItem_type  1 909.31 11.434 0.0007213 ***
## IHC.c          1 953.06 55.179 1.101e-13 ***
## age.c          1 897.90  0.022 0.8833797    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Test for interaction
fig3.modelC.full <- glmer(Accuracy ~ Productivity + TaskItem_type + Productivity:TaskItem_type + 
    IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = fig3.modelC.df)
anova(fig3.modelC.full, fig3.modelC.noint, test = "LRT")
```

```
## Data: fig3.modelC.df
## Models:
## fig3.modelC.noint: Accuracy ~ Productivity + TaskItem_type + IHC.c + age.c + (1 | 
## fig3.modelC.noint:     TaskItem_num) + (1 | LadlabID)
## fig3.modelC.full: Accuracy ~ Productivity + TaskItem_type + Productivity:TaskItem_type + 
## fig3.modelC.full:     IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##                   Df    AIC    BIC  logLik deviance  Chisq Chi Df
## fig3.modelC.noint  7 899.88 934.06 -442.94   885.88              
## fig3.modelC.full   8 901.82 940.87 -442.91   885.82 0.0647      1
##                   Pr(>Chisq)
## fig3.modelC.noint           
## fig3.modelC.full      0.7992
```



## Within / Outside IHC
Add whether the Task Item was within or outside of the kid's initial highest count.

```r
# first, get initial highest count for each kiddo Make a lookup table with SID
# and initial highest count
lookup <- full.data %>% distinct(LadlabID, IHC)

wcn.data %<>% dplyr::mutate(TaskItem = as.numeric(as.character(TaskItem)))

# This is a function that, for each trial, checks the number queried. If number
# queried is above the child's initial highest count, marks that trial as beyond
# count range.
determine_count_range <- function(df) {
    tmp <- df
    for (row in 1:nrow(tmp)) {
        sub = as.character(tmp[row, "LadlabID"])
        count_range = as.numeric(as.character(subset(lookup, LadlabID == sub)$IHC))
        tmp[row, "IHC"] = as.numeric(as.character(count_range))
        if (tmp[row, "TaskItem"] > count_range) {
            tmp[row, "WithinOutsideIHC"] = "outside"
        } else {
            tmp[row, "WithinOutsideIHC"] = "within"
        }
    }
    return(tmp)
}

# Run for wcn
wcn.data <- determine_count_range(wcn.data)
```


WCN accuracy, within and outside of IHC, by various contrasts

```r
wcn.data %>% dplyr::filter(TaskType == "immediate") %>% dplyr::group_by(LadlabID, 
    WithinOutsideIHC) %>% dplyr::summarize(score = mean(Accuracy, na.rm = T)) %>% 
    ungroup() %>% group_by(WithinOutsideIHC) %>% dplyr::summarize(mean = mean(score, 
    na.rm = TRUE), sd = sd(score, na.rm = TRUE), n = n()) %>% kable(digits = 2)
```



WithinOutsideIHC    mean     sd    n
-----------------  -----  -----  ---
outside             0.41   0.32   90
within              0.68   0.32   88

```r
# 2-way productivity, all kids
wcn.data %>% dplyr::filter(TaskType == "immediate") %>% dplyr::group_by(LadlabID, 
    Productivity, WithinOutsideIHC) %>% dplyr::summarize(score = mean(Accuracy, na.rm = T)) %>% 
    ungroup() %>% group_by(Productivity, WithinOutsideIHC) %>% dplyr::summarize(mean = mean(score, 
    na.rm = TRUE), sd = sd(score, na.rm = TRUE), n = n()) %>% kable(digits = 2)
```



Productivity    WithinOutsideIHC    mean     sd    n
--------------  -----------------  -----  -----  ---
Nonproductive   outside             0.27   0.26   49
Nonproductive   within              0.57   0.34   17
Productive      outside             0.58   0.31   41
Productive      within              0.70   0.31   71

```r
# three-way
wcn.data %>% dplyr::filter(TaskType == "immediate") %>% dplyr::group_by(LadlabID, 
    Productivity.tertiary, WithinOutsideIHC) %>% dplyr::summarize(score = mean(Accuracy, 
    na.rm = T)) %>% ungroup() %>% group_by(Productivity.tertiary, WithinOutsideIHC) %>% 
    dplyr::summarize(mean = mean(score, na.rm = TRUE), sd = sd(score, na.rm = TRUE), 
        n = n()) %>% kable(digits = 2)
```



Productivity.tertiary   WithinOutsideIHC    mean     sd    n
----------------------  -----------------  -----  -----  ---
Nonproductive           outside             0.27   0.26   49
Nonproductive           within              0.57   0.34   17
Productive (IHC < 99)   outside             0.58   0.31   41
Productive (IHC < 99)   within              0.54   0.32   39
Productive (IHC ≥ 99)   within              0.91   0.15   32


Plotting WCN as within vs. beyond by productivity 

![](recursionAnalysis_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

Same graph but three-way productvity grouping
![](recursionAnalysis_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

### Fig 4
Try without violins

```r
## HM? 
wcn.data %>%
  mutate(WithinOutsideIHC = factor(WithinOutsideIHC, levels = c("within", "outside"), 
                                   labels = c("Within IHC", "Beyond IHC")))%>%
  dplyr::filter(TaskType == "immediate") %>%
  dplyr::group_by(Productivity.tertiary, WithinOutsideIHC, LadlabID, prod.gradient) %>%
  dplyr::summarize(meansubj = mean(Accuracy, na.rm = TRUE)) %>%
  ggplot(aes(x=WithinOutsideIHC, y=meansubj)) +
  stat_summary(aes(color=Productivity.tertiary),
               fun.data = mean_cl_boot, geom="errorbar",
               width = 0.3)+
  stat_summary(aes(color=Productivity.tertiary), fill="white",
               fun.y = mean,
#               position = position_dodge(width=0.8), 
               geom="point", shape=23, size=3) +
  stat_summary(fun.y="mean", geom="line", aes(color=Productivity.tertiary,
                                              group=factor(Productivity.tertiary))) +
      scale_colour_manual(values=prod.pal, name="Productivity") + 
  scale_fill_brewer(guide=FALSE) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y="Average Proportion Correct", x="Trial Type") +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

```r
ggsave('graphs/wcn-within-beyond-final.png', width=6, height=4)
```

### Remove IHC=99


```
## 'data.frame':	720 obs. of  7 variables:
##  $ LadlabID        : Factor w/ 90 levels "010516-K4","011216-KD1",..: 1 1 1 1 1 1 1 1 2 2 ...
##  $ TaskItem_num    : num  23 40 62 70 37 29 86 59 23 40 ...
##  $ age.c           : num  -1.32 -1.32 -1.32 -1.32 -1.32 ...
##  $ IHC.c           : num  -1.05 -1.05 -1.05 -1.05 -1.05 ...
##  $ Productivity    : Factor w/ 2 levels "Nonproductive",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.456 0.544
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Nonproductive" "Productive"
##   .. .. ..$ : NULL
##  $ WithinOutsideIHC: Factor w/ 2 levels "outside","within": 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.275 0.725
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "outside" "within"
##   .. .. ..$ : NULL
##  $ Accuracy        : int  0 0 0 0 0 0 0 0 0 0 ...
```

Construct models and compare

```r
## WCN model looking at interaction between productivity and trial type in WCN
## task
wcn.model2.base <- glmer(Accuracy ~ age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = wcn_model.df2)
wcn.model2.ihc <- glmer(Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = wcn_model.df2)
wcn.model2.noint <- glmer(Accuracy ~ Productivity + WithinOutsideIHC + IHC.c + age.c + 
    (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = wcn_model.df2)
wcn.model2.int <- glmer(Accuracy ~ Productivity + WithinOutsideIHC + Productivity:WithinOutsideIHC + 
    IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), family = "binomial", data = wcn_model.df2)
# comparison against base models
anova(wcn.model2.int, wcn.model2.noint, wcn.model2.ihc, wcn.model2.base, test = "LRT")
```

```
## Data: wcn_model.df2
## Models:
## wcn.model2.base: Accuracy ~ age.c + (1 | TaskItem_num) + (1 | LadlabID)
## wcn.model2.ihc: Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
## wcn.model2.noint: Accuracy ~ Productivity + WithinOutsideIHC + IHC.c + age.c + 
## wcn.model2.noint:     (1 | TaskItem_num) + (1 | LadlabID)
## wcn.model2.int: Accuracy ~ Productivity + WithinOutsideIHC + Productivity:WithinOutsideIHC + 
## wcn.model2.int:     IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##                  Df    AIC    BIC  logLik deviance   Chisq Chi Df
## wcn.model2.base   4 792.03 810.34 -392.01   784.03               
## wcn.model2.ihc    5 770.98 793.87 -380.49   760.98 23.0497      1
## wcn.model2.noint  7 771.48 803.53 -378.74   757.48  3.4979      2
## wcn.model2.int    8 768.99 805.61 -376.49   752.99  4.4942      1
##                  Pr(>Chisq)    
## wcn.model2.base                
## wcn.model2.ihc    1.579e-06 ***
## wcn.model2.noint    0.17396    
## wcn.model2.int      0.03401 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Interaction was significant, let's view the full model

```r
# summary of final model
summary(wcn.model2.int)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## Accuracy ~ Productivity + WithinOutsideIHC + Productivity:WithinOutsideIHC +  
##     IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##    Data: wcn_model.df2
## 
##      AIC      BIC   logLik deviance df.resid 
##    769.0    805.6   -376.5    753.0      711 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2798 -0.5089 -0.2469  0.5401  7.1062 
## 
## Random effects:
##  Groups       Name        Variance Std.Dev.
##  LadlabID     (Intercept) 1.386    1.177   
##  TaskItem_num (Intercept) 1.130    1.063   
## Number of obs: 719, groups:  LadlabID, 90; TaskItem_num, 8
## 
## Fixed effects:
##                                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     -0.56781    0.41278  -1.376 0.168946    
## Productivity1                    0.79707    0.42572   1.872 0.061170 .  
## WithinOutsideIHC1                0.13181    0.36667   0.359 0.719231    
## IHC.c                            0.82022    0.23593   3.477 0.000508 ***
## age.c                            0.02075    0.20704   0.100 0.920180    
## Productivity1:WithinOutsideIHC1 -1.12524    0.53148  -2.117 0.034242 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) Prdct1 WOIHC1 IHC.c  age.c 
## Productvty1 -0.020                            
## WthnOtsIHC1  0.043 -0.020                     
## IHC.c       -0.041 -0.323 -0.406              
## age.c        0.003 -0.359  0.011 -0.308       
## Prd1:WOIHC1 -0.102 -0.009 -0.363  0.119 -0.016
```

Confidence intervals

```r
tidy(wcn.model2.int, conf.int = TRUE, exponentiate = F, effects = "fixed")
```

```
## # A tibble: 6 x 8
##   effect term       estimate std.error statistic p.value conf.low conf.high
##   <chr>  <chr>         <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
## 1 fixed  (Intercep…  -0.568      0.413    -1.38  1.69e-1  -1.38      0.241 
## 2 fixed  Productiv…   0.797      0.426     1.87  6.12e-2  -0.0373    1.63  
## 3 fixed  WithinOut…   0.132      0.367     0.359 7.19e-1  -0.587     0.850 
## 4 fixed  IHC.c        0.820      0.236     3.48  5.08e-4   0.358     1.28  
## 5 fixed  age.c        0.0207     0.207     0.100 9.20e-1  -0.385     0.427 
## 6 fixed  Productiv…  -1.13       0.531    -2.12  3.42e-2  -2.17     -0.0836
```


Planned contrasts, t-test

```r
# within
wcn_model.df2 %>% group_by(LadlabID, Productivity, WithinOutsideIHC) %>% summarise(score = mean(Accuracy, 
    na.rm = T)) %>% filter(WithinOutsideIHC == "within") %>% t.test(score ~ Productivity, 
    data = .)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  score by Productivity
## t = 0.28461, df = 29.224, p-value = 0.778
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1690409  0.2237130
## sample estimates:
## mean in group Nonproductive    mean in group Productive 
##                   0.5651261                   0.5377900
```

```r
# outside
wcn_model.df2 %>% group_by(LadlabID, Productivity, WithinOutsideIHC) %>% summarise(score = mean(Accuracy, 
    na.rm = T)) %>% filter(WithinOutsideIHC == "outside") %>% t.test(score ~ Productivity, 
    data = .)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  score by Productivity
## t = -5.0915, df = 77.994, p-value = 2.404e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.4329022 -0.1895243
## sample estimates:
## mean in group Nonproductive    mean in group Productive 
##                   0.2677357                   0.5789489
```



### Remove non-productive

We can only compare accuracy for within-IHC trials:

```
## 'data.frame':	405 obs. of  6 variables:
##  $ LadlabID             : Factor w/ 71 levels "010916-D5","012016-AD",..: 1 1 1 1 1 1 1 1 2 2 ...
##  $ TaskItem_num         : num  23 40 62 70 37 29 86 59 23 29 ...
##  $ age.c                : num  -1.04 -1.04 -1.04 -1.04 -1.04 ...
##  $ IHC.c                : num  0.71 0.71 0.71 0.71 0.71 ...
##  $ Productivity.tertiary: Factor w/ 2 levels "Productive (IHC < 99)",..: 2 2 2 2 2 2 2 2 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.632 0.368
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Productive (IHC < 99)" "Productive (IHC ≥ 99)"
##   .. .. ..$ : NULL
##  $ Accuracy             : int  1 1 1 1 1 1 1 1 0 0 ...
```

Construct models and compare

```r
## WCN model looking at interaction between productivity and trial type in WCN
## task
wcn.model3.base <- glmer(Accuracy ~ age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = wcn_model.df3)
wcn.model3.ihc <- glmer(Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID), 
    family = "binomial", data = wcn_model.df3)
wcn.model3.prod <- glmer(Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | 
    TaskItem_num) + (1 | LadlabID), family = "binomial", data = wcn_model.df3)
# compare
anova(wcn.model3.prod, wcn.model3.ihc, wcn.model3.base, test = "LRT")
```

```
## Data: wcn_model.df3
## Models:
## wcn.model3.base: Accuracy ~ age.c + (1 | TaskItem_num) + (1 | LadlabID)
## wcn.model3.ihc: Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
## wcn.model3.prod: Accuracy ~ Productivity.tertiary + IHC.c + age.c + (1 | TaskItem_num) + 
## wcn.model3.prod:     (1 | LadlabID)
##                 Df    AIC    BIC  logLik deviance   Chisq Chi Df
## wcn.model3.base  4 374.06 390.07 -183.03   366.06               
## wcn.model3.ihc   5 336.53 356.55 -163.27   326.53 39.5224      1
## wcn.model3.prod  6 337.26 361.29 -162.63   325.26  1.2687      1
##                 Pr(>Chisq)    
## wcn.model3.base               
## wcn.model3.ihc   3.243e-10 ***
## wcn.model3.prod       0.26    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
Final model: IHC only

```r
summary(wcn.model3.ihc)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Accuracy ~ IHC.c + age.c + (1 | TaskItem_num) + (1 | LadlabID)
##    Data: wcn_model.df3
## 
##      AIC      BIC   logLik deviance df.resid 
##    336.5    356.6   -163.3    326.5      400 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8736  0.0839  0.2183  0.3824  1.7693 
## 
## Random effects:
##  Groups       Name        Variance Std.Dev.
##  LadlabID     (Intercept) 1.546    1.244   
##  TaskItem_num (Intercept) 1.011    1.006   
## Number of obs: 405, groups:  LadlabID, 71; TaskItem_num, 8
## 
## Fixed effects:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  2.06115    0.46501   4.433 9.31e-06 ***
## IHC.c        1.43877    0.25823   5.572 2.52e-08 ***
## age.c        0.01892    0.22801   0.083    0.934    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##       (Intr) IHC.c 
## IHC.c  0.318       
## age.c  0.022 -0.044
```


# Infinity Descriptives

## Counts
Number of kids in each infinity category

```
## # A tibble: 8 x 3
## # Groups:   Productivity [2]
##   Productivity  Category             n
##   <fct>         <chr>            <int>
## 1 Nonproductive A Non-knower        33
## 2 Nonproductive B Endless-only       1
## 3 Nonproductive C Successor-only    12
## 4 Nonproductive D Full-knower        3
## 5 Productive    A Non-knower        26
## 6 Productive    B Endless-only      10
## 7 Productive    C Successor-only    17
## 8 Productive    D Full-knower       20
```

```
##                EndlessKnower
## SuccessorKnower  0  1
##               0 59 11
##               1 29 23
```

Successor knowledge by productivity

```
##    
##     Nonproductive Productive
##   0            34         36
##   1            15         37
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(classification.data$SuccessorKnower, classification.data$Productivity)
## X-squared = 4.0446, df = 1, p-value = 0.04431
```

Endless knowledge by productivity

```
##    
##     Nonproductive Productive
##   0            45         43
##   1             4         30
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(classification.data$EndlessKnower, classification.data$Productivity)
## X-squared = 14.223, df = 1, p-value = 0.0001624
```

Infinity knowledge by productivity

```
##    
##     Nonproductive Productive
##   0            46         53
##   1             3         20
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(classification.data$InfinityKnower, classification.data$Productivity)
## X-squared = 7.3396, df = 1, p-value = 0.006745
```

## Age
Average age of kids for Endless and Successor Knowers

 SuccessorKnower   meanAge   sdAge   meanAgeMonths   sdAgeMonths
----------------  --------  ------  --------------  ------------
               0     4.917   0.577          63.917         7.505
               1     5.108   0.550          66.400         7.147



 EndlessKnower   meanAge   sdAge   meanAgeMonths   sdAgeMonths
--------------  --------  ------  --------------  ------------
             0     4.893   0.560           63.61         7.274
             1     5.270   0.516           68.51         6.704



InfinityKnower    meanAge   sdAge   meanAgeMonths   sdAgeMonths
---------------  --------  ------  --------------  ------------
0                   4.923   0.567          64.001         7.377
1                   5.321   0.476          69.171         6.192

## HC
Infinity in relation to highest count

```
## # A tibble: 2 x 3
##   EndlessKnower mean_IHC mean_FHC
##           <int>    <dbl>    <dbl>
## 1             0     42.8     63.2
## 2             1     70.1     91.8
```

```
## # A tibble: 2 x 3
##   SuccessorKnower mean_IHC mean_FHC
##             <int>    <dbl>    <dbl>
## 1               0     47.4     66.3
## 2               1     54.5     77.7
```

```
## # A tibble: 2 x 3
##   InfinityKnower mean_IHC mean_FHC
##   <fct>             <dbl>    <dbl>
## 1 0                  46.4     66.6
## 2 1                  67.7     90.9
```

## WCN 
Infinity in relation to WCN

```
## Joining, by = "LadlabID"
```

```
## # A tibble: 2 x 3
##   EndlessKnower mean_contig_nn median_contig_nn
##           <int>          <dbl>            <dbl>
## 1             0           3.73              3.5
## 2             1           5.79              6
```

```
## Joining, by = "LadlabID"
```

```
## # A tibble: 2 x 3
##   SuccessorKnower mean_contig_nn median_contig_nn
##             <int>          <dbl>            <dbl>
## 1               0           4.01                5
## 2               1           4.69                5
```

```
## Joining, by = "LadlabID"
```

```
## # A tibble: 2 x 3
##   InfinityKnower mean_contig_nn median_contig_nn
##   <fct>                   <dbl>            <int>
## 1 0                        3.95                4
## 2 1                        5.83                6
```

# [remove] Productivity gradient



## Correlation between productivity gradient and IHC/FHC


## Correlation between Productivity classification and Prod.gradient



***

# Infinity Regression Analyses

## Setup
Counting, Productivity, and Infinity Battery
To identify whether there is connection between counting experience and Infinity Task performance, we will conduct three initial analyses, predicting Infinity Task performance from either (1) Initial Highest Count, (3) Productivity for Decade Rule (defined above), or (3) performance on the Next Number task. 

glm(inf.0/1 ~ (predictor) + age, family = binomial).

---
First, we need to make a model data frame that readily has all of this information


```r
# model base each participant only needs one row here, because we only need to
# know whether they are a Successor Knower or Endless Knower
model.df <- full.data %>% dplyr::distinct(LadlabID, Age, AgeGroup, Gender, SuccessorKnower, 
    EndlessKnower, InfinityKnower, IHC, Productivity, Productivity.tertiary, prod.gradient)
model.df <- right_join(model.df, wcn.accuracy, by = "LadlabID") %>% mutate(SuccessorKnower = factor(SuccessorKnower, 
    levels = c(0, 1)), EndlessKnower = factor(EndlessKnower, levels = c(0, 1)), IHC = as.integer(IHC), 
    LadlabID = factor(LadlabID))
```

## Regression with All subjects
### prep

```r
# scale and center for model fit
distinct_model.df <- model.df %>% mutate(IHC.c = as.vector(scale(IHC, center = TRUE, 
    scale = TRUE)), Age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)), prod.gradient.c = as.vector(scale(prod.gradient, 
    center = TRUE, scale = TRUE)), wcnscore.c = as.vector(scale(wcnscore, center = TRUE, 
    scale = TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(distinct_model.df$Productivity) - 1)
contrasts(distinct_model.df$Productivity) <- c(-wec, 1 - wec)
# structure
str(distinct_model.df)
```

```
## 'data.frame':	122 obs. of  16 variables:
##  $ LadlabID             : Factor w/ 122 levels "010516-K4","010916-D5",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Age                  : num  4.17 4.78 4 4.44 4.12 4.41 4.75 5.42 4.25 4.6 ...
##  $ AgeGroup             : Factor w/ 6 levels "4-4.5y","4.5-5y",..: 1 2 1 1 1 1 2 3 1 2 ...
##  $ Gender               : Factor w/ 5 levels "f","F","m","M",..: 1 3 1 3 1 1 1 2 1 3 ...
##  $ SuccessorKnower      : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 2 2 ...
##  $ EndlessKnower        : Factor w/ 2 levels "0","1": 1 1 1 2 1 1 1 1 1 1 ...
##  $ InfinityKnower       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ IHC                  : int  13 100 5 15 39 29 26 39 29 13 ...
##  $ Productivity         : Factor w/ 2 levels "Nonproductive",..: 1 2 1 1 1 2 1 2 2 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.598 0.402
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Nonproductive" "Productive"
##   .. .. ..$ : NULL
##  $ Productivity.tertiary: chr  "Nonproductive" "Productive (IHC ≥ 99)" "Nonproductive" "Nonproductive" ...
##  $ prod.gradient        : num  0.186 1 0 0.179 0 ...
##  $ wcnscore             : int  0 8 0 1 2 0 6 8 3 0 ...
##  $ IHC.c                : num  -1.107 1.467 -1.344 -1.048 -0.338 ...
##  $ Age.c                : num  -1.449 -0.382 -1.747 -0.977 -1.537 ...
##  $ prod.gradient.c      : num  -0.989 0.848 -1.408 -1.005 -1.408 ...
##  $ wcnscore.c           : num  -1.571 1.35 -1.571 -1.206 -0.841 ...
```
### Successor models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.successor <- glm(SuccessorKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df)

##IHC model##
model.ihc.successor <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df)
##WCN Model##
model.nn.successor <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df)
##Productivity model##
model.prod.successor <- glm(SuccessorKnower ~ Productivity + Age.c, family = "binomial",
                              data = distinct_model.df)

##EXPLORATORY## - GAIN SCORE
model.gain.successor <- glm(SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df)

##Regression table for Successor Knower Models (Table )
mtable.sf.knowers <- mtable('Base' = base.successor,
            'IHC' = model.ihc.successor,
            'NN' = model.nn.successor,
            'Productivity' = model.prod.successor,
            'Prod. gain' = model.gain.successor,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.sf.knowers
```

```
## 
## Calls:
## Base: glm(formula = SuccessorKnower ~ Age.c, family = "binomial", data = distinct_model.df)
## IHC: glm(formula = SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## NN: glm(formula = SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Productivity: glm(formula = SuccessorKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Prod. gain: glm(formula = SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## 
## ============================================================================
##                       Base      IHC        NN     Productivity  Prod. gain  
## ----------------------------------------------------------------------------
##   (Intercept)        -0.306    -0.306    -0.307      -0.313       -0.310    
##                      (0.186)   (0.186)   (0.186)     (0.188)      (0.187)   
##   Age.c               0.342     0.313     0.286       0.163        0.192    
##                      (0.188)   (0.218)   (0.210)     (0.226)      (0.230)   
##   IHC.c                         0.058                                       
##                                (0.216)                                      
##   wcnscore.c                              0.126                             
##                                          (0.209)                            
##   Productivity: 1                                     0.661                 
##                                                      (0.464)                
##   prod.gradient.c                                                  0.262    
##                                                                   (0.232)   
## ----------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.037     0.038     0.041       0.059        0.050    
##   Log-likelihood    -81.539   -81.503   -81.358     -80.515      -80.896    
##   AIC               167.077   169.006   168.716     167.030      167.792    
##   N                 122       122       122         122          122        
## ============================================================================
```

```r
write.mtable(mtable.sf.knowers, file="graphs/table3-all.txt")
```

#### Model comparisons

Comparing models with a single predictor against base model, we find that none of the predictors are significant in accounting for successor knowledge after controlling for age and IHC.

```r
# base v. IHC
anova(base.successor, model.ihc.successor, test = "LRT")  #IHC n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       120     163.08                     
## 2       119     163.01  1 0.071147   0.7897
```

```r
# wcn v. base
anova(base.successor, model.nn.successor, test = "LRT")  #NN n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       120     163.08                     
## 2       119     162.72  1  0.36154   0.5477
```

```r
# Productivity v. base
anova(base.successor, model.prod.successor, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ Productivity + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       120     163.08                     
## 2       119     161.03  1   2.0476   0.1524
```

```r
## Exploratory vs. base
anova(base.successor, model.gain.successor, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       120     163.08                     
## 2       119     161.79  1   1.2849    0.257
```

#### Visualize


```r
plot_models(model.nn.successor, model.prod.successor, model.ihc.successor, model.gain.successor, 
    transform = "plogis", show.values = T, show.p = T, colors = "Dark2", show.intercept = T, 
    spacing = 0.7, m.labels = c("Model 1: NN", "Model 2: Productivity", "Model 3: IHC", 
        "Model 4: Prod. Grad"), show.legend = T, title = "Regression analysis, Successor Knowledge of infinity, all participants", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status", prod.gradient.c = "Productivity Gradient"), 
    axis.title = "Endorsement Probability", axis.lim = c(0, 1)) + theme_bw() + ggplot2::geom_hline(yintercept = 0.5, 
    linetype = "dashed") + ggsave("graphs/reg-succ.png", height = 7, width = 7)
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-71-1.png)<!-- -->

Visualize relationship between IHC and SuccessorKnowledge, by productivity

```r
distinct_model.df %>% ggplot(data = ., mapping = aes(x = IHC, y = SuccessorKnower, 
    color = Productivity, group = Productivity)) + geom_point(alpha = 0.2, size = 2) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"), alpha = 0.2, 
        aes(fill = Productivity)) + scale_color_manual(values = mypalette, name = "Productivity") + 
    scale_fill_manual(values = mypalette, name = "Productivity")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-72-1.png)<!-- -->

### Endless models

```r
# Base model
base.endless <- glm(EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df)

### IHC MODEL###
model.ihc.endless <- glm(EndlessKnower ~ IHC.c + Age.c, family = "binomial", data = distinct_model.df)

### NN MODEL###
model.nn.endless <- glm(EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
    data = distinct_model.df)

### PRODUCTIVITY MODEL###
model.prod.endless <- glm(EndlessKnower ~ Productivity + Age.c, family = "binomial", 
    data = distinct_model.df)

## EXPLORATORY## - GAIN SCORE
model.gain.endless <- glm(EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
    data = distinct_model.df)

## Regression table for Endless Models
mtable.endless.knowers <- mtable(Base = base.endless, IHC = model.ihc.endless, NN = model.nn.endless, 
    Productivity = model.prod.endless, `Prod. gradient` = model.gain.endless, summary.stats = c("Nagelkerke R-sq.", 
        "Log-likelihood", "AIC", "F", "p", "N"))

mtable.endless.knowers
```

```
## 
## Calls:
## Base: glm(formula = EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df)
## IHC: glm(formula = EndlessKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## NN: glm(formula = EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Productivity: glm(formula = EndlessKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Prod. gradient: glm(formula = EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## 
## ======================================================================================
##                        Base        IHC          NN      Productivity  Prod. gradient  
## --------------------------------------------------------------------------------------
##   (Intercept)        -1.057***   -1.131***   -1.152***    -1.202***      -1.218***    
##                      (0.221)     (0.235)     (0.240)      (0.256)        (0.260)      
##   Age.c               0.707**     0.407       0.448        0.329          0.280       
##                      (0.224)     (0.250)     (0.244)      (0.258)        (0.267)      
##   IHC.c                           0.666**                                             
##                                  (0.242)                                              
##   wcnscore.c                                  0.693**                                 
##                                              (0.265)                                  
##   Productivity: 1                                          1.698**                    
##                                                           (0.636)                     
##   prod.gradient.c                                                         0.903**     
##                                                                          (0.329)      
## --------------------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.125       0.205       0.204        0.212          0.220       
##   Log-likelihood    -66.655     -62.810     -62.893      -62.463        -62.063       
##   AIC               137.311     131.619     131.786      130.927        130.127       
##   p                   0.001       0.000       0.000        0.000          0.000       
##   N                 122         122         122          122            122           
## ======================================================================================
```

```r
write.mtable(mtable.endless.knowers, file = "graphs/table3-all.txt")
```

#### Model comparisons
Unlike for successor knowledge, here we find that every predictor is significant addition to the base model.

```r
# base v. IHC
anova(base.endless, model.ihc.endless, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1       120     133.31                        
## 2       119     125.62  1   7.6914 0.005549 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# base v. wcn accuracy
anova(model.nn.endless, base.endless, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ wcnscore.c + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1       119     125.79                        
## 2       120     133.31 -1  -7.5249 0.006085 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# base v. productivity
anova(model.prod.endless, base.endless, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Productivity + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1       119     124.93                        
## 2       120     133.31 -1  -8.3837 0.003786 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
## Exploratory base v. productivity gradient
anova(base.endless, model.gain.endless, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1       120     133.31                        
## 2       119     124.13  1   9.1841 0.002441 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

#### 2-factors
Combining these factors don't explain additional variance over single-factor models.

```r
model.prod.nn.endless <- glm(EndlessKnower ~ Productivity + wcnscore.c + Age.c, family = "binomial", 
    data = distinct_model.df)
drop1(model.prod.nn.endless, test = "Chisq")
```

```
## Single term deletions
## 
## Model:
## EndlessKnower ~ Productivity + wcnscore.c + Age.c
##              Df Deviance    AIC    LRT Pr(>Chi)  
## <none>            122.52 130.52                  
## Productivity  1   125.79 131.79 3.2692  0.07059 .
## wcnscore.c    1   124.93 130.93 2.4104  0.12053  
## Age.c         1   123.56 129.56 1.0449  0.30668  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# prod + ihc
model.prod.ihc.endless <- glm(EndlessKnower ~ Productivity + IHC.c + Age.c, family = "binomial", 
    data = distinct_model.df)
drop1(model.prod.ihc.endless, test = "Chisq")
```

```
## Single term deletions
## 
## Model:
## EndlessKnower ~ Productivity + IHC.c + Age.c
##              Df Deviance    AIC    LRT Pr(>Chi)  
## <none>            122.65 130.65                  
## Productivity  1   125.62 131.62 2.9688  0.08488 .
## IHC.c         1   124.93 130.93 2.2764  0.13135  
## Age.c         1   123.66 129.66 1.0083  0.31530  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# nn + ihc
model.nn.ihc.endless <- glm(EndlessKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", 
    data = distinct_model.df)
drop1(model.nn.ihc.endless, test = "Chisq")
```

```
## Single term deletions
## 
## Model:
## EndlessKnower ~ wcnscore.c + IHC.c + Age.c
##            Df Deviance    AIC    LRT Pr(>Chi)
## <none>          124.41 132.41                
## wcnscore.c  1   125.62 131.62 1.2122   0.2709
## IHC.c       1   125.79 131.79 1.3787   0.2403
## Age.c       1   126.76 132.76 2.3569   0.1247
```

```r
# all 3
large.endless.full <- glm(EndlessKnower ~ Productivity + wcnscore.c + IHC.c + Age.c, 
    family = "binomial", data = distinct_model.df)
summary(large.endless.full)
```

```
## 
## Call:
## glm(formula = EndlessKnower ~ Productivity + wcnscore.c + IHC.c + 
##     Age.c, family = "binomial", data = distinct_model.df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3523  -0.8720  -0.4205   1.0436   2.4066  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -1.2273     0.2600  -4.720 2.36e-06 ***
## Productivity1   1.1013     0.7315   1.505    0.132    
## wcnscore.c      0.2912     0.3687   0.790    0.430    
## IHC.c           0.2444     0.3486   0.701    0.483    
## Age.c           0.2510     0.2646   0.949    0.343    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 144.38  on 121  degrees of freedom
## Residual deviance: 122.02  on 117  degrees of freedom
## AIC: 132.02
## 
## Number of Fisher Scoring iterations: 5
```



#### Visualize

Regressions, simple models

```r
plot_models(model.nn.endless, model.prod.endless, model.ihc.endless, model.gain.endless, 
    transform = "plogis", show.values = T, show.p = T, colors = "Dark2", show.intercept = T, 
    spacing = 0.7, m.labels = c("Model 1: NN", "Model 2: Productivity", "Model 3: IHC", 
        "Model 4: Prod. Grad"), show.legend = T, title = "Regression analysis, Endless Knowledge, all participants", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status", prod.gradient.c = "Productivity Gradient"), 
    axis.title = "Endorsement Probability", axis.lim = c(0, 1)) + theme_bw() + ggplot2::geom_hline(yintercept = 0.5, 
    linetype = "dashed")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

```r
ggsave("graphs/reg-endless.png", height = 7, width = 7)

# twofactors
plot_models(model.prod.nn.endless, model.prod.ihc.endless, model.nn.ihc.endless, 
    transform = "plogis", show.values = T, show.p = T, grid = T, colors = "Dark2", 
    show.intercept = T, spacing = 0.7, m.labels = c("Model 1: Prod+NN", "Model 2: Prod+IHC", 
        "Model 3: NN+IHC"), show.legend = F, title = "Regression analysis, Endless Knowledge, all participants", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status", prod.gradient.c = "Productivity Gradient"), 
    axis.title = "Endorsement Probability", axis.lim = c(0, 1)) + theme_bw() + ggplot2::geom_hline(yintercept = 0.5, 
    linetype = "dashed")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-76-2.png)<!-- -->

```r
# graph
plot_model(large.endless.full, transform = "plogis", show.values = T, show.p = T, 
    colors = "Dark2", show.intercept = T, spacing = 0.7, title = "Regression analysis, Endless Knowledge, all participants", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status", prod.gradient.c = "Productivity Gradient"), 
    axis.title = "Endorsement Probability", axis.lim = c(0, 1)) + theme_bw() + ggplot2::geom_hline(yintercept = 0.5, 
    linetype = "dashed")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-76-3.png)<!-- -->

```r
ggsave("graphs/reg-large.png", height = 7, width = 7)
```

Visualize relationship between IHC and SuccessorKnowledge, by productivity

```r
distinct_model.df %>% ggplot(data = ., mapping = aes(x = IHC, y = EndlessKnower, 
    color = Productivity, group = Productivity)) + geom_point(alpha = 0.2, size = 2) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"), alpha = 0.2, 
        aes(fill = Productivity)) + scale_color_manual(values = mypalette, name = "Productivity") + 
    scale_fill_manual(values = mypalette, name = "Productivity")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-77-1.png)<!-- -->


### Full Infinity Knowledge models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.infinity <- glm(InfinityKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df)

##IHC model
model.ihc.infinity <- glm(InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df)
##Highest NN Model
model.nn.infinity <- glm(InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df)
##Productivity model
model.prod.infinity <- glm(InfinityKnower ~ Productivity + Age.c, family = "binomial",
                              data = distinct_model.df)

##Gain Score model
model.gain.infinity <- glm(InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df)

##Regression table for Infinity Knower Models (Table 4)
mtable.inf.knowers <- mtable('Base' = base.infinity,
            'IHC' = model.ihc.infinity,
            'NN' = model.nn.infinity,
            'Prod. Group' = model.prod.infinity,
            'Prod. Gain' = model.gain.infinity,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.inf.knowers
```

```
## 
## Calls:
## Base: glm(formula = InfinityKnower ~ Age.c, family = "binomial", data = distinct_model.df)
## IHC: glm(formula = InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## NN: glm(formula = InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Prod. Group: glm(formula = InfinityKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df)
## Prod. Gain: glm(formula = InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df)
## 
## =================================================================================
##                        Base        IHC          NN      Prod. Group  Prod. Gain  
## ---------------------------------------------------------------------------------
##   (Intercept)        -1.629***   -1.664***   -1.717***   -1.726***    -1.721***  
##                      (0.266)     (0.273)     (0.286)     (0.294)      (0.293)    
##   Age.c               0.752**     0.567*      0.532       0.480        0.469     
##                      (0.260)     (0.289)     (0.282)     (0.298)      (0.306)    
##   IHC.c                           0.385                                          
##                                  (0.273)                                         
##   wcnscore.c                                  0.573                              
##                                              (0.304)                             
##   Productivity: 1                                         1.219                  
##                                                          (0.730)                 
##   prod.gradient.c                                                      0.593     
##                                                                       (0.364)    
## ---------------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.120       0.143       0.166       0.158        0.156     
##   Log-likelihood    -54.355     -53.375     -52.428     -52.784      -52.853     
##   AIC               112.711     112.750     110.857     111.567      111.706     
##   N                 122         122         122         122          122         
## =================================================================================
```

```r
# save as txt
write.mtable(mtable.inf.knowers, file="graphs/table4-all.txt")
```

#### Model comparisons

```r
# base v. IHC
anova(base.infinity, model.ihc.infinity, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       120     108.71                     
## 2       119     106.75  1   1.9604   0.1615
```

```r
# base v. highest contiguous
anova(base.infinity, model.nn.infinity, test = "LRT")  # significant.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1       120     108.71                       
## 2       119     104.86  1   3.8538  0.04963 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# base v. productivity
anova(base.infinity, model.prod.infinity, test = "LRT")  #n.s. 
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ Productivity + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1       120     108.71                       
## 2       119     105.57  1   3.1433  0.07624 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# base v. productivity gradient
anova(base.infinity, model.gain.infinity, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1       120     108.71                       
## 2       119     105.71  1   3.0041  0.08305 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

#### Visualize

```r
plot_models(model.nn.infinity, model.prod.infinity, model.ihc.infinity, model.gain.infinity, 
    transform = "plogis", show.values = T, show.p = T, colors = "Dark2", show.intercept = T, 
    spacing = 0.7, m.labels = c("Model 1: NN", "Model 2: Productivity", "Model 3: IHC", 
        "Model 4: Prod. Grad"), show.legend = T, title = "Regression analysis, Infinity Knowledge, all participants", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status", prod.gradient.c = "Productivity Gradient"), 
    axis.title = "Endorsement Probability", axis.lim = c(0, 1)) + theme_bw() + ggplot2::geom_hline(yintercept = 0.5, 
    linetype = "dashed")
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-80-1.png)<!-- -->

```r
ggsave("graphs/reg-infinity.png", height = 7, width = 7)
```


### Graph maximal models

```r
large.successor.full.grad <- glm(SuccessorKnower ~ prod.gradient.c + wcnscore.c + 
    IHC.c + Age.c, family = "binomial", data = distinct_model.df)
large.endless.full.grad <- glm(EndlessKnower ~ prod.gradient.c + wcnscore.c + IHC.c + 
    Age.c, family = "binomial", data = distinct_model.df)
large.inf.full.grad <- glm(InfinityKnower ~ prod.gradient.c + wcnscore.c + IHC.c + 
    Age.c, family = "binomial", data = distinct_model.df)

# plot all 3 together
plot_models(large.successor.full.grad, large.endless.full.grad, large.inf.full.grad, 
    transform = "plogis", show.values = TRUE, show.p = T, grid = T, colors = "bw", 
    show.intercept = T, m.labels = c(SuccessorKnower = "Succesor: Can always +1", 
        EndlessKnower = "Endless: Numbers go forever", InfinityKnower = "Full Infinity knowledge"), 
    show.legend = F, title = "Regression analysis, all participants", axis.labels = c(Age.c = "Age", 
        wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", Productivity1 = "Productivity Status", 
        prod.gradient.c = "Productivity Gradient"), axis.title = "Endorsement Probability") + 
    theme_bw() + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed")
```

## Correlations between variables
### LM predicting IHC from NN accuracy and age

```r
lm3 <- lm(IHC ~ wcnscore + Age.c, data = distinct_model.df)
summary(lm3)
```

```
## 
## Call:
## lm(formula = IHC ~ wcnscore + Age.c, data = distinct_model.df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.413 -12.292   0.919  14.198  58.802 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  14.3165     3.8186   3.749 0.000275 ***
## wcnscore      8.3893     0.7725  10.860  < 2e-16 ***
## Age.c         6.9010     2.1159   3.261 0.001446 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 20.76 on 119 degrees of freedom
## Multiple R-squared:  0.6293,	Adjusted R-squared:  0.623 
## F-statistic:   101 on 2 and 119 DF,  p-value: < 2.2e-16
```


## Regressions no ihc=99

### prep

```r
distinct_model.df2 <- model.df %>% filter(Productivity.tertiary != "Productive (IHC ≥ 99)") %>% 
    mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale = TRUE)), Age.c = as.vector(scale(Age, 
        center = TRUE, scale = TRUE)), prod.gradient.c = as.vector(scale(prod.gradient, 
        center = TRUE, scale = TRUE)), wcnscore.c = as.vector(scale(wcnscore, center = TRUE, 
        scale = TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(distinct_model.df2$Productivity) - 1)
contrasts(distinct_model.df2$Productivity) <- c(-wec, 1 - wec)
str(distinct_model.df2)
```

```
## 'data.frame':	90 obs. of  16 variables:
##  $ LadlabID             : Factor w/ 122 levels "010516-K4","010916-D5",..: 1 3 4 5 6 7 8 9 10 11 ...
##  $ Age                  : num  4.17 4 4.44 4.12 4.41 4.75 5.42 4.25 4.6 4.77 ...
##  $ AgeGroup             : Factor w/ 6 levels "4-4.5y","4.5-5y",..: 1 1 1 1 1 2 3 1 2 2 ...
##  $ Gender               : Factor w/ 5 levels "f","F","m","M",..: 1 1 3 1 1 1 2 1 3 1 ...
##  $ SuccessorKnower      : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 2 2 1 ...
##  $ EndlessKnower        : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 1 1 1 1 ...
##  $ InfinityKnower       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ IHC                  : int  13 5 15 39 29 26 39 29 13 77 ...
##  $ Productivity         : Factor w/ 2 levels "Nonproductive",..: 1 1 1 1 2 1 2 2 1 1 ...
##   ..- attr(*, "contrasts")= num [1:2, 1] -0.456 0.544
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr  "Nonproductive" "Productive"
##   .. .. ..$ : NULL
##  $ Productivity.tertiary: chr  "Nonproductive" "Nonproductive" "Nonproductive" "Nonproductive" ...
##  $ prod.gradient        : num  0.186 0 0.179 0 0.857 ...
##  $ wcnscore             : int  0 0 1 2 0 6 8 3 0 7 ...
##  $ IHC.c                : num  -1.048 -1.471 -0.942 0.328 -0.201 ...
##  $ Age.c                : num  -1.318 -1.631 -0.82 -1.41 -0.876 ...
##  $ prod.gradient.c      : num  -0.684 -1.102 -0.701 -1.102 0.824 ...
##  $ wcnscore.c           : num  -1.389 -1.389 -0.962 -0.536 -1.389 ...
```

### Successor models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.successor2 <- glm(SuccessorKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df2)

##IHC model##
model.ihc.successor2 <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df2)
##NN Model##
model.nn.successor2 <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df2)
##Productivity model##
model.prod.successor2 <- glm(SuccessorKnower ~ Productivity + Age.c, family = "binomial",
                              data = distinct_model.df2)

##EXPLORATORY## - GAIN SCORE
model.gain.successor2 <- glm(SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df2)

##Regression table for Successor Knower Models (Table 2)
mtable.sf.knowers2 <- mtable('Base' = base.successor2,
            'IHC' = model.ihc.successor2,
            'NN' = model.nn.successor2,
            'Productivity' = model.prod.successor2,
            'Prod. gain' = model.gain.successor2,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.sf.knowers2
```

```
## 
## Calls:
## Base: glm(formula = SuccessorKnower ~ Age.c, family = "binomial", data = distinct_model.df2)
## IHC: glm(formula = SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## NN: glm(formula = SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Productivity: glm(formula = SuccessorKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Prod. gain: glm(formula = SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## 
## ============================================================================
##                       Base      IHC        NN     Productivity  Prod. gain  
## ----------------------------------------------------------------------------
##   (Intercept)        -0.509*   -0.538*   -0.511*     -0.514*      -0.511*   
##                      (0.220)   (0.227)   (0.221)     (0.221)      (0.221)   
##   Age.c               0.296     0.637*    0.358       0.156        0.206    
##                      (0.221)   (0.287)   (0.242)     (0.267)      (0.274)   
##   IHC.c                        -0.590*                                      
##                                (0.300)                                      
##   wcnscore.c                             -0.155                             
##                                          (0.242)                            
##   Productivity: 1                                     0.496                 
##                                                      (0.532)                
##   prod.gradient.c                                                  0.152    
##                                                                   (0.273)   
## ----------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.027     0.090     0.033       0.040        0.032    
##   Log-likelihood    -58.755   -56.605   -58.547     -58.320      -58.600    
##   AIC               121.509   119.210   123.095     122.640      123.199    
##   N                  90        90        90          90           90        
## ============================================================================
```

```r
write.mtable(mtable.sf.knowers2, file="graphs/table2.txt")
```

#### Model comparisons
Looks like only IHC is a significant predictor. 

```r
# base v. IHC
anova(base.successor2, model.ihc.successor2, test = "LRT")  #IHC significant
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        88     117.51                       
## 2        87     113.21  1   4.2996  0.03812 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# NN v. base
anova(base.successor2, model.nn.successor2, test = "LRT")  #NN not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     117.51                     
## 2        87     117.09  1  0.41472   0.5196
```

```r
# Productivity v. base
anova(base.successor2, model.prod.successor2, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ Productivity + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     117.51                     
## 2        87     116.64  1  0.86926   0.3512
```

```r
## Exploratory vs. base
anova(base.successor2, model.gain.successor2, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     117.51                     
## 2        87     117.20  1  0.30987   0.5778
```

#### best model

```r
summary(model.ihc.successor2)
```

```
## 
## Call:
## glm(formula = SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4905  -0.9731  -0.7610   1.2403   1.7593  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -0.5385     0.2274  -2.368   0.0179 *
## IHC.c        -0.5897     0.2997  -1.968   0.0491 *
## Age.c         0.6367     0.2874   2.216   0.0267 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 119.33  on 89  degrees of freedom
## Residual deviance: 113.21  on 87  degrees of freedom
## AIC: 119.21
## 
## Number of Fisher Scoring iterations: 4
```

```r
plot_model(model.ihc.successor2, type = "pred")
```

```
## $IHC.c
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-86-1.png)<!-- -->

```
## 
## $Age.c
```

![](recursionAnalysis_files/figure-html/unnamed-chunk-86-2.png)<!-- -->

### Endless Models

```r
# Base model
base.endless2 <- glm(EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df2)

### IHC MODEL###
model.ihc.endless2 <- glm(EndlessKnower ~ IHC.c + Age.c, family = "binomial", data = distinct_model.df2)

### WCN MODEL###
model.nn.endless2 <- glm(EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
    data = distinct_model.df2)

### PRODUCTIVITY MODEL###
model.prod.endless2 <- glm(EndlessKnower ~ Productivity + Age.c, family = "binomial", 
    data = distinct_model.df2)

## EXPLORATORY## - GAIN SCORE
model.gain.endless2 <- glm(EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
    data = distinct_model.df2)

## Regression table for Endless Models
mtable.endless.knowers2 <- mtable(Base = base.endless2, IHC = model.ihc.endless2, 
    NN = model.nn.endless2, Productivity = model.prod.endless2, `Prod. gradient` = model.gain.endless2, 
    summary.stats = c("Nagelkerke R-sq.", "Log-likelihood", "AIC", "F", "p", "N"))

mtable.endless.knowers2
```

```
## 
## Calls:
## Base: glm(formula = EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df2)
## IHC: glm(formula = EndlessKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## NN: glm(formula = EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Productivity: glm(formula = EndlessKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Prod. gradient: glm(formula = EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## 
## ======================================================================================
##                        Base        IHC          NN      Productivity  Prod. gradient  
## --------------------------------------------------------------------------------------
##   (Intercept)        -1.519***   -1.543***   -1.578***    -1.667***      -1.690***    
##                      (0.284)     (0.289)     (0.298)      (0.323)        (0.328)      
##   Age.c               0.451       0.271       0.287        0.025         -0.065       
##                      (0.274)     (0.324)     (0.296)      (0.334)        (0.358)      
##   IHC.c                           0.330                                               
##                                  (0.315)                                              
##   wcnscore.c                                  0.466                                   
##                                              (0.308)                                  
##   Productivity: 1                                          1.625*                     
##                                                           (0.726)                     
##   prod.gradient.c                                                         0.914*      
##                                                                          (0.394)      
## --------------------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.049       0.068       0.090        0.142          0.154       
##   Log-likelihood    -42.222     -41.683     -41.036      -39.462        -39.107       
##   AIC                88.445      89.367      88.072       84.924         84.214       
##   p                   0.095       0.145       0.076        0.016          0.011       
##   N                  90          90          90           90             90           
## ======================================================================================
```

```r
write.mtable(mtable.endless.knowers2, file = "graphs/table3.txt")
```

#### Model comparisons
Only productivity (& productivity gain) is significant

```r
# base v. IHC
anova(base.endless2, model.ihc.endless2, test = "LRT")  #IHC not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     84.445                     
## 2        87     83.367  1   1.0777   0.2992
```

```r
# base v. highest contiguous
anova(model.nn.endless2, base.endless2, test = "LRT")  #not significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ wcnscore.c + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        87     82.072                     
## 2        88     84.445 -1   -2.373   0.1234
```

```r
# base v. productivity
anova(model.prod.endless2, base.endless2, test = "LRT")  #Prod significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Productivity + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        87     78.924                       
## 2        88     84.445 -1  -5.5209  0.01879 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
## Exploratory base v. productivity gradient
anova(base.endless2, model.gain.endless2, test = "LRT")  # prod. gradient significant
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        88     84.445                       
## 2        87     78.214  1   6.2302  0.01256 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


### Full Infinity Knowledge models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.infinity2 <- glm(InfinityKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df2)

##IHC model
model.ihc.infinity2 <- glm(InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df2)
##Highest NN Model
model.nn.infinity2 <- glm(InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df2)
##Productivity model
model.prod.infinity2 <- glm(InfinityKnower ~ Productivity + Age.c, family = "binomial",
                              data = distinct_model.df2)

##Gain Score model
model.gain.infinity2 <- glm(InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df2)

##Regression table for Infinity Knower Models (Table 4)
mtable.inf.knowers2 <- mtable('Base' = base.infinity2,
            'IHC' = model.ihc.infinity2,
            'NN' = model.nn.infinity2,
            'Prod. Group' = model.prod.infinity2,
            'Prod. Gain' = model.gain.infinity2,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.inf.knowers2
```

```
## 
## Calls:
## Base: glm(formula = InfinityKnower ~ Age.c, family = "binomial", data = distinct_model.df2)
## IHC: glm(formula = InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## NN: glm(formula = InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Prod. Group: glm(formula = InfinityKnower ~ Productivity + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## Prod. Gain: glm(formula = InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df2)
## 
## =================================================================================
##                        Base        IHC          NN      Prod. Group  Prod. Gain  
## ---------------------------------------------------------------------------------
##   (Intercept)        -1.972***   -1.973***   -2.003***   -2.075***    -2.067***  
##                      (0.337)     (0.337)     (0.346)     (0.370)      (0.367)    
##   Age.c               0.527       0.503       0.417       0.207        0.186     
##                      (0.317)     (0.377)     (0.341)     (0.379)      (0.398)    
##   IHC.c                           0.043                                          
##                                  (0.372)                                         
##   wcnscore.c                                  0.307                              
##                                              (0.349)                             
##   Productivity: 1                                         1.229                  
##                                                          (0.823)                 
##   prod.gradient.c                                                      0.612     
##                                                                       (0.429)    
## ---------------------------------------------------------------------------------
##   Nagelkerke R-sq.    0.058       0.058       0.073       0.104        0.101     
##   Log-likelihood    -33.907     -33.900     -33.514     -32.714      -32.795     
##   AIC                71.814      73.801      73.028      71.428       71.590     
##   N                  90          90          90          90           90         
## =================================================================================
```

```r
# save as txt
write.mtable(mtable.inf.knowers2, file="graphs/table4.txt")
```

#### Model comparisons
No variable is predictive. 

```r
# base v. IHC
anova(base.infinity2, model.ihc.infinity2, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     67.814                     
## 2        87     67.801  1 0.013362    0.908
```

```r
# base v. highest contiguous
anova(base.infinity2, model.nn.infinity2, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     67.814                     
## 2        87     67.028  1  0.78552   0.3755
```

```r
# base v. productivity
anova(base.infinity2, model.prod.infinity2, test = "LRT")  #n.s. 
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ Productivity + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     67.814                     
## 2        87     65.428  1   2.3862   0.1224
```

```r
# base v. productivity gradient
anova(base.infinity2, model.gain.infinity2, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        88     67.814                     
## 2        87     65.590  1   2.2236   0.1359
```

### Visualizations

```r
large.endless.full2 <- glm(EndlessKnower ~ Productivity + wcnscore.c + IHC.c + Age.c, 
    family = "binomial", data = distinct_model.df2)
large.successor.full2 <- glm(SuccessorKnower ~ Productivity + wcnscore.c + IHC.c + 
    Age.c, family = "binomial", data = distinct_model.df2)
large.inf.full2 <- glm(InfinityKnower ~ Productivity + wcnscore.c + IHC.c + Age.c, 
    family = "binomial", data = distinct_model.df2)
# plot all 3 together
plot_models(large.successor.full2, large.endless.full2, large.inf.full2, transform = "plogis", 
    show.values = TRUE, show.p = T, grid = T, colors = "bw", show.intercept = T, 
    m.labels = c(SuccessorKnower = "Succesor: Can always +1", EndlessKnower = "Endless: Numbers go forever", 
        InfinityKnower = "Full Infinity knowledge"), show.legend = F, title = "Regression analysis, exclude IHC ≥ 99", 
    axis.labels = c(Age.c = "Age", wcnscore.c = "Next Number accuracy", IHC.c = "Initial Highest Count", 
        Productivity1 = "Productivity Status"), axis.title = "Endorsement Probability") + 
    theme_bw() + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed")
```

## Regressions productive kids only

### prep

```r
distinct_model.df3 <- model.df %>% filter(Productivity.tertiary != "Nonproductive") %>% 
    mutate(Productivity.tertiary = factor(Productivity.tertiary), IHC.c = as.vector(scale(IHC, 
        center = TRUE, scale = TRUE)), Age.c = as.vector(scale(Age, center = TRUE, 
        scale = TRUE)), prod.gradient.c = as.vector(scale(prod.gradient, center = TRUE, 
        scale = TRUE)), wcnscore.c = as.vector(scale(wcnscore, center = TRUE, scale = TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(distinct_model.df3$Productivity.tertiary) - 1)
contrasts(distinct_model.df3$Productivity.tertiary) <- c(-wec, 1 - wec)
```

### Successor models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.successor3 <- glm(SuccessorKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df3)

##IHC model##
model.ihc.successor3 <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df3)
##NN Model##
model.nn.successor3 <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df3)
##Productivity model##
model.prod.successor3 <- glm(SuccessorKnower ~ Productivity.tertiary + Age.c, family = "binomial",
                              data = distinct_model.df3)

##EXPLORATORY## - GAIN SCORE
model.gain.successor3 <- glm(SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df3)

##Regression table for Successor Knower Models (Table 3)
mtable.sf.knowers3 <- mtable('Base' = base.successor3,
            'IHC' = model.ihc.successor3,
            'NN' = model.nn.successor3,
            'Productivity' = model.prod.successor3,
            'Prod. gain' = model.gain.successor3,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.sf.knowers3
```

```
## 
## Calls:
## Base: glm(formula = SuccessorKnower ~ Age.c, family = "binomial", data = distinct_model.df3)
## IHC: glm(formula = SuccessorKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## NN: glm(formula = SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## Productivity: glm(formula = SuccessorKnower ~ Productivity.tertiary + Age.c, 
##     family = "binomial", data = distinct_model.df3)
## Prod. gain: glm(formula = SuccessorKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## 
## ====================================================================================
##                               Base      IHC        NN     Productivity  Prod. gain  
## ------------------------------------------------------------------------------------
##   (Intercept)                 0.027     0.027     0.027       0.028        0.042    
##                              (0.234)   (0.234)   (0.235)     (0.235)      (0.240)   
##   Age.c                       0.059     0.069     0.036       0.041        0.158    
##                              (0.236)   (0.239)   (0.241)     (0.238)      (0.249)   
##   IHC.c                                -0.056                                       
##                                        (0.239)                                      
##   wcnscore.c                                      0.124                             
##                                                  (0.241)                            
##   Productivity.tertiary: 1                                    0.390                 
##                                                              (0.476)                
##   prod.gradient.c                                                         -0.421    
##                                                                           (0.317)   
## ------------------------------------------------------------------------------------
##   Nagelkerke R-sq.            0.001     0.002     0.006       0.013        0.043    
##   Log-likelihood            -50.561   -50.534   -50.428     -50.224      -49.408    
##   AIC                       105.123   107.068   106.856     106.448      104.815    
##   N                          73        73        73          73           73        
## ====================================================================================
```

```r
write.mtable(mtable.sf.knowers3, file="graphs/table2-prodonly.txt")
```

#### Model comparisons
Productivity gradient marginally significant, in negative direction.

```r
# base v. IHC
anova(base.successor3, model.ihc.successor3, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     101.12                     
## 2        70     101.07  1  0.05408   0.8161
```

```r
# NN v. base
anova(base.successor3, model.nn.successor3, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     101.12                     
## 2        70     100.86  1  0.26686   0.6054
```

```r
# Productivity v. base
anova(base.successor3, model.prod.successor3, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ Productivity.tertiary + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     101.12                     
## 2        70     100.45  1  0.67451   0.4115
```

```r
## Exploratory vs. base
anova(base.successor3, model.gain.successor3, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: SuccessorKnower ~ Age.c
## Model 2: SuccessorKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71    101.123                     
## 2        70     98.815  1   2.3071   0.1288
```


### Endless Models

```r
# Base model
base.endless3 <- glm(EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df3)

### IHC MODEL###
model.ihc.endless3 <- glm(EndlessKnower ~ IHC.c + Age.c, family = "binomial", data = distinct_model.df3)

### WCN MODEL###
model.nn.endless3 <- glm(EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
    data = distinct_model.df3)

### PRODUCTIVITY MODEL###
model.prod.endless3 <- glm(EndlessKnower ~ Productivity.tertiary + Age.c, family = "binomial", 
    data = distinct_model.df3)

## EXPLORATORY## - GAIN SCORE
model.gain.endless3 <- glm(EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
    data = distinct_model.df3)

## Regression table for Endless Models
mtable.endless.knowers3 <- mtable(Base = base.endless3, IHC = model.ihc.endless3, 
    NN = model.nn.endless3, Productivity = model.prod.endless3, `Prod. gradient` = model.gain.endless3, 
    summary.stats = c("Nagelkerke R-sq.", "Log-likelihood", "AIC", "F", "p", "N"))

mtable.endless.knowers3
```

```
## 
## Calls:
## Base: glm(formula = EndlessKnower ~ Age.c, family = "binomial", data = distinct_model.df3)
## IHC: glm(formula = EndlessKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## NN: glm(formula = EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## Productivity: glm(formula = EndlessKnower ~ Productivity.tertiary + Age.c, 
##     family = "binomial", data = distinct_model.df3)
## Prod. gradient: glm(formula = EndlessKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## 
## ========================================================================================
##                               Base      IHC        NN     Productivity  Prod. gradient  
## ----------------------------------------------------------------------------------------
##   (Intercept)                -0.365    -0.378    -0.397      -0.379         -0.394      
##                              (0.240)   (0.244)   (0.249)     (0.245)        (0.248)     
##   Age.c                       0.227     0.170     0.147       0.193          0.152      
##                              (0.245)   (0.249)   (0.253)     (0.249)        (0.254)     
##   IHC.c                                 0.381                                           
##                                        (0.251)                                          
##   wcnscore.c                                      0.526                                 
##                                                  (0.275)                                
##   Productivity.tertiary: 1                                    0.863                     
##                                                              (0.491)                    
##   prod.gradient.c                                                            0.418      
##                                                                             (0.383)     
## ----------------------------------------------------------------------------------------
##   Nagelkerke R-sq.            0.016     0.058     0.088       0.072          0.046      
##   Log-likelihood            -48.997   -47.817   -46.981     -47.424        -48.156      
##   AIC                       101.994   101.634    99.962     100.848        102.313      
##   p                           0.349     0.198     0.086       0.134          0.278      
##   N                          73        73        73          73             73          
## ========================================================================================
```

```r
write.mtable(mtable.endless.knowers3, file = "graphs/table3-prodonly.txt")
```

#### Model comparisons
WCN is significant.

```r
# base v. IHC
anova(base.endless3, model.ihc.endless3, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     97.994                     
## 2        70     95.634  1   2.3598   0.1245
```

```r
# base v. highest contiguous
anova(model.nn.endless3, base.endless3, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ wcnscore.c + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        70     93.962                       
## 2        71     97.994 -1  -4.0326  0.04463 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# base v. productivity
anova(model.prod.endless3, base.endless3, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Productivity.tertiary + Age.c
## Model 2: EndlessKnower ~ Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        70     94.848                       
## 2        71     97.994 -1  -3.1462  0.07611 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
## Exploratory base v. productivity gradient
anova(base.endless3, model.gain.endless3, test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: EndlessKnower ~ Age.c
## Model 2: EndlessKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     97.994                     
## 2        70     96.313  1   1.6814   0.1947
```

Final model

```r
summary(model.nn.endless3)
```

```
## 
## Call:
## glm(formula = EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3371  -1.0648  -0.6822   1.1485   1.7596  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -0.3965     0.2487  -1.594   0.1108  
## wcnscore.c    0.5262     0.2750   1.913   0.0557 .
## Age.c         0.1469     0.2526   0.582   0.5609  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 98.872  on 72  degrees of freedom
## Residual deviance: 93.962  on 70  degrees of freedom
## AIC: 99.962
## 
## Number of Fisher Scoring iterations: 4
```


### Full Infinity Knowledge models

```r
###MODEL BUILDING AND COMPARISONS###
#base model for successor knower
base.infinity3 <- glm(InfinityKnower ~ Age.c, family = "binomial", 
                        data = distinct_model.df3)

##IHC model
model.ihc.infinity3 <- glm(InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
                             data = distinct_model.df3)
##Highest NN Model
model.nn.infinity3 <- glm(InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
                            data = distinct_model.df3)
##Productivity model
model.prod.infinity3 <- glm(InfinityKnower ~ Productivity.tertiary + Age.c, family = "binomial",
                              data = distinct_model.df3)

##Gain Score model
model.gain.infinity3 <- glm(InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial",
                              data = distinct_model.df3)

##Regression table for Infinity Knower Models (Table 4)
mtable.inf.knowers3 <- mtable('Base' = base.infinity3,
            'IHC' = model.ihc.infinity3,
            'NN' = model.nn.infinity3,
            'Prod. Group' = model.prod.infinity3,
            'Prod. Gain' = model.gain.infinity3,
            #summary.stats = c('R-squared','F','p','N'))
            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'))
mtable.inf.knowers3
```

```
## 
## Calls:
## Base: glm(formula = InfinityKnower ~ Age.c, family = "binomial", data = distinct_model.df3)
## IHC: glm(formula = InfinityKnower ~ IHC.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## NN: glm(formula = InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## Prod. Group: glm(formula = InfinityKnower ~ Productivity.tertiary + Age.c, 
##     family = "binomial", data = distinct_model.df3)
## Prod. Gain: glm(formula = InfinityKnower ~ prod.gradient.c + Age.c, family = "binomial", 
##     data = distinct_model.df3)
## 
## =========================================================================================
##                                Base        IHC          NN      Prod. Group  Prod. Gain  
## -----------------------------------------------------------------------------------------
##   (Intercept)                -0.998***   -1.003***   -1.049***   -1.015***    -1.007***  
##                              (0.268)     (0.269)     (0.281)     (0.272)      (0.271)    
##   Age.c                       0.323       0.296       0.253       0.295        0.287     
##                              (0.280)     (0.282)     (0.283)     (0.279)      (0.287)    
##   IHC.c                                   0.158                                          
##                                          (0.276)                                         
##   wcnscore.c                                          0.474                              
##                                                      (0.315)                             
##   Productivity.tertiary: 1                                        0.572                  
##                                                                  (0.536)                 
##   prod.gradient.c                                                              0.181     
##                                                                               (0.365)    
## -----------------------------------------------------------------------------------------
##   Nagelkerke R-sq.            0.028       0.034       0.076       0.050        0.033     
##   Log-likelihood            -42.163     -41.999     -40.905     -41.590      -42.021     
##   AIC                        88.326      89.997      87.810      89.180       90.043     
##   N                          73          73          73          73           73         
## =========================================================================================
```

```r
# save as txt
write.mtable(mtable.inf.knowers3, file="graphs/table4-prodonly.txt")
```

#### Model comparisons
No variable is predictive. 

```r
# base v. IHC
anova(base.infinity3, model.ihc.infinity3, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ IHC.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     84.326                     
## 2        70     83.997  1  0.32886   0.5663
```

```r
# base v. highest contiguous
anova(base.infinity3, model.nn.infinity3, test = "LRT")  # n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ wcnscore.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     84.326                     
## 2        70     81.810  1   2.5163   0.1127
```

```r
# base v. productivity
anova(base.infinity3, model.prod.infinity3, test = "LRT")  #n.s. 
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ Productivity.tertiary + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     84.326                     
## 2        70     83.180  1   1.1464   0.2843
```

```r
# base v. productivity gradient
anova(base.infinity3, model.gain.infinity3, test = "LRT")  #n.s.
```

```
## Analysis of Deviance Table
## 
## Model 1: InfinityKnower ~ Age.c
## Model 2: InfinityKnower ~ prod.gradient.c + Age.c
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        71     84.326                     
## 2        70     84.043  1  0.28333   0.5945
```
