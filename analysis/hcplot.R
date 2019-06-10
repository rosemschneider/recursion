## Setup ====
# rm(list = ls())
# Set WD
setwd("~/Sites/recursion/")
library(tidyverse)
set.seed(43)
'%!in%' <- function(x,y)!('%in%'(x,y))
# cb friendly red blue green
mypalette <- c("#D55E00", "#0073B3", "#009E73")
# cb friendly green, orange, bright blue for fig3
mypalette3 <- c("#009E73", "#E69F00", "#56B4E9")

## Load data ====
hc.recoded <- read.csv('data/HCdata_jccoding.csv', stringsAsFactors = F)
load("/Users/junyichu/Sites/recursion/data/includedsubj.RData")
hc.recoded %<>% filter(LadlabID %in% unique.full)

## Check participant N, etc ====
# Without decade errors
hc.recoded %>% dplyr::select(LadlabID, prod_tomerge, error.start) %>%
  unique() %>% filter(!is.na(error.start)) %>%
  count(LadlabID, prod_tomerge) %>%
  count(n) %>%
  dplyr::rename(n.errors=n, n.subj=nn)
# how many decade prompts
hc.recoded %>% dplyr::select(LadlabID, prod_tomerge, decadesprompted) %>%
  unique() %>% filter(!is.na(decadesprompted)) %>%
  count(LadlabID, prod_tomerge) %>%
  count(n) %>%
  dplyr::rename(n.decades=n, n.subj=nn)


## Plot Figure 2a ====
# Select only productive kids and reorder by ihc, tiebreak with fhc, then dce.
hc.recoded.prod <- hc.recoded %>% filter(prod_tomerge=="prod") %>%
  dplyr::select(-decadesprompted) %>%
  unique()
hc.recoded.prod$LadlabID = fct_reorder(hc.recoded.prod$LadlabID, hc.recoded.prod$fhc, min)
hc.recoded.prod$LadlabID = fct_reorder(hc.recoded.prod$LadlabID, hc.recoded.prod$dce, min)
hc.recoded.prod$LadlabID = fct_reorder(hc.recoded.prod$LadlabID, hc.recoded.prod$ihc, min)

ggplot(hc.recoded.prod, aes(x=LadlabID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc), color="black", stroke=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5), 
                 color="#D95F02", size=1.1) +
  geom_point(aes(y=error.start), color="#D95F02", shape=16, size=2) +
  geom_point(aes(y=fhc), color="#7570B3", shape=16, size=3) +
  geom_point(aes(y=dce+1), color="#D95F02", shape=17, size=3) +
  geom_point(aes(y=ihc), color="#1B9E77", shape=16, size=2) +
#  geom_point(aes(y=dce), color="#D95F02", shape=6, size=2) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  labs(title="a. Distance, Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count",
       colour="Highest Count Coding",
       shape="Highest Count Coding") +
  theme_bw(base_size = 12) + 
  theme(legend.position="bottom", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank())

#### try to add legends
ggplot(hc.recoded.prod, aes(x=LadlabID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc), color="black", size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5), 
                 color="#D95F02", size=1) +
  geom_point(aes(y=error.start, color="Error - uncorrected", shape="Error - uncorrected"), size=2) +
  geom_point(aes(y=fhc, color="Final Highest Count", shape="Final Highest Count"), size=2) +
  geom_point(aes(y=dce+1, color="Error - first Decade Prompt", shape="Error - first Decade Prompt"), size=3) +
  geom_point(aes(y=ihc, color="Initial Highest Count", shape="Initial Highest Count"), size=2) +
#  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_color_manual(name = "Highest Count Coding",
                       breaks = c("Final Highest Count", "Error - first Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                       values = c("Final Highest Count"="#7570B3", "Error - first Decade Prompt"="#D95F02", "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                       guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - first Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=16, "Error - first Decade Prompt"=17, "Error - uncorrected"=16, "Initial Highest Count"=16),
                       guide = "legend") +
  labs(title="a. Distance, Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count",
       colour="Highest Count Coding",
       shape="Highest Count Coding") +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank())
ggsave('graphs/distance-prod-sorted-wprompts.png', width=10, height=4)


## Figure 2b ====
hc.recoded %>% filter(prod_tomerge=="nonprod") %>%
  dplyr::select(-decadesprompted) %>%
  unique() %>%
  mutate(LadlabID = fct_reorder(LadlabID, fhc, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, dce, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, ihc, min)) %>%
  ggplot(aes(x=LadlabID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc), color="black", size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5), 
                 color="#D95F02", size=1) +
  geom_point(aes(y=error.start, color="Error - uncorrected", shape="Error - uncorrected"), size=2) +
  geom_point(aes(y=fhc, color="Final Highest Count", shape="Final Highest Count"), size=2) +
  geom_point(aes(y=dce+1, color="Error - first Decade Prompt", shape="Error - first Decade Prompt"), size=3) +
  geom_point(aes(y=ihc, color="Initial Highest Count", shape="Initial Highest Count"), size=2) +
  #  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_color_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - first Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"="#7570B3", "Error - first Decade Prompt"="#D95F02", "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                     guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - first Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=16, "Error - first Decade Prompt"=17, "Error - uncorrected"=16, "Initial Highest Count"=16),
                     guide = "legend") +
  labs(title="b. Distance, Nonproductive Counters",
       x = "Each line = individual participant",
       y="Highest Count",
       colour="Highest Count Coding",
       shape="Highest Count Coding") +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        #axis.text.x = element_text(angle = 370, hjust = 1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank())
ggsave('graphs/distance-nonprod-sorted-wprompts.png', width=10, height=4)



## FULL DATA PLOT ====
hc.recoded %>% filter(prod_tomerge=="prod") %>%
  mutate(LadlabID = fct_reorder(LadlabID, fhc, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, dce, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, ihc, min)) %>%
  ggplot(aes(x=LadlabID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc), color="black", size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5), 
                 color="#D95F02", size=1) +
  geom_point(aes(y=error.start, color="Error - uncorrected", shape="Error - uncorrected"), size=2) +
  geom_point(aes(y=decadesprompted, color="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3) +
  geom_point(aes(y=fhc, color="Final Highest Count", shape="Final Highest Count"), size=2) +
  geom_point(aes(y=ihc, color="Initial Highest Count", shape="Initial Highest Count"), size=2) +
  #  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_color_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"="#7570B3", "Error - got Decade Prompt"="#D95F02", "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                     guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=16, "Error - got Decade Prompt"=17, "Error - uncorrected"=16, "Initial Highest Count"=16),
                     guide = "legend") +
  labs(title="a. Distance, Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count",
       colour="Highest Count Coding",
       shape="Highest Count Coding") +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        #axis.text.x = element_text(angle = 370, hjust = 1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank())
ggsave('graphs/distance-prod-sorted-allprompts.png', width=10, height=4)


hc.recoded %>% filter(prod_tomerge=="nonprod") %>%
  mutate(LadlabID = fct_reorder(LadlabID, fhc, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, dce, min)) %>%
  mutate(LadlabID = fct_reorder(LadlabID, ihc, min)) %>%
  ggplot(aes(x=LadlabID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc), color="black", size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5), 
                 color="#D95F02", size=1) +
  geom_point(aes(y=error.start, color="Error - uncorrected", shape="Error - uncorrected"), size=2) +
  geom_point(aes(y=decadesprompted, color="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3) +
  geom_point(aes(y=fhc, color="Final Highest Count", shape="Final Highest Count"), size=2) +
  geom_point(aes(y=ihc, color="Initial Highest Count", shape="Initial Highest Count"), size=2) +
  #  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_color_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"="#7570B3", "Error - got Decade Prompt"="#D95F02", "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                     guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=16, "Error - got Decade Prompt"=17, "Error - uncorrected"=16, "Initial Highest Count"=16),
                     guide = "legend") +
  labs(title="b. Distance, Non-Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count",
       colour="Highest Count Coding",
       shape="Highest Count Coding") +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme_bw(base_size = 12) + 
  theme(legend.position="right", 
        #axis.text.x = element_text(angle = 370, hjust = 1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank())
ggsave('graphs/distance-nonprod-sorted-allprompts.png', width=10, height=4)
