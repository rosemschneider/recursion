rm(list = ls())
# Load cleaned data
load("CountingToInfinity-data.RData")

# Custom global variables
# colorblind friendly red blue green
myRGBpalette <- c("#D55E00", "#0073B3", "#009E73")
# colorblind friendly green, orange, bright blue
myGOBpalette <- c("#009E73", "#E69F00", "#56B4E9")

library(tidyverse)
library(ggpubr)
theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0))))
# --- PLOT: Histogram of IHC, by productivity ----
## three-way productivity
fig1 <- ggplot(data.hcunique, aes(x=IHC)) + 
  geom_dotplot(aes(fill = Productivity.tertiary),
               binwidth=1, stackgroups=TRUE, binpositions="all",method="dotdensity", dotsize = 1) +
  scale_fill_manual(values=myRGBpalette, name="Productivity Group") +
  coord_fixed(ratio=1) +
  scale_y_continuous(breaks=seq(0,40,10), lim=c(0,35)) +
  scale_x_continuous(breaks=seq(0,100,by=10)) +
  labs(x="Initial Highest Count", 
       y="Frequency") +
  theme(legend.position="bottom", 
        panel.grid.minor = element_blank())
ggsave('graphs/fig1.png',fig1,dpi=600,
       width=6, height=3)

# --- PLOT: Productivity by Age ----
fig1b <- ggplot(data.hcunique, aes(x=AgeMonths)) + 
  geom_dotplot(aes(fill = Productivity.tertiary), color="black",
               binwidth = 1,
               stackgroups=TRUE, binpositions="all") +
  coord_fixed(ratio=1) +
  scale_y_continuous(breaks=seq(0,10,5), lim=c(0,13)) +
  scale_x_continuous(breaks=seq(48,73,by=6)) +
  scale_color_manual(values=myRGBpalette, name="Productivity Classification") +
  scale_fill_manual(values=myRGBpalette, name="Productivity Classification") +
  labs( x="Age in Months", 
        y="Frequency") +
  theme(legend.position="right")
ggsave('graphs/fig1b-prod-by-age.png', fig1b,
       width=6, height=3, dpi=600)

# --- PLOT: IHC by Age ----
fig1c <- data.hcunique %>% ggplot(aes(x=AgeMonths, y=IHC, color=Productivity.tertiary, fill=Productivity.tertiary)) +
  geom_smooth(method="lm") +
  geom_point(alpha=0.5, position=position_dodge2(width=.3)) +
  scale_color_manual(values=myRGBpalette, name="Productivity Classification",
                     guide = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=myRGBpalette, name="Productivity Classification",
                    guide = guide_legend(reverse=TRUE)) +
  labs(x="Age in Months", y="Initial Highest Count") +
  scale_x_continuous(breaks=seq(48,73,by=6)) +
  theme(legend.position="right")
ggsave('graphs/fig1c-ihc-by-age-prod.png', fig1c, width=6, height=3, dpi=600)

## --- FIG 2----
## First let's plot productive counters
fig2a <- data.hcerrors %>% filter(prod_tomerge=="prod") %>%
  ggplot(aes(x=subID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc, color="Accurate Sequence", linetype="Accurate Sequence"), size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5, color="Skipped Sequence", linetype="Skipped Sequence"), size=1) +
  geom_point(aes(y=error.start, fill="Error - uncorrected", shape="Error - uncorrected"), size=2, stroke = 0) +
  geom_point(aes(y=decadesprompted, fill="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3, stroke = 0) +
  geom_point(aes(y=fhc, fill="Final Highest Count", shape="Final Highest Count"), size=2, stroke = 0) +
  geom_point(aes(y=ihc, fill="Initial Highest Count", shape="Initial Highest Count"), size=2, stroke = 0) +
  #  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_fill_manual(name = "Highest Count Coding",
                    breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                    values = c("Final Highest Count"="#7570B3", "Error - got Decade Prompt"="#D95F02", "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                    guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=21, "Error - got Decade Prompt"=24, "Error - uncorrected"=21, "Initial Highest Count"=21),
                     guide = "legend") +
  scale_color_manual(name = "Counting Progress",
                     breaks = c("Accurate Sequence", "Skipped Sequence"),
                     values = c("Accurate Sequence"="black", "Skipped Sequence"="grey70"),
                     guide = "legend") +
  scale_linetype_manual(name = "Counting Progress",
                        guide="legend",
                        breaks = c("Accurate Sequence", "Skipped Sequence"),
                        values = c("Accurate Sequence"="solid", "Skipped Sequence"="solid")) +
  labs(subtitle = "a. Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count") +
  theme(legend.position="none", panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust=0.5, size=8))

# Same plot, for Non-productive kids
fig2b <- data.hcerrors %>% filter(prod_tomerge=="nonprod") %>%
  ggplot(aes(x=subID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc, color="Accurate Sequence", linetype="Accurate Sequence"), size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5, color="Skipped Sequence", linetype="Skipped Sequence"), size=1) +
  geom_point(aes(y=error.start, fill="Error - uncorrected", shape="Error - uncorrected"), size=2, stroke = 0) +
  geom_point(aes(y=decadesprompted, fill="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3, stroke = 0) +
  geom_point(aes(y=fhc, fill="Final Highest Count", shape="Final Highest Count"), size=2, stroke = 0) +
  geom_point(aes(y=ihc, fill="Initial Highest Count", shape="Initial Highest Count"), size=2, stroke = 0) +
  #  geom_point(aes(y=dce, color="#D95F02", shape="6"), size=2.5) +
  scale_y_continuous(breaks=seq(0,100,20), lim=c(0,100)) +
  scale_fill_manual(name = "Highest Count Coding",
                    breaks = c("Final Highest Count", "Error - got Decade Prompt", 
                               "Error - uncorrected", "Initial Highest Count"),
                    values = c("Final Highest Count"="#7570B3", "Error - got Decade Prompt"="#D95F02", 
                               "Error - uncorrected"="#D95F02", "Initial Highest Count"="#1B9E77"),
                    guide = "legend") +
  scale_shape_manual(name = "Highest Count Coding",
                     breaks = c("Final Highest Count", "Error - got Decade Prompt", 
                                "Error - uncorrected", "Initial Highest Count"),
                     values = c("Final Highest Count"=21, "Error - got Decade Prompt"=24, 
                                "Error - uncorrected"=21, "Initial Highest Count"=21),
                     guide = "legend") +
  scale_color_manual(name = "Counting Progress",
                     breaks = c("Accurate Sequence", "Skipped Sequence"),
                     values = c("Accurate Sequence"="black", "Skipped Sequence"="grey70"),
                     guide = "legend") +
  scale_linetype_manual(name = "Counting Progress",
                        guide="legend",
                        breaks = c("Accurate Sequence", "Skipped Sequence"),
                        values = c("Accurate Sequence"="solid", "Skipped Sequence"="solid")) +
  labs(subtitle = "b. Non-Productive Counters",
       x = "Each line = individual participant",
       y="Highest Count") +
  theme(legend.position="right", panel.grid.major.x = element_blank(),legend.margin=margin(0,0,0,0),
        axis.text.x = element_text(angle = 90, vjust=0.5, size=8))
# save
ggsave('graphs/fig2a-distance-prod-sorted-allprompts.png', fig2a, width=7, height=3, dpi=600)
ggsave('graphs/fig2b-distance-nonprod-sorted-allprompts.png', fig2b, width=7, height=3, dpi=600)
ggsave('graphs/fig2-distance.png', ggarrange(fig2a, fig2b, nrow=2), width=7, height=5)

## Fig 3----
fig3.data <- data.wcn %>%
  filter(TaskType == "immediate")%>%
  mutate(TaskItem_type= ifelse(mod(TaskItem_num,10)==9, "Item Type: Decade transition", "Item Type: Mid-decade")) %>%
  mutate(TaskItem_type_ordered = ordered(TaskItem_type, levels=c("Item Type: Mid-decade", "Item Type: Decade transition")))
# PLOT: NN accuracy by item & prod ----
fig3.data %>%
  #  mutate(Productivity.tertiary = factor(Productivity.tertiary, levels = c("Nonproductive", "Productive (IHC ≥ 99)", "Productive (IHC < 99)")))%>%
  group_by(TaskItem_type_ordered, TaskItem, Productivity.tertiary)%>%
  summarise(mean = mean(Accuracy, na.rm = TRUE), 
            n = n(), 
            sd = sd(Accuracy, na.rm = TRUE), 
            se = sd/sqrt(n)) %>%
  ggplot(aes(x = factor(TaskItem), y = mean, colour = Productivity.tertiary, group= Productivity.tertiary)) +
  geom_point(size = 2.5) + 
  geom_line(size = .7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0, size = .5) +
  facet_grid(~TaskItem_type_ordered, scales = "free_x")+#, space = "free_x") +
  scale_y_continuous( labels = scales::percent)+
  scale_colour_manual(name="Productivity classification",
                      values = myRGBpalette, 
                      labels=c("Non-Productive", "Productive (IHC < 99)", "Productive (IHC ≥ 99)"),
                      guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = "right") +
  labs(x = "Item magnitude", y = "Accuracy") +
  theme(axis.text.x = element_text(hjust = 0.5))
ggsave('graphs/fig3-wcn-trial-accuracy.png',dpi=600,
       width=6, height=3)

## ---- PLOT Fig 4 ----
nn.long <- fig3.data %>%
  dplyr::select(c(subID, IHC, FHC, Age, Productivity, Productivity.tertiary,
                  TaskItem_num, TaskItem_type, Accuracy, WithinOutsideIHC)) %>%
  mutate(IHC = as.integer(IHC), subID=as.factor(subID), TaskItem_type=as.factor(TaskItem_type))
nn.long %>%
  mutate(WithinOutsideIHC = factor(WithinOutsideIHC, levels = c("within", "outside"), 
                                   labels = c("Within IHC", "Beyond IHC")))%>%
  dplyr::group_by(Productivity.tertiary, WithinOutsideIHC, subID) %>%
  dplyr::summarize(meansubj = mean(Accuracy, na.rm = TRUE)) %>%
  ggplot(aes(x=WithinOutsideIHC, y=meansubj, colour = Productivity.tertiary, group= Productivity.tertiary)) +
  stat_summary(fun.y="mean", geom="line")+#, position = position_dodge(width=0.3)) +
  stat_summary(fun.data = mean_cl_boot, geom="errorbar",#position = position_dodge(width=0.3), 
               width = 0.1)+
  stat_summary(fill="white",
               fun.y = mean,
               #position = position_dodge(width=0.3), 
               geom="point", shape=23, size=3) +
  scale_colour_manual(name="Productivity classification",
                      values = myRGBpalette, 
                      labels=c("Non-Productive", "Productive (IHC < 99)", "Productive (IHC ≥ 99)"),
                      guide = guide_legend(reverse=TRUE)) +
  scale_y_continuous(limits=c(0,1), labels=scales::percent) +
  labs(y="Average Accuracy", x="Item Range") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
#ggsave('graphs/fig4-nnn-within-beyond-final.png', width=5, height=3)
ggsave('graphs/fig4-nnn-within-beyond-final-nojitter.png', width=5, height=3)
