rm(list = ls())
# Load cleaned data
load("data/CountingToInfinity-data.RData")

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
# --- Fig 2 ----
# PLOT: Histogram of IHC, by productivity
## three-way productivity
fig2 <- ggplot(data.hcunique, aes(x=IHC)) + 
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
ggsave('figures/manuscript/fig2.eps',fig2,dpi=600,
       width=6, height=3)

# --- Extra plot: Productivity by Age
fig2b <- ggplot(data.hcunique, aes(x=AgeMonths)) + 
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
ggsave('figures/extra/productivity-by-age.png', fig2b,
       width=6, height=3, dpi=600)

# --- Extra plot: IHC by Age
fig2c <- data.hcunique %>% ggplot(aes(x=AgeMonths, y=IHC, color=Productivity.tertiary, fill=Productivity.tertiary)) +
  geom_smooth(method="lm") +
  geom_point(alpha=0.5, position=position_dodge2(width=.3)) +
  scale_color_manual(values=myRGBpalette, name="Productivity Classification",
                     guide = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=myRGBpalette, name="Productivity Classification",
                    guide = guide_legend(reverse=TRUE)) +
  labs(x="Age in Months", y="Initial Highest Count") +
  scale_x_continuous(breaks=seq(48,73,by=6)) +
  theme(legend.position="right")
ggsave('figures/extra/ihc-by-age-productivity.png', fig2c, width=6, height=3, dpi=600)

## --- Fig 3----
## First let's plot productive counters
fig3a <- data.hcerrors %>% filter(productivity=="prod") %>%
  ggplot(aes(x=subID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc, color="Accurate Sequence", linetype="Accurate Sequence"), size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5, color="Skipped Sequence", linetype="Skipped Sequence"), size=1) +
  geom_point(aes(y=error.start, fill="Error - uncorrected", shape="Error - uncorrected"), size=2, stroke = 0) +
  geom_point(aes(y=decadeprompt, fill="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3, stroke = 0) +
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
fig3b <- data.hcerrors %>% filter(productivity=="nonprod") %>%
  ggplot(aes(x=subID)) +
  geom_linerange(aes(ymin=ihc, ymax=fhc, color="Accurate Sequence", linetype="Accurate Sequence"), size=.5) +
  geom_linerange(aes(ymin=error.start-.5, ymax=error.end+.5, color="Skipped Sequence", linetype="Skipped Sequence"), size=1) +
  geom_point(aes(y=error.start, fill="Error - uncorrected", shape="Error - uncorrected"), size=2, stroke = 0) +
  geom_point(aes(y=decadeprompt, fill="Error - got Decade Prompt", shape="Error - got Decade Prompt"), size=3, stroke = 0) +
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
ggsave('figures/manuscript/fig3.eps', ggarrange(fig3a, fig3b, nrow=2), width=7, height=5)

## Fig 4----
# NN accuracy by item & prod
fig4.data <- data.wcn %>%
  filter(TaskType == "immediate")%>%
  mutate(TaskItem_type= ifelse(mod(TaskItem_num,10)==9, "Item Type: Decade transition", "Item Type: Mid-decade")) %>%
  mutate(TaskItem_type_ordered = ordered(TaskItem_type, levels=c("Item Type: Mid-decade", "Item Type: Decade transition")))
fig4.data %>%
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
ggsave('figures/manuscript/fig4.eps',dpi=600,
       width=6, height=3)

## ---- Fig 5 ----
nn.long <- fig4.data %>%
  dplyr::select(c(LadlabID, IHC, FHC, Age, Productivity, Productivity.tertiary,
                  TaskItem_num, TaskItem_type, Accuracy, WithinOutsideIHC)) %>%
  mutate(IHC = as.integer(IHC), subID=as.factor(LadlabID), TaskItem_type=as.factor(TaskItem_type))
nn.long %>%
  mutate(WithinOutsideIHC = factor(WithinOutsideIHC, levels = c("within", "outside"), 
                                   labels = c("Within IHC", "Beyond IHC")))%>%
  dplyr::group_by(Productivity.tertiary, WithinOutsideIHC, LadlabID) %>%
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
# ggsave('graphs/fig4-nnn-within-beyond-final-nojitter.png', width=5, height=3)
ggsave('figures/manuscript/fig5.eps', width=5, height=3)
