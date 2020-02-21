# SETUP ----
# source("0-clean.R") # data cleaning script, produces recursionOSF.RData
# Load cleaned data - 3 data frames
rm(list = ls())
load("CountingToInfinity-data.RData")

# load packages ----
library(tidyverse)
library(magrittr)
library(effects)
library(car)
library(lme4)
library(emmeans)
library(ggpubr)
library(broom)
library(broom.mixed)
library(sjstats)
library(sjPlot)
require("lmPerm") ## permutation tests for linear regressions
require("lm.beta") ## standardized regression parameters
if (!require("lmtest")) {install.packages("lmtest"); require("lmtest")}          ## testing infrastructure for lm 
if (!require("sandwich")) {install.packages("sandwich"); require("sandwich")}    ## robust standard errors

# Custom global variables
# colorblind friendly red blue green
myRGBpalette <- c("#D55E00", "#0073B3", "#009E73")
# colorblind friendly green, orange, bright blue
myGOBpalette <- c("#009E73", "#E69F00", "#56B4E9")

theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0))))

## ---- 1) HIGHEST COUNT ----

## ... Descriptives ----
data.full%>%
  dplyr::distinct(subID, ProductivityStrict, Age, IHC, DCE, FHC, delta.hc, prod.gradient, suptimes.final)%>%
  group_by(ProductivityStrict) %>%
  dplyr::summarise_at(c('Age', 'IHC','DCE','FHC','delta.hc', 'prod.gradient','suptimes.final'),
                      list(~mean(., na.rm=T), 
                           ~sd(., na.rm=T),
                           ~median(., na.rm=T),
                           ~min(., na.rm=T),
                           ~max(., na.rm=T),
                           ~sum(!is.na(.)))) %>%
  gather(stat, val, -ProductivityStrict) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(ProductivityStrict, var, n=sum, mean, sd,  median, min, max)
## now by 3-way productivity
data.full%>%
  dplyr::distinct(subID, ProductivityStrict.tertiary, Age, IHC, DCE, FHC, delta.hc, prod.gradient, suptimes.final)%>%
  group_by(ProductivityStrict.tertiary) %>%
  dplyr::summarise_at(c('Age', 'IHC','DCE','FHC','delta.hc', 'prod.gradient','suptimes.final'),
                      list(~mean(., na.rm=T), 
                           ~sd(., na.rm=T),
                           ~median(., na.rm=T),
                           ~min(., na.rm=T),
                           ~max(., na.rm=T),
                           ~sum(!is.na(.)))) %>%
  gather(stat, val, -ProductivityStrict.tertiary) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(ProductivityStrict.tertiary, var, n=sum, mean, sd,  median, min, max)

# ... Prod ~ Age * IHC, ihc<99 ----
fit_prod <- glm(ProductivityStrict ~ IHC * AgeMonths, 
                data=data.hcunique[data.hcunique$IHC<99,],
                family=binomial())
glance(fit_prod)
Anova(fit_prod) # no interaction, just main effects of both Age and IHC
tidy(fit_prod, conf.int = TRUE, digits=4) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp)) # report odds ratios
# effects plots
ggsave("graphs/suppd-fig1d-prod-by-age-ihc.png",
       ggarrange(
         ggarrange(plotlist=plot_model(fit_prod, type="pred", title = ""), align="h"),
         plot_model(fit_prod, type = "int", terms="IHC [all]", title = "")+legend_style(inside=TRUE, pos="bottom right"),
         nrow = 2),
       height=4, width=4)

## ---- 2) NEXT NUMBER ----
## First create relevant dataframes for plotting and analysis
# To obtain more conservative test of hypothesis that productivity predicts NN performance, 
# we create dataframes that exclude IHC=99
nn.wide <- data.wcn.wide[data.wcn.wide$IHC<99,]
nn.long <- data.wcn.long[data.wcn.long$IHC<99,]

# ... descriptives (all) ----
data.wcn.wide %>% group_by(ProductivityStrict) %>%
  summarise(mean=mean(score), sd=sd(score), meanperc=mean(perc), n=n())
t.test(perc ~ 
         ProductivityStrict, data = data.wcn.wide, var.equal = TRUE) %>% tidy()
# remove ihc=99
data.wcn.wide %>% group_by(ProductivityStrict.tertiary) %>%
  summarise(mean=mean(score), sd=sd(score), meanperc=mean(perc), n=n())
t.test(perc ~ 
         ProductivityStrict.tertiary, data = nn.wide, var.equal = TRUE) %>% tidy()

# ... plot accuracy histogram ----
ggplot(nn.wide, aes(x=score)) + 
  geom_histogram(aes(fill = ProductivityStrict.tertiary),
                 binwidth=1, position=position_dodge(width=0.75),alpha=0.6) +
  scale_fill_manual(values=myRGBpalette, name="ProductivityStrict") +
  labs(x="Accuracy (out of 8)", 
       y="Frequency",
       title="Distribution of What Comes Next score") +
  theme_bw(base_size = 11) + 
  theme(legend.position="bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank())
ggsave('graphs/suppd-nn-hist.png', width=6, dpi=600)

## Accuracy by item-level covariates (IHC < 99) ----
## Note: Age should be centered and scaled across participants. 
nn.wide %<>% mutate(age.c = scale(Age, center=TRUE, scale=TRUE),
                     ihc.c = scale(IHC, center=TRUE, scale=TRUE))
nn.long <- left_join(nn.long, dplyr::select(nn.wide, subID, age.c, ihc.c), by="subID")
## Note: weighted effect coding allows regression estimates to be interpreted as differences from grand mean,
## even though samples are unbalanced
wec <- mean(as.numeric(nn.long$ProductivityStrict)-1)
contrasts(nn.long$ProductivityStrict) <- c(-wec,1-wec)
wec <- mean(as.numeric(nn.long$TaskItem_type)-1)
contrasts(nn.long$TaskItem_type) <- c(-wec,1-wec)

## ... a) prod*ihc +age ----
set.seed(1234)
fit_nn_log <- glmer(Accuracy ~ ihc.c * ProductivityStrict + age.c + (1|subID) + (1|TaskItem_num),
                    data = nn.long, family = binomial, 
                    glmerControl(optimizer = "bobyqa")) # optimizer to deal with convergence error
glance(fit_nn_log)
# LRT tests
Anova(fit_nn_log) # significant interaction 
# Model summary, with 95%CI and exponentiated for odds ratios
tidy(fit_nn_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# post hoc conditional slopes with 95%CI, and exponentiated ORs.
tidy(emtrends(fit_nn_log, ~ProductivityStrict, var="ihc.c")) %>% mutate_at(c(-1,-4), list(EXP=exp))
## effects plots
ggsave("graphs/suppd-fig3b-nn-by-prod-ihc.png",
       ggarrange(
         plot_model(fit_nn_log, type = "int", title="NN ~ prod * ihc.c + age.c + (1|subj)", axis.lim = c(0,1)),
         ggarrange(plot_model(fit_nn_log, type="pred", terms="ihc.c [all]", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1)),
                   plot_model(fit_nn_log, type="pred", terms="ProductivityStrict", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
                   plot_model(fit_nn_log, type="pred", terms="age.c", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
                   align="h", ncol=3),
         nrow=2, heights = c(1.5,1)),
       height=4, width=5)

## ... b) prod*mid/cross decade +age ----
nn.long %>% group_by(subID, TaskItem_type) %>% summarise(score = mean(Accuracy)) %>%
  ungroup() %>% group_by(TaskItem_type) %>% summarise(mean = mean(score), sd=sd(score))
## (are decade transitions more difficult?)
set.seed(1234)
fit_nn2_log <- glmer(Accuracy ~ ProductivityStrict*ihc.c + TaskItem_type+ age.c+(1|subID) + (1|TaskItem_num),
                     data = nn.long, family = binomial , 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn2_log, fit_nn_log, test="LR") # Task Item type sig.
fit_nn2_log_int <- glmer(Accuracy ~ProductivityStrict*ihc.c +  ProductivityStrict*TaskItem_type + age.c+(1|subID) + (1|TaskItem_num),
                     data = nn.long, family = binomial, 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn2_log, fit_nn2_log_int, test="LR") # interaction n.s.
# both Productive Counters and Non-Productive Counters found decade transition items harder than mid-decade
# Report model
tidy(fit_nn2_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
ggsave("graphs/suppd-fig3c-nn-by-prod-middecade.png",
       ggarrange(
         plot_model(fit_nn2_log, type = "int", title="NN ~ prod * item type + age.c + (1|subj)", axis.lim = c(0,1)),
         plot_model(fit_nn2_log, type="pred", terms="ihc.c [all]", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1)),
         plot_model(fit_nn2_log, type="pred", terms="ProductivityStrict", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
         plot_model(fit_nn2_log, type="pred", terms="TaskItem_type", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
         align="h", ncol=2, nrow=2),
       height=4, width=4)

## ... c) within vs beyond ----
# Hypothesis: productive counters equally good when items outside counting range. Non-productive counters should be bad.
set.seed(1234)
# do weighted effect coding
nn.long <- nn.long %>% mutate(WithinOutsideIHC = factor(WithinOutsideIHC, levels=c("within", "outside")))
wec <- mean(as.numeric(nn.long$WithinOutsideIHC)-1)
contrasts(nn.long$WithinOutsideIHC) <- c(-wec,1-wec)
# construct models
fit_nn3_log <- glmer(Accuracy ~ ProductivityStrict*WithinOutsideIHC + ihc.c + (1|subID) + (1|TaskItem_num),
                     data = nn.long , family = binomial, glmerControl(optimizer = "bobyqa"))
# interaction sig.
Anova(fit_nn3_log)
tidy(fit_nn3_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# estimated marginal means
emmeans(fit_nn3_log,"ProductivityStrict", by="WithinOutsideIHC", type="response")

## t-tests for contrasts
# within n.s.
nn.long %>% 
  group_by(subID, ProductivityStrict, WithinOutsideIHC) %>%
  summarise(score=mean(Accuracy, na.rm=T)) %>%
  filter(WithinOutsideIHC=="within") %>%
  t.test(score ~ 
           ProductivityStrict, data = ., var.equal = TRUE) %>% tidy()
# outside sig.
nn.long %>% 
  group_by(subID, ProductivityStrict, WithinOutsideIHC) %>%
  summarise(score=mean(Accuracy, na.rm=T)) %>%
  filter(WithinOutsideIHC=="outside") %>% 
  t.test(score ~ 
           ProductivityStrict, data = ., var.equal = TRUE) %>% tidy()

## ---- 3) PREDICTORS OF INFINITY ----
model.df <- data.full %>%
  dplyr::distinct(subID, Age, AgeGroup, Gender, 
                  SuccessorKnower, EndlessKnower, InfinityKnower, NonKnower,
                  IHC, ProductivityStrict, ProductivityStrict.tertiary, prod.gradient, Category) %>%
  left_join(dplyr::select(nn.wide, "subID", wcnscore=score), by="subID") %>%
  mutate(SuccessorKnower = factor(SuccessorKnower, levels = c(0,1)), 
         EndlessKnower = factor(EndlessKnower, levels = c(0,1)),
         IHC = as.integer(IHC), 
         subID = factor(subID))
# scale and center
model.df2 <- model.df %>%
  filter(ProductivityStrict.tertiary != "Productive (IHC \u2265 99)") %>%
  mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale=TRUE)),
         Age.c = as.vector(scale(Age, center = TRUE, scale=TRUE)),
         prod.gradient.c = as.vector(scale(prod.gradient, center=TRUE, scale=TRUE)),
         wcnscore.c = as.vector(scale(wcnscore, center = TRUE, scale=TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(model.df2$ProductivityStrict)-1)
contrasts(model.df2$ProductivityStrict) <- c(-wec, 1-wec)

## Table 1 ----
# Infinity classification
xtabs(~SuccessorKnower + EndlessKnower, model.df)
# Infinity levels according to theorized stages
model.df %>% 
  count(ProductivityStrict.tertiary, Category) %>% ungroup() %>%
  bind_rows(cbind(Category='Total', count(model.df, ProductivityStrict.tertiary))) %>%
  spread(key=ProductivityStrict.tertiary, value=n)

## ... correlate IHC & prod / nn ----
# IHC predicts productivity
Anova(glm(ProductivityStrict ~ IHC, 
    data=data.hcunique[data.hcunique$IHC<99,],
    family=binomial()))
# IHC predicts productivity status (controlling for age)
Anova(fit_prod)
# IHC predicts gradient measure of productivity
tidy(cor.test(model.df2$IHC, model.df2$prod.gradient))
# IHC predicts NN
tidy(cor.test(model.df2$IHC, model.df2$wcnscore))

## A) Successor knowledge ----

## A.1) Single-predictor
set.seed(1234)
# Null & Base model with age
succ.null <- glm(SuccessorKnower ~ 1, family="binomial", data=model.df2)
succ.age <- glm(SuccessorKnower ~ Age.c, family = "binomial", data = model.df2)
# IHC
succ.age.ihc <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.ihc, test = 'LRT') # IHC n.s.
# NN 
succ.age.nn <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.nn, test = 'LRT') # NN n.s.
# ProductivityStrict
succ.age.prod <- glm(SuccessorKnower ~ ProductivityStrict + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.prod, test="LRT") # Prod n.s.


## B) Endless knowledge ----
## B.1) Single-predictor
set.seed(1234)
# Null & Base model with age
end.null <- glm(EndlessKnower ~ 1, family="binomial", data=model.df2)
end.age <- glm(EndlessKnower ~ Age.c, family = "binomial", data = model.df2)
# IHC
end.age.ihc <- glm(EndlessKnower ~ IHC.c + Age.c, family = "binomial", data = model.df2)
anova(end.age, end.age.ihc, test = 'LRT') # IHC n.s.
# NN 
end.age.nn <- glm(EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df2)
anova(end.age, end.age.nn, test = 'LRT') # NN n.s.
# ProductivityStrict
end.age.prod <- glm(EndlessKnower ~ ProductivityStrict + Age.c, family = "binomial", data = model.df2)
anova(end.age, end.age.prod, test="LRT") # Prod n.s.



## C) Full Infinity knowledge ----
## C.1) Single-predictor
set.seed(1234)
# Null & Base model with age
inf.null <- glm(InfinityKnower ~ 1, family="binomial", data=model.df2)
inf.age <- glm(InfinityKnower ~ Age.c, family = "binomial", data = model.df2)
anova(inf.age, inf.null, test = 'LRT') # age n.s.
# IHC
inf.age.ihc <- glm(InfinityKnower ~ IHC.c + Age.c, family = "binomial", data = model.df2)
anova(inf.age, inf.age.ihc, test = 'LRT') # IHC n.s.
# NN 
inf.age.nn <- glm(InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df2)
anova(inf.age, inf.age.nn, test = 'LRT') # NN n.s.
# ProductivityStrict
inf.age.prod <- glm(InfinityKnower ~ ProductivityStrict + Age.c, family = "binomial", data = model.df2)
anova(inf.age, inf.age.prod, test="LRT") # Prod significant.

# ## C.2) Multiple predictors
# ProductivityStrict with ihc
inf.age.prod.ihc <- glm(InfinityKnower ~ ProductivityStrict + IHC.c + Age.c, family = "binomial", data = model.df2)
inf.age.prodXihc <- glm(InfinityKnower ~ ProductivityStrict * IHC.c + Age.c, family = "binomial", data = model.df2)
Anova(inf.age.prodXihc) # prod significant, no interaction

AIC(inf.null, inf.age, inf.age.ihc, inf.age.prod, inf.age.nn,
    inf.age.prod.ihc, inf.age.prodXihc) # best: inf.age.prod 
# more complex models (with ihc) n.s.:
anova(inf.null, inf.age, inf.age.prod, inf.age.prod.ihc, inf.age.prodXihc, test="LR") # more complex models n.s.

## ... best infinity model ----
glance(inf.age.prod)
tidy(inf.age.prod, conf.int = TRUE, digits=4) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))


## tables ----
library(memisc)
write.mtable(memisc::mtable('Base' = succ.age,
                            'IHC' = succ.age.ihc,
                            'NN' = succ.age.nn,
                            'Prod.strict' =succ.age.prod,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="tables/supp-D-table2.html")

write.mtable(memisc::mtable('Base' = end.age,
                            'IHC' = end.age.ihc,
                            'NN' = end.age.nn,
                            'Prod.strict' = end.age.prod,
#                            'Prod+IHC'=end.age.prod.ihc,
#                            'Prod*IHC'=end.age.prodXihc,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="tables/supp-D-table3.html")

write.mtable(memisc::mtable('Base' = inf.age,
                            'IHC' = inf.age.ihc,
                            'NN' = inf.age.nn,
                            'Prod.strict' =inf.age.prod,
                            'Prod.strict+IHC'=inf.age.prod.ihc,
                            'Prod.strict*IHC'=inf.age.prodXihc,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="tables/supp-D-table4.html")
