## This analysis is organized into 3 sets of research questions about counting ability and beliefs about infinity. 
## The primary measures are:
## - Initial Highest Count (1-99), also Final Highest Count (1-99) after decade prompts
## - Productivity (yes/no)
## - Next Number Accuracy (score out of 8 items), also interested in item-level effects
## - Successor knowledge (yes/no)
## - Endless knowledge (yes/no)
## - Full Infinity knowledge (yes/no) -- this is any child who has both Successor & Endless knowledge
## GLM regressions control for subject age (range 4.0 - 5.9)
## GLMMs additionally include random intercepts for subject and relevant trial-level variables

## Scientific background / hypotheses:
## Previous work finds that children who can count higher are more likely to endorse beliefs about infinity (i.e. that you can always add one, & that numbers go on forever)
## We hypothesize that this relationship is moderated by an insight into the structure of the count list: that it is a recursive list that can keep generating new numbers based on morphosyntactic rules
## 1) We first seek evidence for individual differences in acquisition of a productive decade+unit rule for generating successive numbers.
## 2) Then we validate the Productivity classifications on a separate task (Next Number), which asks for successors on arbitary numbers outside the counting up context
## 3) Finally we test if the two measures of productive knowledge predicts infinity knowledge above and beyond rote counting familiarity (IHC).

# SETUP ----
source("0-clean.R") # data cleaning script, produces recursionOSF.RData
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
library(tidylog)
require("lmPerm") ## permutation tests for linear regressions
require("lm.beta") ## standardized regression parameters
if (!require("lmtest")) {install.packages("lmtest"); require("lmtest")}          ## testing infrastructure for lm 
if (!require("sandwich")) {install.packages("sandwich"); require("sandwich")}    ## robust standard errors
if (!require("robust")) {install.packages("robust"); require("robust")}   ## robust regression

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
## Binary productivity
data.full%>%
  dplyr::distinct(subID, Productivity, Age, IHC, DCE, FHC, delta.hc, prod.gradient, suptimes.final)%>%
  group_by(Productivity) %>%
  dplyr::summarise_at(c('Age','IHC','DCE','FHC','delta.hc', 'prod.gradient','suptimes.final'),
                      list(~mean(., na.rm=T), 
                           ~sd(., na.rm=T),
                           ~median(., na.rm=T),
                           ~min(., na.rm=T),
                           ~max(., na.rm=T),
                           ~sum(!is.na(.)))) %>%
  gather(stat, val, -Productivity) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(Productivity, var, n=sum, mean, sd,  median, min, max)

## now by 3-way productivity
data.full%>%
  dplyr::distinct(subID, Productivity.tertiary, Age, IHC, DCE, FHC, delta.hc, prod.gradient, suptimes.final)%>%
  group_by(Productivity.tertiary) %>%
  dplyr::summarise_at(c('Age', 'IHC','DCE','FHC','delta.hc', 'prod.gradient','suptimes.final'),
                      list(~mean(., na.rm=T), 
                           ~sd(., na.rm=T),
                           ~median(., na.rm=T),
                           ~min(., na.rm=T),
                           ~max(., na.rm=T),
                           ~sum(!is.na(.)))) %>%
  gather(stat, val, -Productivity.tertiary) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(Productivity.tertiary, var, n=sum, mean, sd,  median, min, max)

# ... IHC ~ Age ----
## ihc and age are correlated
tidy(cor.test(data.hcunique$IHC, data.hcunique$AgeMonths))

## regression shows significant beta
fit_ihc <- lm(IHC~ scale(AgeMonths, scale = F), data=data.hcunique)
summary(fit_ihc)
coeftest(fit_ihc, vcov = vcovHC(fit_ihc))  # similar; sandwich estimator helps correct for se when outliers

# ... IHC ~ Age, ihc<99 ----
fit_ihc2 <- lm(IHC ~ scale(AgeMonths, scale=F), data=data.hcunique[data.hcunique$IHC<99,])
summary(fit_ihc2)
coeftest(fit_ihc2, vcov = vcovHC(fit_ihc2))   # similar; sandwich estimator helps correct for se when outliers
## conclude: AGE predicts IHC, R^2 = 0.318

# ... Prod ~ Age * IHC, ihc<99 ----
fit_prod <- glm(Productivity ~ IHC * AgeMonths, 
                data=data.hcunique[data.hcunique$IHC<99,],
                family=binomial())
glance(fit_prod)
Anova(fit_prod) # interaction n.s, sig. main effects of Age and IHC
tidy(fit_prod, conf.int=T) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# effects plots
ggsave('graphs/fig1d-prod-by-age-ihc.png', ggarrange(
  ggarrange(plotlist=plot_model(fit_prod, type="pred", title = ""), align="h"),
  plot_model(fit_prod, type = "int", terms="IHC [all]", title = "interaction n.s.")+
    legend_style(inside=TRUE, pos="bottom right"),
  nrow = 2), width=4, height=4.5)

## ---- 2) NEXT NUMBER ----
## First create relevant dataframes for plotting and analysis
# To obtain more conservative test of hypothesis that productivity predicts NN performance, 
# we create dataframes that exclude IHC=99
nn.wide <- data.wcn.wide[data.wcn.wide$IHC<99,]
nn.long <- data.wcn.long[data.wcn.long$IHC<99,]

# ... descriptives ----
data.wcn.wide %>% group_by(Productivity) %>%
  summarise(mean=mean(score), sd=sd(score), meanperc=mean(perc), n=n())
t.test(perc ~ 
         Productivity, data = data.wcn.wide, var.equal = TRUE) %>% tidy()
# Productivity tertiary
data.wcn.wide %>% group_by(Productivity.tertiary) %>%
  summarise(mean=mean(score), sd=sd(score), meanperc=mean(perc), n=n())
t.test(perc ~ 
         Productivity, data = nn.wide, var.equal = TRUE) %>% tidy()

# ... plot accuracy histogram ----
ggplot(nn.wide, aes(x=score)) + 
  geom_histogram(aes(fill = Productivity.tertiary),
                 binwidth=1, position=position_dodge(width=0.75),alpha=0.6) +
  scale_fill_manual(values=myRGBpalette, name="Productivity") +
  scale_y_continuous(limits=c(0,15))+
  labs(x="Accuracy (out of 8)", 
       y="Frequency",
       title="Next Number task") +
  theme_bw(base_size = 11) + 
  theme(legend.position="bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank())
ggsave('graphs/nn-hist.png', width=6, dpi=600)


## Accuracy by item-level covariates ----
## Note: Age should be centered and scaled across participants. 
nn.wide %<>% mutate(age.c = scale(Age, center=TRUE, scale=TRUE),
                     ihc.c = scale(IHC, center=TRUE, scale=TRUE))
nn.long <- left_join(nn.long, dplyr::select(nn.wide, LadlabID, age.c, ihc.c), by="LadlabID")
## Note: weighted effect coding allows regression estimates to be interpreted as differences from grand mean,
## even though samples are unbalanced
wec <- mean(as.numeric(nn.long$Productivity)-1)
contrasts(nn.long$Productivity) <- c(-wec,1-wec)
wec <- mean(as.numeric(nn.long$TaskItem_type)-1)
contrasts(nn.long$TaskItem_type) <- c(-wec,1-wec)

## ... a) prod*ihc +age ----
set.seed(1234)
fit_nn_log <- glmer(Accuracy ~ ihc.c * Productivity + age.c + (1|LadlabID) + (1|TaskItem_num),
                     data = nn.long, family = binomial, 
                    glmerControl(optimizer = "bobyqa")) # optimizer to deal with convergence error
glance(fit_nn_log)
# LRT tests
Anova(fit_nn_log) # significant interaction 
# Model summary, with 95%CI and exponentiated for odds ratios
tidy(fit_nn_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# post hoc conditional slopes with 95%CI, and exponentiated ORs.
tidy(emtrends(fit_nn_log, ~Productivity, var="ihc.c")) %>% mutate_at(c(-1,-3,-4), list(EXP=exp))
## effects plots
ggsave("graphs/fig3b-nn-by-prod-ihc.png",
       ggarrange(
         plot_model(fit_nn_log, type = "int", title="NN ~ prod * ihc.c + age.c + (1|subj)", axis.lim = c(0,1)),
         ggarrange(plot_model(fit_nn_log, type="pred", terms="ihc.c [all]", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1)),
                   plot_model(fit_nn_log, type="pred", terms="Productivity", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
                   plot_model(fit_nn_log, type="pred", terms="age.c", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
                   align="h", ncol=3),
         nrow=2, heights = c(1.5,1)),
       height=4, width=5)

## ... b) prod*mid/cross decade +age ----
## (are decade transitions more difficult?)
set.seed(1234)
fit_nn2_log <- glmer(Accuracy ~ TaskItem_type + ihc.c*Productivity + age.c+(1|LadlabID) + (1|TaskItem_num),
                     data = nn.long, family = binomial , 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn_log, fit_nn2_log, test="LR")

fit_nn2_log_int <- glmer(Accuracy ~Productivity*ihc.c +  Productivity*TaskItem_type + age.c+(1|LadlabID) + (1|TaskItem_num),
                     data = nn.long, family = binomial, 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn2_log, fit_nn2_log_int, test="LR") # interaction n.s.
# both Productive Counters and Non-Productive Counters found decade transition items harder than mid-decade
# Report model
Anova(fit_nn2_log)
tidy(fit_nn2_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# Means for task item type
nn.long %>% group_by(TaskItem_type) %>% summarise(mean(Accuracy), sd(Accuracy))
nn.long %>% group_by(Productivity, TaskItem_type) %>% summarise(mean(Accuracy), sd(Accuracy))

ggsave("graphs/fig3c-nn-by-prod-middecade.png",
       ggarrange(
         plot_model(fit_nn2_log, type = "int", title="NN ~ prod * item type + age.c + (1|subj)", axis.lim = c(0,1)),
         plot_model(fit_nn2_log, type="pred", terms="ihc.c [all]", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1)),
         plot_model(fit_nn2_log, type="pred", terms="Productivity", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
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
fit_nn3_log <- glmer(Accuracy ~ Productivity*WithinOutsideIHC + ihc.c + age.c + (1|LadlabID) + (1|TaskItem_num),
                     data = nn.long , family = binomial, glmerControl(optimizer = "bobyqa"))
# interaction sig.
Anova(fit_nn3_log)
tidy(fit_nn3_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# estimated marginal means
emmeans(fit_nn3_log,"Productivity", by="WithinOutsideIHC", type="response")

## t-tests for contrasts
nn.long %>% 
  group_by(LadlabID, Productivity, WithinOutsideIHC) %>%
  summarise(score=mean(Accuracy, na.rm=T)) %>%
  filter(WithinOutsideIHC=="within") %>%
  t.test(score ~ 
           Productivity, data = ., var.equal = TRUE) %>% tidy()
# outside
nn.long %>% 
  group_by(LadlabID, Productivity, WithinOutsideIHC) %>%
  summarise(score=mean(Accuracy, na.rm=T)) %>%
  filter(WithinOutsideIHC=="outside") %>%
  t.test(score ~ 
           Productivity, data = ., var.equal = TRUE) %>% tidy()


## ---- 3) PREDICTORS OF INFINITY ----
model.df <- data.full %>%
  dplyr::distinct(subID, Age, AgeGroup, Gender, 
                  SuccessorKnower, EndlessKnower, InfinityKnower, NonKnower,
                  IHC, Productivity, Productivity.tertiary, prod.gradient, Category) %>%
  left_join(select(nn.wide, "subID", wcnscore=score), by="subID") %>%
  mutate(SuccessorKnower = factor(SuccessorKnower, levels = c(0,1)), 
         EndlessKnower = factor(EndlessKnower, levels = c(0,1)),
         IHC = as.integer(IHC), 
         subID = factor(subID))
# scale and center
model.df2 <- model.df %>%
  filter(Productivity.tertiary != "Productive (IHC \u2265 99)") %>%
  mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale=TRUE)),
         Age.c = as.vector(scale(Age, center = TRUE, scale=TRUE)),
         prod.gradient.c = as.vector(scale(prod.gradient, center=TRUE, scale=TRUE)),
         wcnscore.c = as.vector(scale(wcnscore, center = TRUE, scale=TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(model.df2$Productivity)-1)
contrasts(model.df2$Productivity) <- c(-wec, 1-wec)

## ... scatter plots ----
## Plot - Previous studies say age and initial highest count predict infinity knowledge.
## Is this true? Is knowledge found only in children with higher IHC and higher Age?
scatterFullInf <- ggplot(model.df2, aes(x=Age, y=IHC)) +
  geom_point(aes(color=InfinityKnower)) +
  geom_rug(aes(color=InfinityKnower), size=0.2) +
  theme_minimal()+theme(legend.position="top")
scatterSucc <-ggplot(model.df2, aes(x=Age, y=IHC)) +
  geom_point(aes(color=SuccessorKnower)) +
  geom_rug(aes(color=SuccessorKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")
scatterEnd <-ggplot(model.df2, aes(x=Age, y=IHC)) +
  geom_point(aes(color=EndlessKnower)) +
  geom_rug(aes(color=EndlessKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")
scatterNoInf <-ggplot(model.df2, aes(x=Age, y=IHC)) +
  geom_point(aes(color=NonKnower)) +
  geom_rug(aes(color=NonKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")

infscatter<- ggarrange(scatterSucc, scatterEnd, scatterFullInf, scatterNoInf)
ggsave("graphs/infscatter.png", infscatter, width=8, height=8)

## ... Table 1 ----
# Infinity classification
xtabs(~SuccessorKnower + EndlessKnower, model.df)
# Infinity levels according to theorized stages
model.df %>% 
  count(Productivity.tertiary, Category) %>% ungroup() %>%
  bind_rows(cbind(Category='Total', count(model.df, Productivity.tertiary))) %>%
  spread(key=Productivity.tertiary, value=n)
# Productivity vs No knowledge
prop.table(table(model.df$Productivity, model.df$Category=="A Non-knower"), margin=1)
chisq.test(table(model.df$Productivity, model.df$Category=="A Non-knower"))
# Productivity vs Full knowledge
prop.table(table(model.df$Productivity, model.df$Category=="D Full-knower"), margin=1)
chisq.test(table(model.df$Productivity, model.df$Category=="D Full-knower"))


## ... correlate IHC & prod / nn ----
# IHC predicts productivity
Anova(glm(Productivity ~ IHC, 
    data=data.hcunique[data.hcunique$IHC<99,],
    family=binomial()))
# IHC predicts productivity status (controlling for age)
Anova(fit_prod)
# IHC predicts gradient measure of productivity
tidy(cor.test(model.df2$IHC, model.df2$prod.gradient))
# IHC predicts NN
tidy(cor.test(model.df2$IHC, model.df2$wcnscore))

## A) Successor knowledge ----
# Exploring the distributions of Prod, IHC, Successor
# ggplot(model.df2, aes(y=SuccessorKnower,x=IHC)) + 
#   geom_point(alpha=0.3,size=3) + 
#   facet_grid(Productivity.tertiary~.) + theme_bw() + labs(title="Productivity")
# ggplot(model.df, aes(x=wcnscore)) + geom_histogram() + facet_grid(SuccessorKnower~.) +
#   labs(y='Frequency', x="Next Number accuracy")
# tab<- table(model.df$Productivity.tertiary, model.df$SuccessorKnower)
# spineplot(tab,xlab="Productivity", ylab="Successor knowledge")

## A.1) Single-predictor
set.seed(1234)
# Null & Base model with age
succ.null <- glm(SuccessorKnower ~ 1, family="binomial", data=model.df2)
succ.age <- glm(SuccessorKnower ~ Age.c, family = "binomial", data = model.df2)
# IHC
succ.age.ihc <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.ihc, test = 'LR') # IHC n.s.
# NN 
succ.age.nn <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.nn, test = 'LRT') # NN n.s.
# Productivity
succ.age.prod <- glm(SuccessorKnower ~ Productivity + Age.c, family = "binomial", data = model.df2)
anova(succ.age, succ.age.prod, test="LRT") # Prod n.s.
# # Effects plots
# plot_models(model.nn.successor, model.prod.successor, model.ihc.successor,
#             transform="plogis", show.values = T, show.p=T, grid=T,
#             colors="bw", show.intercept = T, spacing=0.2,
#             m.labels = c("Model 1: NN",
#                          "Model 2: Productivity",
#                          "Model 3: IHC",
#                          "Model 4: Prod. Grad"),
#             show.legend=F,
#             title="Individual predictors of Successor Knowledge, controlling for age (IHC<99)",
#             axis.labels = c("Age.c"="Age", "wcnscore.c"="Next Number accuracy",
#                             "IHC.c"= "Initial Highest Count",
#                             "Productivity1"="Productivity Status"),
#             axis.title = "Endorsement Probability",
#             axis.lim=c(0,1)) +
#   theme_bw() + ggplot2::geom_hline(yintercept = 0.5, linetype="dashed")
# ggsave("graphs/successor-regression1.png", width=6,height=4)

# ## A.2) Combined predictors
# # NN with ihc
# succ.age.nn.ihc <- glm(SuccessorKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df2)
# succ.age.nnXihc <- glm(SuccessorKnower ~ wcnscore.c * IHC.c + Age.c, family = "binomial", data = model.df2)
# Anova(succ.age.nnXihc) # NN n.s. when controlling for ihc and age
# # Productivity with ihc
# succ.age.prod.ihc <- glm(SuccessorKnower ~ Productivity + IHC.c + Age.c, family = "binomial", data = model.df2)
# succ.age.prodXihc <- glm(SuccessorKnower ~ Productivity * IHC.c + Age.c, family = "binomial", data = model.df2)
# Anova(succ.age.prodXihc) # Prod significant when controlling for ihc and age
# # Productivity with NN and IHC
# succ.age.ihc.prod.nn <- glm(SuccessorKnower ~ Productivity * IHC.c + wcnscore.c+ Age.c, family = "binomial",data = model.df2)
# Anova(succ.age.ihc.prod.nn) # Prod significant when controlling for ihc, age, wcn
# succ.full <- glm(SuccessorKnower ~ IHC.c * Productivity * wcnscore.c + Age.c, family = "binomial",data = model.df2)
# Anova(succ.full)
# plot_model(succ.full)

# AIC(succ.null, succ.age, succ.age.ihc, succ.age.prod, succ.age.nn,
#     succ.age.nn.ihc, succ.age.nnXihc, succ.age.prod.ihc, succ.age.prodXihc,
#     succ.age.ihc.prod.nn, succ.full) # best: succ.age.prod.ihc

## ... best successor model
# Anova(succ.age.prod.ihc)
# glance(succ.age.prod.ihc)
# tidy(succ.age.prod.ihc, conf.int = TRUE) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# # kind of weird that IHC is negative! That showed up in original plots too. Hmm.
# 
# # outliers? 
# sort(hatvalues(model.prodihc.successor))          ## most influential 3 obs: subj 8, 89, 82
# outlierTest(model.prodihc.successor)   ## prints out the worst observation only, subj84 (Bonferroni p not significant)
# influenceIndexPlot(model.prodihc.successor)# subj84 again
# influencePlot(model.prodihc.successor, ylim = c(-2.5, 4))  ## subj 84!!
# 
# # Robust regression
# rmodel.prodihc.successor <- glmRob(SuccessorKnower ~ IHC.c * Productivity + Age.c, family = "binomial",data = model.df2)
# summary(rmodel.prodihc.successor) # similar results
# ## compare coefficients with non-robust fit
# coef(rmodel.prodihc.successor)
# coef(model.prodihc.successor)
# # Effects plot
# plot_model(rmodel.prodihc.successor)


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
# Productivity
end.age.prod <- glm(EndlessKnower ~ Productivity + Age.c, family = "binomial", data = model.df2)
anova(end.age, end.age.prod, test="LRT") # Prod sig.

## B.2) Multiple predictors
# NN with ihc
end.age.nn.ihc <- glm(EndlessKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df2)
end.age.nnXihc <- glm(EndlessKnower ~ wcnscore.c * IHC.c + Age.c, family = "binomial", data = model.df2)
Anova(end.age.nnXihc) # NN n.s. when controlling for ihc and age
# Productivity with ihc
end.age.prod.ihc <- glm(EndlessKnower ~ Productivity + IHC.c + Age.c, family = "binomial", data = model.df2)
end.age.prodXihc <- glm(EndlessKnower ~ Productivity * IHC.c + Age.c, family = "binomial", data = model.df2)
Anova(end.age.prodXihc) # Prod significant when controlling for ihc and age

AIC(end.null, end.age, end.age.ihc, end.age.prod, end.age.nn,
    end.age.nn.ihc, end.age.nnXihc, end.age.prod.ihc, end.age.prodXihc) # best: end.age.prod
# more complex models (with ihc) n.s.:
anova(end.age.prod, end.age.prod.ihc, test="LR") 
anova(end.age.prod, end.age.prodXihc, test="LR") 
anova(end.age.prod.ihc, end.age.prodXihc, test="LR") 

## ... best endless model ----
Anova(end.age.prod)
summary(end.age.prod)

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
# Productivity
inf.age.prod <- glm(InfinityKnower ~ Productivity + Age.c, family = "binomial", data = model.df2)
anova(inf.age, inf.age.prod, test="LRT") # Prod n.s.

# ## C.2) Multiple predictors
# # NN with ihc
# inf.age.nn.ihc <- glm(InfinityKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df2)
# inf.age.nnXihc <- glm(InfinityKnower ~ wcnscore.c * IHC.c + Age.c, family = "binomial", data = model.df2)
# Anova(inf.age.nnXihc) # NN n.s. when controlling for ihc and age
# # Productivity with ihc
# inf.age.prod.ihc <- glm(InfinityKnower ~ Productivity + IHC.c + Age.c, family = "binomial", data = model.df2)
# inf.age.prodXihc <- glm(InfinityKnower ~ Productivity * IHC.c + Age.c, family = "binomial", data = model.df2)
# Anova(inf.age.prodXihc) # Prod n.s. when controlling for ihc and age
# 
# AIC(inf.null, inf.age, inf.age.ihc, inf.age.prod, inf.age.nn,
#     inf.age.nn.ihc, inf.age.nnXihc, inf.age.prod.ihc, inf.age.prodXihc,
#     inf.age.ihc.prod.nn, inf.full) # best: inf.age.prod
# anova(inf.null, inf.age,inf.age.prod, inf.age.prod.ihc, inf.age.prodXihc, test="LR") # more complex models n.s.


# Tables 2-4 ----
library(memisc)
write.mtable(memisc::mtable('Base' = succ.age,
                            'IHC' = succ.age.ihc,
                            'NN' = succ.age.nn,
                            'Prod' =succ.age.prod,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="tables/main-table2.html")

write.mtable(memisc::mtable('Base' = end.age,
       'IHC' = end.age.ihc,
       'NN' = end.age.nn,
       'Prod' = end.age.prod,
       'Prod+IHC'=end.age.prod.ihc,
       'Prod*IHC'=end.age.prodXihc,
       summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
       format="HTML", file="tables/main-table3.html")

write.mtable(memisc::mtable('Base' = inf.age,
                            'IHC' = inf.age.ihc,
                            'NN' = inf.age.nn,
                            'Prod' =inf.age.prod,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="tables/main-table4.html")
