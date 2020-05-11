# Reanalyses using all participants for 3.1.2 and 3.3

# SETUP ----
# source("0-clean.R") # data cleaning script, produces recursionOSF.RData
# Load cleaned data - 3 data frames
rm(list = ls())
load("data/CountingToInfinity-data.RData")

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
if (!require("robust")) {install.packages("robust"); require("robust")}   ## robust regression

# Custom global variables
# colorblind friendly red blue green
myRGBpalette <- c("#D55E00", "#0073B3", "#009E73")
# colorblind friendly green, orange, bright blue
myGOBpalette <- c("#009E73", "#E69F00", "#56B4E9")

theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0))))

## ---- 3.1.2) NEXT NUMBER ----

# ... descriptives ----
data.wcn.wide %>% group_by(Productivity) %>%
  summarise(mean=mean(score), sd=sd(score), meanperc=mean(perc), n=n())
t.test(perc ~ 
         Productivity, data = data.wcn.wide, var.equal = TRUE) %>% tidy()

# ... plot accuracy histogram ----
ggplot(data.wcn.wide, aes(x=score)) + 
  geom_histogram(aes(fill = Productivity),
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
ggsave('figures/supplemental materials/suppC-nextnumber-histogram-byproductivity.png', width=6, dpi=600)


## Accuracy by item-level covariates ----
## Note: Age should be centered and scaled across participants. 
data.wcn.wide %<>% mutate(age.c = scale(Age, center=TRUE, scale=TRUE),
                     ihc.c = scale(IHC, center=TRUE, scale=TRUE))
data.wcn.long <- left_join(data.wcn.long, dplyr::select(data.wcn.wide, LadlabID, age.c, ihc.c), by="LadlabID")
## Note: weighted effect coding allows regression estimates to be interpreted as differences from grand mean,
## even though samples are unbalanced
wec <- mean(as.numeric(data.wcn.long$Productivity)-1)
contrasts(data.wcn.long$Productivity) <- c(-wec,1-wec)
wec <- mean(as.numeric(data.wcn.long$TaskItem_type)-1)
contrasts(data.wcn.long$TaskItem_type) <- c(-wec,1-wec)

## ... a) prod*ihc +age ----
set.seed(1234)
fit_nn_log <- glmer(Accuracy ~ ihc.c * Productivity + age.c + (1|LadlabID) + (1|TaskItem_num),
                     data = data.wcn.long, family = binomial, 
                    glmerControl(optimizer = "bobyqa")) # optimizer to deal with convergence error
glance(fit_nn_log)
# LRT tests
Anova(fit_nn_log) # interaction n.s., only age sig.
# Model summary, with 95%CI and exponentiated for odds ratios
tidy(fit_nn_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# post hoc conditional slopes with 95%CI, and exponentiated ORs.
tidy(emtrends(fit_nn_log, ~Productivity, var="ihc.c")) %>% mutate_at(c(-1,-3,-4), list(EXP=exp))
## effects plots
ggsave("figures/supplemental materials/suppC-nextnumber-glmer-by-prod-ihc-age.png",
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
fit_nn2_log <- glmer(Accuracy ~ Productivity*ihc.c + TaskItem_type+ age.c+(1|LadlabID) + (1|TaskItem_num),
                     data = data.wcn.long, family = binomial , 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn2_log, fit_nn_log, test="LR") # sig.
fit_nn2_log_int <- glmer(Accuracy ~Productivity*ihc.c +  Productivity*TaskItem_type + age.c+(1|LadlabID) + (1|TaskItem_num),
                     data = data.wcn.long, family = binomial, 
                     glmerControl(optimizer = "bobyqa"))
anova(fit_nn2_log, fit_nn2_log_int, test="LR") # interaction n.s.
# both Productive Counters and Non-Productive Counters found decade transition items harder than mid-decade
# Report model
Anova(fit_nn2_log)
tidy(fit_nn2_log, conf.int = TRUE, effects="fixed") %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))
# Means for task item type
data.wcn.long %>% group_by(TaskItem_type) %>% summarise(mean(Accuracy), sd(Accuracy))
data.wcn.long %>% group_by(Productivity, TaskItem_type) %>% summarise(mean(Accuracy), sd(Accuracy))

ggsave("figures/supplemental materials/suppC-nextnumber-glmer-by-prod-itemtype-age.png",
       ggarrange(
         plot_model(fit_nn2_log, type = "int", title="NN ~ prod * item type + age.c + (1|subj)", axis.lim = c(0,1)),
         plot_model(fit_nn2_log, type="pred", terms="ihc.c [all]", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1)),
         plot_model(fit_nn2_log, type="pred", terms="Productivity", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
         plot_model(fit_nn2_log, type="pred", terms="TaskItem_type", axis.lim = c(0,1), title = "")+theme(plot.margin=margin(t=1))+rremove("ylab"),
         align="h", ncol=2, nrow=2),
       height=4, width=4)


## ---- 3.3) PREDICTORS OF INFINITY ----
model.df <- data.full %>%
  dplyr::distinct(LadlabID, Age, AgeGroup, Gender, 
                  SuccessorKnower, EndlessKnower, InfinityKnower, NonKnower,
                  IHC, Productivity, Category) %>%
  left_join(dplyr::select(data.wcn.wide, "LadlabID", wcnscore=score), by="LadlabID") %>%
  mutate(SuccessorKnower = factor(SuccessorKnower, levels = c(0,1)), 
         EndlessKnower = factor(EndlessKnower, levels = c(0,1)),
         IHC = as.integer(IHC), 
         LadlabID = factor(LadlabID)) %>%
  mutate(IHC.c = as.vector(scale(IHC, center = TRUE, scale=TRUE)),
         Age.c = as.vector(scale(Age, center = TRUE, scale=TRUE)),
         wcnscore.c = as.vector(scale(wcnscore, center = TRUE, scale=TRUE)))
# weighted effect coding for productivity
wec <- mean(as.numeric(model.df$Productivity)-1)
contrasts(model.df$Productivity) <- c(-wec, 1-wec)

## ... scatter plots ----
## Plot - Previous studies say age and initial highest count predict infinity knowledge.
## Is this true? Is knowledge found only in children with higher IHC and higher Age?
scatterFullInf <- ggplot(model.df, aes(x=Age, y=IHC)) +
  geom_point(aes(color=InfinityKnower)) +
  geom_rug(aes(color=InfinityKnower), size=0.2) +
  theme_minimal()+theme(legend.position="top")
scatterSucc <-ggplot(model.df, aes(x=Age, y=IHC)) +
  geom_point(aes(color=SuccessorKnower)) +
  geom_rug(aes(color=SuccessorKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")
scatterEnd <-ggplot(model.df, aes(x=Age, y=IHC)) +
  geom_point(aes(color=EndlessKnower)) +
  geom_rug(aes(color=EndlessKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")
scatterNoInf <-ggplot(model.df, aes(x=Age, y=IHC)) +
  geom_point(aes(color=NonKnower)) +
  geom_rug(aes(color=NonKnower), size=0.2) +   
  theme_minimal()+theme(legend.position="top")

infscatter<- ggarrange(scatterSucc, scatterEnd, scatterFullInf, scatterNoInf)
ggsave("figures/supplemental materials/suppC-infinity-scatterplots.png", infscatter, width=8, height=8)

## ... Table 1 ----
# Infinity classification
xtabs(~SuccessorKnower + EndlessKnower, model.df)
# Infinity levels according to theorized stages
model.df %>% 
  count(Productivity, Category) %>% ungroup() %>%
  bind_rows(cbind(Category='Total', count(model.df, Productivity))) %>%
  spread(key=Productivity, value=n)
# Productivity vs No knowledge
prop.table(table(model.df$Productivity, model.df$Category=="A Non-knower"), margin=1)
chisq.test(table(model.df$Productivity, model.df$Category=="A Non-knower"))
# Productivity vs Full knowledge
prop.table(table(model.df$Productivity, model.df$Category=="D Full-knower"), margin=1)
chisq.test(table(model.df$Productivity, model.df$Category=="D Full-knower"))


## ... correlate IHC & prod / nn ----
# IHC predicts productivity
Anova(glm(Productivity ~ IHC, 
    data=data.hcunique,
    family=binomial()))
# IHC still predicts productivity status (controlling for age)
Anova(glm(Productivity ~ IHC * Age, 
          data=data.hcunique,
          family=binomial()))
# IHC predicts NN
tidy(cor.test(model.df$IHC, model.df$wcnscore))

## A) Successor knowledge ----
# Exploring the distributions of Prod, IHC, Successor
# ggplot(model.df, aes(y=SuccessorKnower,x=IHC)) +
#   geom_point(alpha=0.3,size=3) +
#   facet_grid(Productivity~.) + theme_bw() + labs(title="Productivity")
# ggplot(model.df, aes(x=wcnscore)) + geom_histogram() + facet_grid(SuccessorKnower~.) +
#   labs(y='Frequency', x="Next Number accuracy")
# tab<- table(model.df$Productivity, model.df$SuccessorKnower)
# spineplot(tab,xlab="Productivity", ylab="Successor knowledge")

## A.1) Single-predictor
set.seed(1234)
# Null & Base model with age
succ.null <- glm(SuccessorKnower ~ 1, family="binomial", data=model.df)
succ.age <- glm(SuccessorKnower ~ Age.c, family = "binomial", data = model.df)
# IHC
succ.age.ihc <- glm(SuccessorKnower ~ IHC.c + Age.c, family = "binomial", data = model.df)
anova(succ.age, succ.age.ihc, test = 'LR') # IHC n.s.
# NN 
succ.age.nn <- glm(SuccessorKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df)
anova(succ.age, succ.age.nn, test = 'LRT') # NN n.s.
# Productivity
succ.age.prod <- glm(SuccessorKnower ~ Productivity + Age.c, family = "binomial", data = model.df)
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

## B) Endless knowledge ----
## B.1) Single-predictor
set.seed(1234)
# Null & Base model with age
end.null <- glm(EndlessKnower ~ 1, family="binomial", data=model.df)
end.age <- glm(EndlessKnower ~ Age.c, family = "binomial", data = model.df)
# IHC
end.age.ihc <- glm(EndlessKnower ~ IHC.c + Age.c, family = "binomial", data = model.df)
anova(end.age, end.age.ihc, test = 'LRT') # IHC sig.
# NN 
end.age.nn <- glm(EndlessKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df)
anova(end.age, end.age.nn, test = 'LRT') # NN sig.
# Productivity
end.age.prod <- glm(EndlessKnower ~ Productivity + Age.c, family = "binomial", data = model.df)
anova(end.age, end.age.prod, test="LRT") # Prod sig.

## B.2) Multiple predictors
# NN with ihc
end.age.nn.ihc <- glm(EndlessKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df)
end.age.nnXihc <- glm(EndlessKnower ~ wcnscore.c * IHC.c + Age.c, family = "binomial", data = model.df)
anova(end.age.nn, end.age.nn.ihc, end.age.nnXihc, "LR") # more complex models do not improve fit
anova(end.age.nn, end.age.nnXihc, "LR")
summary(end.age.nnXihc) # NN n.s. when controlling for ihc and age

# Productivity with ihc
end.age.prod.ihc <- glm(EndlessKnower ~ Productivity + IHC.c + Age.c, family = "binomial", data = model.df)
end.age.prodXihc <- glm(EndlessKnower ~ Productivity * IHC.c + Age.c, family = "binomial", data = model.df)
anova(end.age.prod, end.age.prod.ihc, end.age.prodXihc, test="LR") # more complex models do not improve fit
summary(end.age.prodXihc) # prod n.s.

# three way
end.age.prod.nn.ihc <- glm(EndlessKnower ~ Productivity + wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df)
Anova(end.age.prod.nn.ihc) # all n.s. -- likely overlapping variance
anova(end.age.prod, end.age.prod.nn.ihc, test="LRT") #n.s.
anova(end.age.nn, end.age.prod.nn.ihc, test="LRT") #n.s.
anova(end.age.ihc, end.age.prod.nn.ihc, test="LRT") #n.s.
anova(end.age, end.age.prod.nn.ihc, test="LRT") #sig
summary(end.age.prod.nn.ihc)

end.three.prodXihc <- glm(EndlessKnower ~ Productivity * IHC.c + wcnscore.c + Age.c, family = "binomial", data = model.df)
end.three.nnXihc <- glm(EndlessKnower ~ wcnscore.c * IHC.c + Productivity + Age.c, family = "binomial", data = model.df)
end.three.twoint <- glm(EndlessKnower ~ wcnscore.c * IHC.c + Productivity* IHC.c + Age.c, family = "binomial", data = model.df)

AIC(end.null, end.age, end.age.ihc, end.age.prod, end.age.nn,
    end.age.nn.ihc, end.age.nnXihc, end.age.prod.ihc, end.age.prodXihc,
    end.three.prodXihc, end.three.nnXihc, end.three.twoint) # best: end.age.prod, end.age.prod.ihc
# more complex models (with ihc) n.s.:
anova(end.age.prod, end.age.prod.ihc, test="LRT") # n.s.
anova(end.age.prod, end.age.prodXihc, test="LRT") # n.s.
anova(end.age.prod.ihc, end.age.prodXihc, test="LRT") # n.s.

## ... best endless model ----
Anova(end.age.prod)
tidy(end.age.prod, conf.int = TRUE) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))


## C) Full Infinity knowledge ----
## C.1) Single-predictor
set.seed(1234)
# Null & Base model with age
inf.null <- glm(InfinityKnower ~ 1, family="binomial", data=model.df)
inf.age <- glm(InfinityKnower ~ Age.c, family = "binomial", data = model.df)
anova(inf.age, inf.null, test = 'LRT') # age sig
# IHC
inf.age.ihc <- glm(InfinityKnower ~ IHC.c + Age.c, family = "binomial", data = model.df)
anova(inf.age, inf.age.ihc, test = 'LRT') # IHC n.s.
# NN 
inf.age.nn <- glm(InfinityKnower ~ wcnscore.c + Age.c, family = "binomial", data = model.df)
anova(inf.age, inf.age.nn, test = 'LRT') # NN sig
# Productivity
inf.age.prod <- glm(InfinityKnower ~ Productivity + Age.c, family = "binomial", data = model.df)
anova(inf.age, inf.age.prod, test="LRT") # Prod n.s.

## C.2) Multiple predictors
# NN with ihc
inf.age.nn.ihc <- glm(InfinityKnower ~ wcnscore.c + IHC.c + Age.c, family = "binomial", data = model.df)
inf.age.nnXihc <- glm(InfinityKnower ~ wcnscore.c * IHC.c + Age.c, family = "binomial", data = model.df)
Anova(inf.age.nnXihc) # NN n.s. when controlling for ihc and age
# Productivity with ihc
inf.age.prod.ihc <- glm(InfinityKnower ~ Productivity + IHC.c + Age.c, family = "binomial", data = model.df)
inf.age.prodXihc <- glm(InfinityKnower ~ Productivity * IHC.c + Age.c, family = "binomial", data = model.df)
Anova(inf.age.prodXihc) # Prod n.s. when controlling for ihc and age
# model comparison for nn
AIC(inf.null, inf.age, inf.age.ihc, inf.age.prod, inf.age.nn,
    inf.age.nn.ihc, inf.age.nnXihc) # best: inf.age.nn
anova(inf.null, inf.age,inf.age.nn, inf.age.nn.ihc, inf.age.nnXihc, test="LR") # more complex models n.s.
## ... best full model ----
Anova(inf.age.nn)
tidy(inf.age.nn, conf.int = TRUE) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))


# Tables 2-4 ----
library(memisc)
write.mtable(memisc::mtable('Base' = succ.age,
                            'IHC' = succ.age.ihc,
                            'NN' = succ.age.nn,
                            'Prod' =succ.age.prod,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="figures/supplemental materials/supp-C-tableS1.html")

write.mtable(memisc::mtable('Base' = end.age,
       'IHC' = end.age.ihc,
       'NN' = end.age.nn,
       'Prod' = end.age.prod,
       'Prod+IHC'=end.age.prod.ihc,
       'Prod*IHC'=end.age.prodXihc,
       'NN+IHC'=end.age.nn.ihc,
       'NN*IHC'=end.age.nnXihc,
       'Prod+NN'=end.age.nn.ihc,
       'Prod*NN'=end.age.nnXihc,
       'Prod+NN+IHC'=end.age.nn.ihc,
       summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
       format="HTML", file="figures/supplemental materials/supp-C-tableS2.html")

write.mtable(memisc::mtable('Base' = inf.age,
                            'IHC' = inf.age.ihc,
                            'NN' = inf.age.nn,
                            'Prod' =inf.age.prod,
                            'NN+IHC'=inf.age.nn.ihc,
                            'NN*IHC'=inf.age.nnXihc,
                            summary.stats = c('Nagelkerke R-sq.','Log-likelihood','AIC','N'), digits=4),
             format="HTML", file="figures/supplemental materials/supp-C-tableS3.html")
