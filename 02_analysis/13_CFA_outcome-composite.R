library(dplyr)
library(ggplot2)
library(magrittr)
library(ggplot2)
library(PerformanceAnalytics)
library(psych)
library(corrplot)
library(nFactors)
library(kableExtra)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(gt)
library(lavaan)

# Data Frame
source("./02_analysis/02_static_responses.R")
# colnames of the specific question groups
source("./02_analysis/99_question_groups.R")

##data.questions$outcome.composite <- outcome.df$outcome.composite

model <- "OC =~ impactTargetGroup.pub.+impactTargetGroup.busi.+impactTargetGroup.socgr.+impactTargetGroup.welfare.+impactTargetGroup.civsoc.+impactTargetGroup.policy. "
fit <- cfa(model, data=data.questions)
summary(fit, fit.measures = TRUE, standardized = TRUE)




model2 <- "OC =~ impactTargetGroup.pub.+impactTargetGroup.busi.+impactTargetGroup.socgr.+impactTargetGroup.welfare.+impactTargetGroup.civsoc.+impactTargetGroup.policy.+adoptByPolicy.rate.+scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.+Impactstatements.capab.+Impactstatements.emanc.+Impactstatements.understanding.+Impactstatements.mitig.+Impactstatements.unknown.+Impactstatements.unaddressed."
fit2 <- cfa(model2, data=data.questions)

summary(fit2, fit.measures = TRUE, standardized = TRUE)

