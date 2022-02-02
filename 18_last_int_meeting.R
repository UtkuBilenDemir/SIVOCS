library(psych)
source("./02_analysis/02_static_responses.R")

# ----------- PFA

# Load the feature importances from python script
feature_list <- read.csv("./02_analysis/PFA_feature_importance.csv")
features_to_rm <- tail(feature_list$X0, 40)

# Qualitatively select the worse performing features
features_to_rm <- features_to_rm[c(22,26,29:40)]
features_to_rm <- c(features_to_rm, 
                    "dissChannels.trad.",
                    "dissChannels.socmed.",
                    "dissChannels.consult.",
                    "dissChannels.events.", 
                    "dissChannels.public.",
                    "concepts.data.",
                    "concepts.code.",
                    "concepts.infra.",
                    "contribToSI.rate.",
                    "groupsInvolved.res.",
                    "natureOfInvolvement.res.",
                    "contribToSI.rate."
                    )

# remove the weak features
df_red <- feat_df.num_o[, !(colnames(feat_df.num_o ) %in% features_to_rm)]

# ----------- FACTOR ANALYSIS

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(df_red)) # get eigenvalues

ap <- parallel(subject=nrow(df_red),var=ncol(df_red),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors


# PCA Variable Factor Map 
library(FactoMineR)
result <- PCA(df_red) # graphs generated automatically

# --- FA explanatory
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(df_red, 8, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(df_red),cex=.7) # add variable names


fit$loadings


# ---------- CFA Model
library(lavaan)
mydata.cov <- cov(df_red)
model <-"
  ## Factor 1 is policy
  policy =~ groupsInvolved.policy.+natureOfInvolvement.policy.+concepts3+impactTargetGroup.policy.+kindOfChange.policy.+adoptByPolicy.rate.+dissChannels.policy.+adoptByPolicyHow.SQ001.+adoptByPolicyHow.SQ002.+adoptByPolicyHow.SQ003.
  
  ## Factor 2 is targetGroup
  targetGroup =~ targetGroupsGoals.empower.+dissChannels.events.+targetGroupsGoals.diversity.+groupsInvolved.welfare.+natureOfInvolvement.citiz.+targetGroupsGoals.socneeds.+targetGroupsGoals.socneeds.+kindOfChange.civsoc.+Impactstatements.capab.+Impactstatements.understanding.
  
  ## Factor 3 is targetGroup
  scale =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  
  ## Factor 4 is business
  business =~ groupsInvolved.busi.+natureOfInvolvement.busi.+kindOfChange.busi.
  
  ## Factor 5 is familiarity 
  familiarity =~ familiarWithSI.response.
  
  ## Factor 6 is concepts 
  concepts =~ concepts.pub.+concepts.code.+concepts.infra.+concepts.review.
  
  ## Factor 7 is civsoc
  civsoc =~ natureOfInvolvement.civsoc.
  
  ## Factor 8 is impulse
  impulse =~ impulseForNonAcad.health.+motivation.welfare.+impulseForNonAcad.ecol.
  
  ## Factor 9 is 
  change =~ kindOfChange.pub.+kindOfChange.busi.+kindOfChange.civsoc.+kindOfChange.socgr.
  
  ## Factor 10 is motprob
  motprob =~ motivation.welfare.
  "
colnames(df_red)

fit <- cfa(model, data = sapply(FUN=scale, na.omit(df_red)) )
summary(fit, fit.measures=TRUE, standardized=TRUE)

fit




fit2 <- cfa(model, df_red)
summary(fit2)



modificationindices(fit)




modelb <-"
  ## Factor 1 is policy
  policy =~ groupsInvolved.policy.+natureOfInvolvement.policy.+concepts3+impactTargetGroup.policy.+kindOfChange.policy.+adoptByPolicy.rate.+dissChannels.policy.+adoptByPolicyHow.SQ001.+adoptByPolicyHow.SQ002.+adoptByPolicyHow.SQ003.
  
  ## Factor 2 is targetGroup
  targetGroup =~ targetGroupsGoals.empower.+dissChannels.events.+targetGroupsGoals.diversity.+groupsInvolved.welfare.+natureOfInvolvement.citiz.+targetGroupsGoals.socneeds.+targetGroupsGoals.socneeds.+kindOfChange.civsoc.+Impactstatements.capab.+Impactstatements.understanding.
  
  ## Factor 3 is targetGroup
  scale =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  
  ## Factor 4 is business
  business =~ groupsInvolved.busi.+natureOfInvolvement.busi.+kindOfChange.busi.
  
  ## Factor 5 is familiarity 
  familiarity =~ familiarWithSI.response.
  
  ## Factor 6 is concepts 
  concepts =~ concepts.pub.+concepts.code.+concepts.infra.+concepts.review.
  
  ## Factor 7 is civsoc
  civsoc =~ natureOfInvolvement.civsoc.
  
  ## Factor 8 is impulse
  impulse =~ impulseForNonAcad.health.+motivation.welfare.+impulseForNonAcad.ecol.
  
  ## Factor 9 is change
  change =~ kindOfChange.pub.+kindOfChange.busi.+kindOfChange.civsoc.+kindOfChange.socgr.+motivation.welfare.
  "

fit3 <- cfa(modelb, df_red)
lavInspect(fit3, "cov.lv")

library(magrittr)
library(tidyverse)

modificationindices(fit3) %>% arrange(-mi) %>% head(10)

pchisq(66.125,1, lower.tail = FALSE)





modelc <-"
  ## Factor 1 is policy
  policy =~ groupsInvolved.policy.+natureOfInvolvement.policy.+concepts3+impactTargetGroup.policy.+kindOfChange.policy.+adoptByPolicy.rate.+dissChannels.policy.+adoptByPolicyHow.SQ001.+adoptByPolicyHow.SQ002.+adoptByPolicyHow.SQ003.
  
  ## Factor 2 is targetGroup
  targetGroup =~ targetGroupsGoals.empower.+dissChannels.events.+targetGroupsGoals.diversity.+groupsInvolved.welfare.+natureOfInvolvement.citiz.+targetGroupsGoals.socneeds.+targetGroupsGoals.socneeds.+kindOfChange.civsoc.+Impactstatements.capab.+Impactstatements.understanding.
  
  ## Factor 3 is targetGroup
  scale =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  
  ## Factor 4 is business
  business =~ groupsInvolved.busi.+natureOfInvolvement.busi.+kindOfChange.busi.
  
  ## Factor 5 is familiarity 
  familiarity =~ familiarWithSI.response.
  
  ## Factor 6 is concepts 
  concepts =~ concepts.pub.+concepts.code.+concepts.infra.+concepts.review.
  
  ## Factor 7 is civsoc
  civsoc =~ natureOfInvolvement.civsoc.
  
  ## Factor 8 is impulse
  impulse =~ impulseForNonAcad.health.+motivation.welfare.+impulseForNonAcad.ecol.
  
  ## Factor 9 is change
  change =~ kindOfChange.pub.+kindOfChange.busi.+kindOfChange.civsoc.+kindOfChange.socgr.+motivation.welfare.
groupsInvolved.policy. ~~ natureOfInvolvement.policy.

"

fit4 <- cfa(modelc, df_red)
lavInspect(fit4, "cov.lv")

modificationindices(fit4) %>% arrange(-mi) %>% head(10)

pchisq(66.125,1, lower.tail = FALSE)

anova(fit3, fit4)


parameterestimates(fit4)


fitmeasures(fit4)
 
length(colnames(df_red))

model_theory <-"
  ## SI Familiarity
  fam =~ familiarWithSI.response.+transdisciplinaryExp.rate.
  
  ## intention_agency
  ia_human_condition =~ motivation.welfare.+benefitForNonAcademy+impulseForNonAcad.soc.+targetGroupsGoals.improve.+impulseForNonAcad.health.+impulseForNonAcad.ecol.
  
  ia_non_academic =~ impulseForNonAcad.econ.+impulseForNonAcad.tech.
  
  ## transdisciplinary_apects
  transdisciplinary_social =~ groupsInvolved.citiz.+groupsInvolved.civsoc.+groupsInvolved.welfare.+natureOfInvolvement.citiz.+natureOfInvolvement.civsoc.+natureOfInvolvement.welfare.+targetGroupsGoals.socneeds.+targetGroupsGoals.socgroups.+targetGroupsGoals.empower.+targetGroupsGoals.diversity.
  
  ## outcome
  outcome_public =~ impactTargetGroup.pub.+impactTargetGroup.socgr.+impactTargetGroup.welfare.+impactTargetGroup.civsoc.+kindOfChange.pub.+kindOfChange.socgr.+kindOfChange.welfare.+kindOfChange.civsoc.
  
  outcome_statement =~ Impactstatements.capab.+Impactstatements.emanc.+Impactstatements.understanding.+Impactstatements.mitig.+Impactstatements.unknown.+Impactstatements.unaddressed.
  
  ## MISC:scalability
  scale =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  
  ## MISC:policy
  policy =~ groupsInvolved.policy.+impactTargetGroup.policy.+kindOfChange.policy.+natureOfInvolvement.policy.+groupsInvolved.policy.+adoptByPolicyHow.SQ001. 
  
  ## MISC:business
  busi =~ groupsInvolved.busi.+impactTargetGroup.busi.+kindOfChange.busi.
"

model_efa <- "
  f1 =~ impulseForNonAcad.soc.+groupsInvolved.citiz.+targetGroupsGoals.socneeds.+targetGroupsGoals.socgroups.+impactTargetGroup.socgr.+Impactstatements.capab.+Impactstatements.emanc.+Impactstatements.understanding.+Impactstatements.mitig.+transdisciplinaryExp.rate.+familiarWithSI.response. 
  f2 =~ motivation.welfare.+benefitForNonAcademy+targetGroupsGoals.improve.+impulseForNonAcad.health.+impactTargetGroup.pub. 
  f3 =~ groupsInvolved.policy.+impactTargetGroup.policy.+kindOfChange.policy.+natureOfInvolvement.policy.+impulseForNonAcad.econ.+natureOfInvolvement.policy.
  f4 =~ groupsInvolved.welfare.+natureOfInvolvement.welfare.+impactTargetGroup.welfare.+kindOfChange.welfare. 
  f5 =~ groupsInvolved.civsoc.+natureOfInvolvement.civsoc.+impactTargetGroup.civsoc.+kindOfChange.civsoc.
  f6 =~ impactTargetGroup.busi.+kindOfChange.busi.+groupsInvolved.busi.
  f7 =~ scalabilityRating.up.+scalabilityRating.out.+scalabilityRating.deep.
  f8 =~ kindOfChange.pub.+kindOfChange.socgr.+kindOfChange.acad.
"

colnames(df_red)

fit_theory <- cfa(model_theory, df_red)
fit_efa <- cfa(model_efa, df_red)

summary(fit_theory, fit.measures=TRUE, standardized = TRUE)
summary(fit_efa, fit.measures=TRUE, standardized = TRUE)
lavInspect(fit_theory, "cov.lv")
lavInspect(fit_efa, "cov.lv")

modificationindices(fit_theory) %>% arrange(-mi) %>% head(10)
modificationindices(fit_efa) %>% arrange(-mi) %>% head(10)

pchisq(66.125,1, lower.tail = FALSE)

anova(fit_theory, fit_efa)


parameterestimates(fit4)




DF.refined <- data.frame(predict(fit_theory))


a <- scales::rescale(apply(DF.refined, mean, MARGIN = 1), to = c(0, 10))
b <- feat_df.num_o$contribToSI.rate.
c <- 1:length(a)

ab <- as.data.frame(cbind(a,b,c))
library(ggplot2)
ggplot(ab, aes(x = c)) + 
  geom_line(aes(y = a), color = "darkred") + 
  geom_line(aes(y = b), color="steelblue") 



DF.refined2 <- data.frame(predict(fit_efa))

d <- scales::rescale(apply(DF.refined2, mean, MARGIN = 1), to = c(0, 10))

db <- as.data.frame(cbind(d,b,c))
ggplot(db, aes(x = c)) + 
  geom_line(aes(y = d), color = "darkred") + 
  geom_line(aes(y = b), color="steelblue") 




var_prop_efa <- c(0.13,  0.06,    0.05,    0.05,    0.05,    0.05,    0.04,    0.03)
new_DF_refined2 <- as.data.frame( as.matrix(DF.refined2) %*% diag(var_prop_efa))

f <- scales::rescale(apply(new_DF_refined2, mean, MARGIN = 1), to = c(0, 10))
mean(f-b)
mean(a-b)
mean(d-b)


feature_list_red <- feature_list[!(feature_list$X0 %in% features_to_rm),]
feature_list_red$logX <-  log(feature_list_red$X1)

for (i in seq_along(ncol(df_red)))

matrix(df_red) diag(feature_list_red$logX)
