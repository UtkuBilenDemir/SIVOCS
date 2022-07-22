source("./07_model_new/07_metadata_SI-index_relations.R")


cfa_five <-
# Intention
"
a =~motivation.welfare.+
benefitForNonAcademy+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
impulseForNonAcad.soc.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.empower.+
impulseForNonAcad.health.+
targetGroupsGoals.diversity.

## Intention-other
b =~ motivation.prob.+
impulseForNonAcad.ecol.+
impulseForNonAcad.tech.+
impulseForNonAcad.econ. 


# Actors
c =~ groupsInvolved.citiz.+
natureOfInvolvement.citiz.+
groupsInvolved.welfare.+
groupsInvolved.civsoc.+
natureOfInvolvement.civsoc.+
kindOfChange.civsoc. 

d =~ kindOfChange.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.

e =~ Impactstatements.capab.+
Impactstatements.emanc.+
Impactstatements.understanding.+
Impactstatements.mitig.+
Impactstatements.unknown.+
impactTargetGroup.socgr.+
impactTargetGroup.policy.+
impactTargetGroup.civsoc.
"

model_theory2 <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  impulseForNonAcad.soc.+ 
  targetGroupsGoals.improve. 
  
  ia_other_nonacad_impulse =~ 
  impulseForNonAcad.econ.+ 
  impulseForNonAcad.ecol.+
  impulseForNonAcad.health.+ 
  impulseForNonAcad.tech.
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~ 
  groupsInvolved.citiz. + 
  groupsInvolved.civsoc. + 
  groupsInvolved.welfare. + 
  natureOfInvolvement.citiz. + 
  natureOfInvolvement.civsoc. + 
  natureOfInvolvement.welfare.
  
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.
  
  # NOVELTY 
  innovativeness =~
  kindOfChange.pub. + 
  kindOfChange.socgr. + 
  kindOfChange.welfare. + 
  kindOfChange.civsoc.
  
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.welfare. + 
  impactTargetGroup.civsoc. + 
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig. + 
  Impactstatements.unknown. + 
  Impactstatements.unaddressed.
  
  # MISC:policy
  policy =~ groupsInvolved.policy.+impactTargetGroup.policy.+kindOfChange.policy.+natureOfInvolvement.policy.+groupsInvolved.policy.+adoptByPolicyHow.SQ001."



model_theory3 <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  targetGroupsGoals.improve. 
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~ 
  groupsInvolved.citiz. + 
  groupsInvolved.civsoc. + 
  groupsInvolved.welfare. + 
  groupsInvolved.policy.+
  natureOfInvolvement.citiz. + 
  natureOfInvolvement.civsoc. + 
  natureOfInvolvement.policy.+
  natureOfInvolvement.welfare.
  
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.
  
  # NOVELTY 
  innovativeness =~
  kindOfChange.pub. + 
  kindOfChange.socgr. + 
  kindOfChange.policy.+
  kindOfChange.welfare. 
  
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.welfare. + 
  impactTargetGroup.civsoc. + 
  impactTargetGroup.policy.+
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig. + 
  Impactstatements.unknown. + 
  Impactstatements.unaddressed.
"

model_theory3.strict <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  targetGroupsGoals.improve. 
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~
  groupsInvolved.citiz.+
  groupsInvolved.civsoc.+ 
  natureOfInvolvement.citiz.+
  natureOfInvolvement.civsoc.
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.
  
  # NOVELTY 
  innovativeness =~
  kindOfChange.pub. + 
  kindOfChange.socgr. + 
  kindOfChange.policy.+
  kindOfChange.welfare. 
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.civsoc. + 
  impactTargetGroup.policy.+
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig.
"
model_theory4 <-"
  # SOLUTION ORIENTATION
  ia_human_condition =~ 
  motivation.welfare.+ 
  benefitForNonAcademy+ 
  targetGroupsGoals.improve. 
  
  #  ACTORS & NETWORKS
  transdisciplinary_involvement =~
  groupsInvolved.citiz.+
  groupsInvolved.civsoc.+ 
  natureOfInvolvement.citiz.+
  natureOfInvolvement.civsoc.
  
  transdisciplinary_goals =~ 
  targetGroupsGoals.socneeds. + 
  targetGroupsGoals.socgroups. + 
  targetGroupsGoals.empower. + 
  targetGroupsGoals.diversity.+
  kindOfChange.socgr.
  
  # OUTCOME
  outcomes =~ 
  impactTargetGroup.pub. + 
  impactTargetGroup.socgr. + 
  impactTargetGroup.civsoc. + 
  impactTargetGroup.policy.+
  Impactstatements.capab. + 
  Impactstatements.emanc. + 
  Impactstatements.understanding. + 
  Impactstatements.mitig.
"


# Scale also df_model
df_model <- na.omit(df_model)
df_model.no_scale <- df_model
df_model <- sapply(FUN=scales::rescale, df_model, c(0, 10))
fit.model2 <- cfa(model_theory3, data = df_model, estimator = "MLR", mimic = "Mplus")
fit.model3.strict <- cfa(model_theory3.strict, data = df_model, estimator = "MLR", mimic = "Mplus")
fit.model4 <- cfa(model_theory4, data = df_model, estimator = "MLR", mimic = "Mplus")
fit.model5 <- cfa(cfa_five, data = df_model, estimator = "MLR", mimic = "Mplus")
# MLR because our data is not normally distributed

semPaths(semPlotModel(fit.model2), rotation = 2)

# Factor loadings should be > 0.7, remove everything below 0.6 (it is Std.all)
# CFI, TLI > 0.9
# RMSEA, SRMR < 0.08
summary(fit.model2, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.model3.strict, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.model4, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.model5, fit.measures = T, standardized = T, rsq = T)  # Not a good fit


# For this time, we are choosing fit.model_strict
pred <- predict(fit.model2)
pred.strict <- predict(fit.model3.strict)
## pred <- t(pred)

df_pred <- apply(apply(pred,MARGIN = 2, scales::rescale, c(0,10)), MARGIN = 2, round, 2)
df_pred.strict <- apply(apply(pred.strict ,MARGIN = 2, scales::rescale, c(0,10)), MARGIN = 2, round, 2)
df_pred <- as.data.frame(df_pred)
df_pred.strict <- as.data.frame(df_pred.strict)






library(blavaan)
# Without prior ddistribution definition
# We don't have distribution assumptions
bfit <- bcfa(
  model_theory3.strict
  , data = df_model
  , burnin = 1000
  , sample = 1000
  , save.lvs =  T
  )



blavInspect(bfit, "lvs") # mcmc.lsit of posterior samples
blavInspect(bfit, "lvmeans")  # matrix of posterior means
# Convergence diagnostics:
blavInspect(bfit, "rhat")  # rhat diagnostic
blavInspect(bfit, "neff")  # effective sample sizes

bfit
















