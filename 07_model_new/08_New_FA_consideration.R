source("./07_model_new/07_metadata_SI-index_relations.R")


# df_model is a reduced df without PFA
# -> apply efa and cfa to this model
df_model

# Scree Plot
#-------------------------------------------------------------------------------
ev <- eigen(cor(df_model)) # get eigenvalues

ap <- parallel(subject=nrow(df_model), var=ncol(df_model),
               rep=1000,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors

df.model <- as.data.frame(df_model)
df_model.rescaled <- sapply(df_model, scales::rescale, c(0,10))
df_model.rescaled <- na.omit(df_model.rescaled)
efa_model <- fa.parallel(df_model, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors

eight_factor_v <- fa(df_model, "varimax", fm = "ml", nfactors = 8)
eight_factor_o <- fa(df_model, "oblimin", fm = "ml", nfactors = 8)


anova(
  six_factor_v
  , six_factor_o
  , six_factor_q
  , six_factor_p
  , six_factor_c
)


summary(six_factor_v)
summary(six_factor_o)
summary(six_factor_q)
summary(six_factor_p)
summary(six_factor_c)


# 8 factor not bad results
summary(eight_factor_v)

print(eight_factor_v, cut = .4, digits = 2)



# ---
  

model_miro_1 <-"

motivation_change =~ motivation.welfare.+
benefitForNonAcademy+
kindOfChange.pub.+
kindOfChange.busi.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.

motivation_impulse =~ 
impulseForNonAcad.health.


transdisciplinary_involvement =~ groupsInvolved.busi.+
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.media.+
groupsInvolved.welfare.+
natureOfInvolvement.res.+
natureOfInvolvement.busi.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.media.+
natureOfInvolvement.welfare.



transdisciplinary_goals =~ 
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

outcomes =~ impactTargetGroup.pub.+
impactTargetGroup.busi.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
Impactstatements.capab.+
Impactstatements.emanc.+
Impactstatements.understanding.+
Impactstatements.mitig.+
Impactstatements.unknown.+
Impactstatements.unaddressed.
"

df_model <- as.data.frame(df_model)
df_model$impulseForNonAcad.health.

fit.model_miro <- cfa(model_miro_1, data = df_model, estimator = "MLR", mimic = "Mplus")

summary(fit.model_miro, fit.measures = T, standardized = T, rsq = T)  # Not a good fit



impulseForNonAcad.soc.+
impulseForNonAcad.econ.+
  
impulseForNonAcad.ecol.+
  
impulseForNonAcad.tech.