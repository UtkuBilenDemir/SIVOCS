source("./03_model//07_metadata_SI-index_relations.R")

# df_model doesn't include the scalability variables but
# we intend to use 2 of those in the new cfa model
# -> keep the out & up-scalability
# -> remove all purely academic variables

df_model2 <- feat_df.num_o %>%
  dplyr::select(-all_of(c(
    factor_1
    , "contribToSI.rate."
    , "motivation.pheno."
    , "scalabilityRating.deep."
    , "groupsInvolved.res."
    , "impactTargetGroup.acad."
    , "kindOfChange.acad."
    , "concepts.pub."
    , "concepts.data."
    , "concepts.code."
    , "concepts.infra."          
    , "concepts.review."
    , "concepts2"
    ## , "concepts3"
    )))

# a less strict version of the reduced dataframe
df_less_red.rescaled <- as.data.frame(cbind(df_red.rescaled, df_model2[, c("scalabilityRating.out.", "scalabilityRating.up.", "concepts3", "adoptByPolicy.rate.")]))

# An overlook on the correlations
library(corrr)
corr_matrix <- df_model2 %>%
  correlate() %>% 
  stretch()

best_correlations <- corr_matrix[order(corr_matrix$r, decreasing = T),]
write.csv(best_correlations[1:100, ], "./03_model//best_correlations.csv")

# OLD MODELS
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


# NEW MODELS

# Collection of the variables that doesn't fit well:
# impulseForNonAcad.tech.+
# impulseForNonAcad.ecol.+
# impulseForNonAcad.health.+
# impulseForNonAcad.econ.+
# motivation.prob.+
# ---
# groupsInvolved.busi.+
# natureOfInvolvement.media.+
# natureOfInvolvement.busi.+
# ---
# Impactstatements.unknown.+
# Impactstatements.unaddressed.+

miro_model <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
Impactstatements.unknown.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.

outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.+ 
Impactstatements.emanc. +
Impactstatements.capab.
"

# New category as trans_goals_achievements
miro_model2 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.+
Impactstatements.unknown.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.

outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# targetgroup goals as novelty maybe?
miro_model3 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.policy.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.policy.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
impactTargetGroup.policy.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
kindOfChange.policy.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# No policy
miro_model4 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
groupsInvolved.welfare.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.+
natureOfInvolvement.welfare.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.welfare.+
impactTargetGroup.civsoc.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.welfare.+
kindOfChange.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# No welfare
miro_model5 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.+
targetGroupsGoals.empower.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.

novelty =~
scalabilityRating.out.+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
scalabilityRating.up.+
Impactstatements.unknown.


outputs_outcomes =~
targetGroupsGoals.diversity.+
concepts3+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# Broader outcome
miro_model6 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.+
Impactstatements.unknown.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.

outputs_outcomes =~
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.
"

# Even Broader outcome
miro_model7 <- "
solution_orientation =~
motivation.welfare.+
benefitForNonAcademy+
impulseForNonAcad.soc.+
targetGroupsGoals.improve.

actors_networks =~
groupsInvolved.civsoc.+
groupsInvolved.citiz.+
natureOfInvolvement.civsoc.+
natureOfInvolvement.citiz.

trans_goals_achievements =~
Impactstatements.emanc. +
Impactstatements.capab.+
targetGroupsGoals.socneeds.+
targetGroupsGoals.socgroups.+
targetGroupsGoals.empower.+
targetGroupsGoals.diversity.

novelty =~
scalabilityRating.out.+
scalabilityRating.up.+
Impactstatements.unknown.

outputs_outcomes =~
concepts3+
impactTargetGroup.pub.+
impactTargetGroup.socgr.+
impactTargetGroup.civsoc.+
adoptByPolicy.rate.+
Impactstatements.mitig.+
Impactstatements.understanding.+
kindOfChange.pub.+
kindOfChange.socgr.+
kindOfChange.civsoc.
"


fit.miro_model <- cfa(
  miro_model6
  , data = df_model2
  , estimator = "MLR"
  , mimic = "Mplus"
  , std.lv = T
  )

fit.miro_model_ordered <- cfa(
  miro_model6
  , data = df_model2
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )

fit.old_model_ordered <- cfa(
  model_theory4
  , data = df_model2
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )

summary(fit.miro_model_ordered, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.old_model_ordered, fit.measures = T, standardized = T, rsq = T)  # Not a good fit
summary(fit.miro_model, fit.measures = T, standardized = T, rsq = T)  # Not a good fit

parameterestimates(fit.miro_model)
fitmeasures(fit.miro_model_ordered)

# Share this in annex 
cov(df_model2)
