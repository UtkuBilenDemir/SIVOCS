source("./07_model_new/09_final_cfa_models.R")

# -> Test with a simple LM
# DV: OUTCOMES
lm_fitted <- lm(outputs_outcomes ~ 
                  
     solution_orientation + 
     actors_networks +
     trans_goals_achievements +
     novelty,
   pred_df2
     )
summary(lm_fitted)# Linear Regression Model
# looks good
