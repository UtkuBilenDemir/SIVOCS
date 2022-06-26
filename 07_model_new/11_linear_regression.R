source("./07_model_new/10_cfa_model_cross-validation.R")

# -> Test with a simple LM
# DV: OUTCOMES
lm_fitted <- lm(outputs_outcomes ~ 
                  
     solution_orientation + 
     an_transdisciplinary_inv +
     an_transdisciplinary_goals +
     novelty,
   pred_df2
     )
summary(lm_fitted)# Linear Regression Model
# looks good
