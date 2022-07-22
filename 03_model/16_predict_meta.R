source("./03_model/15_check_soundness_of_the_cfa_model.R")

# Create a df with metadata and cfa predicted df
cfa_pred_df <- as.data.frame(cbind(pred_df2, meta_df))
outcomes4 <- ifelse(
  cfa_pred_df$outputs_outcomes<3, 0, 
  ifelse(cfa_pred_df$outputs_outcomes<6, 1,
         ifelse(cfa_pred_df$outputs_outcomes<8, 2, 3
                )
  )
)

multi_log_model.domain <- multinom(domain  ~ 
    solution_orientation + 
    an_transdisciplinary_inv +
    an_transdisciplinary_goals +
    novelty + 
      outputs_outcomes
       ,
                            cfa_pred_df,
                            Hess = T
)

summary(multi_log_model.domain)

# ------------------------------------------------------------------------------
# ORDINAL LOGIT
# PREDICTING OUTCOME

# 1. DV: OUTCOMES
ord_log_model <- polr(
  as.factor(outputs_outcomes) ~ 
    solution_orientation + 
    an_transdisciplinary_inv +
    an_transdisciplinary_goals +
    novelty,
  cfa_pred_df,
  Hess = T
  )
ord_log_model.2 <- clm(
  as.factor(outputs_outcomes) ~ 
  solution_orientation + 
    an_transdisciplinary_inv +
    an_transdisciplinary_goals +
    novelty,
  data = train,
  link = "logit"
  )

ord_log_model.2
summary(ord_log_model.2)
summary(ord_log_model)


pred <- predict(ord_log_model, train[1:5, ], type = "prob")
print(pred, digits = 3)

