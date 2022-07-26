source("./03_model//05_logit_model.R")

library(nnet)

multi_log_model <- multinom(round(outcomes) ~ 
                              ia_human_condition + 
                              transdisciplinary_involvement +
                              transdisciplinary_goals +
                              innovativeness,
                            train,
                            Hess = T
)

multi_log_model.sum <- summary(multi_log_model)
w <- multi_log_model.sum$coefficients / multi_log_model.sum$standard.errors
pval <- 2 * (1 - pnorm(abs(w)))
# Reduce the levels of "outcomes" to 4 levels 
df_indexes$outcomes_3 <-  ifelse(
  df_indexes$outcomes > 7, 3,
  ifelse(
    df_indexes$outcomes > 4, 2,
    ifelse(df_indexes$outcomes > 1, 1, 0)
    
  )
)

# Train-test split with 4 outcome categories
inds_2 <- sample(2, nrow(df_indexes), replace = T, prob = c(0.8, 0.2))
train_2 <- df_indexes[inds_2 == 1, ]
test_2 <- df_indexes[inds_2 == 2, ]
multi_log_model.2 <- multinom(outcomes_3 ~ 
                                ia_human_condition + 
                                transdisciplinary_involvement +
                                transdisciplinary_goals +
                                innovativeness,
                              train_2,
                              Hess = T
)

multi_log_model.2_sum <- summary(multi_log_model.2)
w <- multi_log_model.2_sum$coefficients / multi_log_model.2_sum$standard.errors
pval <- 2 * (1 - pnorm(abs(w)))

# Another Model without ia_human_condition
multi_log_model.3 <- multinom(outcomes_3 ~ 
                                transdisciplinary_involvement +
                                transdisciplinary_goals +
                                innovativeness,
                              train_2,
                              Hess = T
)

multi_log_model.3_sum <- summary(multi_log_model.3)
w <- multi_log_model.3_sum$coefficients / multi_log_model.3_sum$standard.errors
pval <- 2 * (1 - pnorm(abs(w)))

anova(multi_log_model.2, multi_log_model.3, test = "Chisq")
# We can correct the variables with low explanatory power with AIC
multi_log_model.4 <- stepAIC(multi_log_model.2, direction = "both", k = log(dim(train_2))[1])
summary(multi_log_model.4)
anova(multi_log_model.2, multi_log_model.4, test = "Chisq")
predictions.2 <- predict(multi_log_model.2, test_2, type = "probs")
cbind(test_2, round(predictions.2, 2))

accurracy.2 <- sum(predict(multi_log_model.2, test_2) == test_2$outcomes_3) / 75
confusion_matrix.2 <- table(predict(multi_log_model.2, test_2), test_2$outcomes_3)
predictions.2_train <- predict(multi_log_model.2, train_2, type = "probs")
cbind(train_2, round(predictions.2_train, 2))

accurracy.2_train <- sum(predict(multi_log_model.2, train_2) == train_2$outcomes_3) / length( train_2$outcomes_3)
confusion_matrix.2_train <- table(predict(multi_log_model.2, train_2), train_2$outcomes_3)
predictions.4 <- predict(multi_log_model.4, test_2, type = "probs")
accurracy.4 <- sum(predict(multi_log_model.4, test_2) == test_2$outcomes_3) / 75
par(mfrow = c(2, 3))

plot(
  df_indexes$ia_human_condition
  , df_indexes$transdisciplinary_involvement
  , lwd = 2
  , lty = 1
  , main =1
  , "lty = 1"
  )
plot(df_indexes[, 1:5])
