source("./07_model_new/04_lr_model_SI-indices.R")

library(MASS)

# ORDINAL LOGISTIC REGRESSION
#-------------------------------------------------------------------------------

# 1. DV: OUTCOMES
ord_log_model.1 <- polr(
  as.factor(round(outcomes)) ~ 
    ia_human_condition + 
    transdisciplinary_involvement +
    transdisciplinary_goals +
    innovativeness,
  train,
  Hess = T
  )
ord_log_model.2 <- clm(
  "as.factor(round(outcomes))~ia_human_condition+transdisciplinary_involvement+transdisciplinary_goals+innovativeness",
  train,
  link = "logit"
  )
summary(ord_log_model.2)

summary(ord_log_model.1)




(ctable <- coef(summary(ord_log_model)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
(ctable <- cbind("p value" = p))


train[1:5, "outcomes"]
pred <- predict(ord_log_model, train[1:5, ], type = "prob")
print(pred, digits = 2)
colnames(pred)[apply(pred, 1,which.max)]


pred2 <- predict(ord_log_model, test[1:5, ], type = "prob")
print(pred2, digits = 2)
colnames(pred2)[apply(pred2, 1,which.max)]

pred3 <- predict(ord_log_model, train)
(tab <- table(pred3, train$outcomes))
1- sum(diag(tab))/sum(tab)
babap <- as.data.frame(tab)

# 2. DV = si_index.rowmeans

# -> Build a null model first
modelnull.rowmeans <- clm(as.factor(si_index.rowmeans)~1,
                 data = train,
                 link = "logit")
# -> build the real model
model.rowmeans.1 <- clm("as.factor(si_index.rowmeans)~ia_human_condition+transdisciplinary_involvement+transdisciplinary_goals+innovativeness+outcomes",  
              data = train,
              link = "logit")

model.rowmeans.2 <- polr("as.factor(si_index.rowmeans)~ia_human_condition+transdisciplinary_involvement+transdisciplinary_goals+innovativeness+outcomes",  
              data = train,
              Hess = T)

anova(modelnull.rowmeans, model.rowmeans)
nagelkerke(fit  = model.rowmeans,
           null = modelnull.rowmeans)  


summary(model.rowmeans.1)

# 3. DV = si_index.ord_weight

# -> Build a null model first
modelnull.ord_weight <- clm(as.factor(si_index.ord_weight)~1,
                 data = train,
                 link = "logit")
# -> build the real model
model.ord_weight <- clm("as.factor(si_index.ord_weight)~ia_human_condition+transdisciplinary_involvement+transdisciplinary_goals+innovativeness+outcomes",  
              data = train,
              link = "logit")

anova(modelnull.rowmeans, model.ord_weight)
nagelkerke(fit  = model.ord_weight,
           null = modelnull.ord_weight)  


summary(model.ord_weight)
confint(model.ord_weight)
exp(coef(model.ord_weight))
exp(confint(model.ord_weight))


# 4. DV = si_index.ord

# -> Build a null model first
modelnull.ord_weight <- clm(as.factor(si_index.ord_weight)~1,
                 data = train,
                 link = "logit")
# -> build the real model
model.ord_weight <- clm("as.factor(si_index.ord_weight)~ia_human_condition+transdisciplinary_involvement+transdisciplinary_goals+innovativeness+outcomes",  
              data = train,
              link = "logit")

anova(modelnull.rowmeans, model.ord_weight)
nagelkerke(fit  = model.ord_weight,
           null = modelnull.ord_weight)  


summary(model.ord_weight)


#-------------------------------------------------------------------------------
# LOGIT

log_model <- glm(si_index.strict ~ ., data = train[, c(1:5, 7)], family = "binomial")
summary(log_model)


# LOGIT MODEL AGAINST OUTCOME
log_model.strict <- glm(ifelse(outcomes > 5, 1, 0) ~ ., data = train[, 1:5], family = "binomial")
summary(log_model.strict)




