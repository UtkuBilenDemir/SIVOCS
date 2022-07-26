source("./03_model/12_si-index_generation.R")

library(MASS)

# ORDINAL LOGISTIC REGRESSION
#-------------------------------------------------------------------------------

ord_log_model.2 <- clm(
  as.factor(round(outputs_outcomes)) ~ 
    solution_orientation + 
     an_transdisciplinary_inv +
     an_transdisciplinary_goals +
     novelty,
  data = train,
  link = "logit"
  )

summary(ord_log_model.2)
summary(ord_log_model)



# 2. DV = si_index.rowmeans

# -> Build a null model first
modelnull.rowmeans <- clm(as.factor(si_index.rowmeans)~1,
                 data = train,
                 link = "logit")
# -> build the real model
model.rowmeans.1 <- clm("as.factor(si_index.rowmeans)~
                        solution_orientation + 
     an_transdisciplinary_inv +
     an_transdisciplinary_goals +
     novelty+
                        outputs_outcomes
                        ",  
              data = train,
              link = "logit")


# 3. DV = si_index.ord_weight

# -> Build a null model first
modelnull.ord_weight <- clm(as.factor(si_index.ord_weight)~1,
                 data = train,
                 link = "logit")
# -> build the real model
model.ord_weight <- clm("as.factor(si_index.ord_weight)~
                        solution_orientation + 
     an_transdisciplinary_inv +
     an_transdisciplinary_goals+
     novelty+
     outputs_outcomes
                        ",  
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
model.ord_weight <- clm("as.factor(si_index.ord_weight)~
                        solution_orientation + 
     an_transdisciplinary_inv +
     an_transdisciplinary_goals +
     novelty+
     outputs_outcomes
                        ",  
              data = train,
              link = "logit")

anova(modelnull.rowmeans, model.ord_weight)
nagelkerke(fit  = model.ord_weight,
           null = modelnull.ord_weight)  
summary(model.ord_weight)
