source("./07_model_new/03_cfa_models.R")

library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds
library(magrittr)

# Linear Regression Model
#-------------------------------------------------------------------------------
lm_fitted <- lm(outcomes ~ 
     ia_human_condition + 
     transdisciplinary_involvement +
     transdisciplinary_goals +
     innovativeness,
   df_pred.strict
     )
summary(lm_fitted)

# Build SI-indices
#-------------------------------------------------------------------------------

# 1. Row means based index
si_index.rowmeans <- rowMeans(df_pred.strict)

# 2. These are what we call socially innovative "projects"
# Incrementally higher than 3 in all of the fitted features
# This approach is too strict
# a new df
df_fitted <- df_pred
df_colnames <- colnames(df_pred)
df_fitted <- as.data.frame(df_fitted)
df_fitted$id <- 1:nrow(df_fitted)

# These are what we call socially innovative "projects"
# Incrementally higher than 3 in all of the fitted features
si_index.strict <- df_fitted %>%
  dplyr::filter(.[[df_colnames[1]]] > 3) %>%
  dplyr::filter(.[[df_colnames[2]]] > 3) %>%
  dplyr::filter(.[[df_colnames[3]]] > 3) %>%
  dplyr::filter(.[[df_colnames[4]]] > 3) %>%
  dplyr::filter(.[[df_colnames[5]]] > 3) %>%
  dplyr::select(id)
 
# 3. Note each of the columns (some coluns include subcolumns)
si_index.ord_weight <- rep(0, nrow(df_pred.strict)) 

si_index.ord_weight <- ifelse(
  df_pred.strict$ia_human_condition > 6, si_index.ord_weight + 2,
    ifelse(df_pred.strict$ia_human_condition > 3, si_index.ord_weight + 1, 
      si_index.ord_weight + 0
           )
       )
si_index.ord_weight <- ifelse(
  df_pred.strict$transdisciplinary_involvement > 6, si_index.ord_weight + 1,
    ifelse(df_pred.strict$transdisciplinary_involvement > 3, si_index.ord_weight + 0.5, 
      si_index.ord_weight + 0
           )
       )
si_index.ord_weight <- ifelse(
  df_pred.strict$transdisciplinary_goals > 6, si_index.ord_weight + 1,
    ifelse(df_pred.strict$transdisciplinary_goals > 3, si_index.ord_weight + 0.5, 
      si_index.ord_weight + 0
           )
       )
si_index.ord_weight <- ifelse(
  df_pred.strict$innovativeness > 6, si_index.ord_weight + 2,
    ifelse(df_pred.strict$innovativeness > 3, si_index.ord_weight + 1, 
      si_index.ord_weight + 0
           )
       )
si_index.ord_weight <- ifelse(
  df_pred.strict$outcomes > 6, si_index.ord_weight + 2,
    ifelse(df_pred.strict$outcomes > 3, si_index.ord_weight + 1, 
      si_index.ord_weight + 0
           )
       )

si_index.ord_weight <- round(scales::rescale(si_index.ord_weight, c(0,10)), 2)

# 4. Note each of the columns (subcolumns are not weighted)
si_index.ord <- rep(0, nrow(df_pred.strict)) 
si_index.ord <- ifelse(
  df_pred.strict$ia_human_condition > 6, si_index.ord + 2,
    ifelse(df_pred.strict$ia_human_condition > 3, si_index.ord + 1, 
      si_index.ord + 0
           )
       )
si_index.ord <- ifelse(
  df_pred.strict$transdisciplinary_involvement > 6, si_index.ord + 2,
    ifelse(df_pred.strict$transdisciplinary_involvement > 3, si_index.ord + 1, 
      si_index.ord + 0
           )
       )
si_index.ord <- ifelse(
  df_pred.strict$transdisciplinary_goals > 6, si_index.ord + 2,
    ifelse(df_pred.strict$transdisciplinary_goals > 3, si_index.ord + 1, 
      si_index.ord + 0
           )
       )
si_index.ord <- ifelse(
  df_pred.strict$innovativeness > 6, si_index.ord + 2,
    ifelse(df_pred.strict$innovativeness > 3, si_index.ord + 1, 
      si_index.ord + 0
           )
       )
si_index.ord <- ifelse(
  df_pred.strict$outcomes > 6, si_index.ord + 2,
    ifelse(df_pred.strict$outcomes > 3, si_index.ord + 1, 
      si_index.ord + 0
           )
       )

si_index.ord <- round(scales::rescale(si_index.ord, c(0,10)), 2)

# Ordinal Logistic Regression
#-------------------------------------------------------------------------------


# Split into 2 groups
inds <- sample(2, nrow(df_pred.strict), replace = T, prob = c(0.8, 0.2))
train <- df_pred.strict[inds == 1, ]
test <- df_pred.strict[inds == 2, ]

library(MASS)
ord_log_model <- polr(
  as.factor(outcomes) ~ 
    ia_human_condition + 
    transdisciplinary_involvement +
    transdisciplinary_goals +
    innovativeness,
  train,
  Hess = T
  )
summary(ord_log_model)


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


#-------------------------------------------------------------------------------
# LOGIT

# -> Create an index where the (fitted) features are used incrementally
# to apply a binary SI classification

# a new df
df_fitted <- df_pred
df_colnames <- colnames(df_pred)
df_fitted <- as.data.frame(df_fitted)
df_fitted$id <- 1:nrow(df_fitted)

str(df_fitted)


# These are what we call socially innovative "projects"
# Incrementally higher than 3 in all of the fitted features
si_ids <- df_fitted %>%
  dplyr::filter(.[[df_colnames[1]]] > 3) %>%
  dplyr::filter(.[[df_colnames[2]]] > 3) %>%
  dplyr::filter(.[[df_colnames[3]]] > 3) %>%
  dplyr::filter(.[[df_colnames[4]]] > 3) %>%
  dplyr::filter(.[[df_colnames[5]]] > 3) %>%
  dplyr::select(id)
  

# Create a binary feature which classifies between socially innovative 
# and non-innovative projects
df_fitted$si <- 0
df_fitted$si[unlist(si_ids)] <- 1

# Remove the ids column
df_fitted <- df_fitted[, colnames(df_fitted) != "id"]

log_model <- glm(si ~ ., data = df_fitted, family = "binomial")
summary(log_model)

