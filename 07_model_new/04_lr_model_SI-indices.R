# TODO: ROW SUM Index

source("./07_model_new/03_cfa_models.R")

library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds
library(magrittr)



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
si_index.strict_which <- df_fitted %>%
  dplyr::filter(.[[df_colnames[1]]] > 3) %>%
  dplyr::filter(.[[df_colnames[2]]] > 3) %>%
  dplyr::filter(.[[df_colnames[3]]] > 3) %>%
  dplyr::filter(.[[df_colnames[4]]] > 3) %>%
  dplyr::filter(.[[df_colnames[5]]] > 3) %>%
  dplyr::select(id)

si_index.strict <- rep(0, nrow(df_pred.strict)) 
si_index.strict[unlist(si_index.strict_which)] <- 1
sum(si_index.strict)
 
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

# DF creation and train test split
#-------------------------------------------------------------------------------
df_indexes <- as.data.frame(cbind(
  df_pred.strict
  , si_index.rowmeans     = round(si_index.rowmeans, 1)
  , si_index.strict       = round(si_index.strict, 1)
  , si_index.ord_weight   = round(si_index.ord_weight, 1)
  , si_index.ord          = round(si_index.ord, 1)         
))

# Train-test split
inds <- sample(2, nrow(df_indexes), replace = T, prob = c(0.8, 0.2))
train <- df_indexes[inds == 1, ]
test <- df_indexes[inds == 2, ]

# Linear Regression Model
#-------------------------------------------------------------------------------
# DV: OUTCOMES
lm_fitted <- lm(outcomes ~ 
     ia_human_condition + 
     transdisciplinary_involvement +
     transdisciplinary_goals +
     innovativeness,
   df_pred.strict
     )
summary(lm_fitted)# Linear Regression Model

# DV: ord_weight
lm_fitted.ord_weight <- lm(si_index.ord_weight ~
     ia_human_condition + 
     transdisciplinary_involvement +
     transdisciplinary_goals +
     innovativeness + 
    outcomes
       ,
   train
     )
summary(lm_fitted.ord_weight)


# DV: ord_weight
lm_fitted.ord <- lm(si_index.ord ~
     ia_human_condition + 
     transdisciplinary_involvement +
     transdisciplinary_goals +
     innovativeness + 
    outcomes
       ,
   train
     )
summary(lm_fitted.ord)
