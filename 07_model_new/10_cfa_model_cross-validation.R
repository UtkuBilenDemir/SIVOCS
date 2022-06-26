source("./07_model_new/09_final_cfa_models.R")

# NOTES
# ---
#  Here is a 5 fold cross validation with a 0.2 to 0.8 test split
# mean squared distances between fit parameters are minimal

cv_df <- data.frame()
# create an index generator for CV
inds <- sample.int(5, nrow(df_model2), replace = T, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
for (i in unique(inds)) {
  test_df <- df_model2[inds == i, ]
  train_df <- df_model2[inds != i, ]
  
  train_fit <- cfa(
  final_model2
  , data = train_df
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )

  test_fit <- cfa(
  final_model2
  , data = test_df
  , estimator = "WLSMV"
  , mimic = "Mplus"
  , std.lv = T
  , ordered = T
  )
  
  train_sum <- summary(
  train_fit
  , fit.measures = T
  , standardized = T
  , rsq = T
  )

  test_sum <- summary(
  test_fit
  , fit.measures = T
  , standardized = T
  , rsq = T
  )
  
  params <- c("tli.scaled", "cfi.scaled", "rmsea")
  print(paste0(i, ". cycle"))
  print(paste0("train_params", train_sum$FIT[params]))
  print(paste0("test_params", test_sum$FIT[params]))
  cv_df <- rbind(
    cv_df
    , train_sum$FIT[params] - test_sum$FIT[params]
    , train_sum$PE$std.all - test_sum$PE$std.all
    )
  
}

cv_df <- cv_df^2
# Mean squared distances between the 
cv_df.colmeans <- colMeans(cv_df)
which(cv_df.colmeans == max(cv_df.colmeans, na.rm = T ))
cv_df.colmeans[62]
