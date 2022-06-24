source("./07_model_new/07_metadata_SI-index_relations.R")


# df_model is a reduced df without PFA
# -> apply efa and cfa to this model
df_model

# Scree Plot
#-------------------------------------------------------------------------------
ev <- eigen(cor(df_model)) # get eigenvalues

ap <- parallel(subject=nrow(df_model), var=ncol(df_model),
               rep=1000,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)  # 9 Factors

df.model <- as.data.frame(df_model)
df_model.rescaled <- sapply(df_model, scales::rescale, c(0,10))
df_model.rescaled <- na.omit(df_model.rescaled)
efa_model <- fa.parallel(df_model, fm = "ml", fa = "fa")
efa_model$fa.values  # 4 or 5 factors

eight_factor_v <- fa(df_model, "varimax", fm = "ml", nfactors = 8)
eight_factor_o <- fa(df_model, "oblimin", fm = "ml", nfactors = 8)


anova(
  six_factor_v
  , six_factor_o
  , six_factor_q
  , six_factor_p
  , six_factor_c
)


summary(six_factor_v)
summary(six_factor_o)
summary(six_factor_q)
summary(six_factor_p)
summary(six_factor_c)


# 8 factor not bad results
summary(eight_factor_v)

print(eight_factor_v, cut = .4, digits = 2)
plot(five_factor)
cor.plot(five_factor)



---



