source("./07_model_new/16_predict_meta.R")

summary(pred_df2)

summary(
  fit.final_model2
  , fit.measures = T
  , standardized = T
  , rsq = T
  )

parameterEstimates(fit.final_model2, standardized = TRUE)



#-------------------------------------------------------------------------------
# Visualise domains among factors

# Find the mean of each group
library(plyr)
cfa_pred_df.rating_mean <- ddply(cfa_pred_df, "domain", summarise, solution_orientation.rating.mean=mean(solution_orientation))
cfa_pred_df.rating_mean$an_transdisciplinary_inv.rating.mean <- ddply(cfa_pred_df, "domain", summarise, an_transdisciplinary_inv.rating.mean=mean(an_transdisciplinary_inv))$an_transdisciplinary_inv.rating.mean
cfa_pred_df.rating_mean$an_transdisciplinary_goals.rating.mean <- ddply(cfa_pred_df, "domain", summarise, an_transdisciplinary_goals.rating.mean=mean(an_transdisciplinary_goals))$an_transdisciplinary_goals.rating.mean

cfa_pred_df.rating_mean$novelty.rating.mean <- ddply(cfa_pred_df, "domain", summarise, novelty.rating.mean=mean(novelty))$novelty.rating.mean
cfa_pred_df.rating_mean$outputs_outcomes.rating.mean <- ddply(cfa_pred_df, "domain", summarise, outputs_outcomes.rating.mean=mean(outputs_outcomes))$outputs_outcomes.rating.mean
# Overlaid histograms with means
so_dist <- ggplot(cfa_pred_df, aes(x=solution_orientation, fill=domain)) +
  geom_histogram( alpha=0.9, position="identity") +
  scale_fill_manual(
     values = c("#FC4E07", "#E7B800", "#00AFBB")
     , name = "Domain"
     ) + 
  ## geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(
    data = cfa_pred_df.rating_mean
    , aes(
      xintercept = solution_orientation.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("Solution Orientation")

trinv_dist <- ggplot(
  cfa_pred_df
  , aes(
    x = an_transdisciplinary_inv
    , fill=domain
    )
  ) +
  geom_histogram( 
    alpha=0.9
    , position="identity"
    ) +
  scale_fill_manual(
     values = c("#FC4E07", "#E7B800", "#00AFBB")
     , name = "Domain"
     ) + 
  geom_vline(
    data = cfa_pred_df.rating_mean
    , aes(
      xintercept = an_transdisciplinary_inv.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("Transdisciplinary Involvement")



trgoals_dist <- ggplot(
  cfa_pred_df
  , aes(
    x = an_transdisciplinary_goals
    , fill=domain
    )
  ) +
  geom_histogram( 
    alpha=0.9
    , position="identity"
    ) +
  scale_fill_manual(
     values = c("#FC4E07", "#E7B800", "#00AFBB")
     , name = "Domain"
     ) + 
  geom_vline(
    data = cfa_pred_df.rating_mean
    , aes(
      xintercept = an_transdisciplinary_goals.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  scale_x_continuous(breaks = seq(0, 10, 2)) + 
  xlab("Transdisciplinary Goals")




novelty_dist <- ggplot(
  cfa_pred_df
  , aes(
    x = novelty
    , fill=domain
    )
  ) +
  geom_histogram( 
    alpha=0.9
    , position="identity"
    ) +
  scale_fill_manual(
     values = c("#FC4E07", "#E7B800", "#00AFBB")
     , name = "Domain"
     ) + 
  geom_vline(
    data = cfa_pred_df.rating_mean
    , aes(
      xintercept = novelty.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("Novelty")



out_dist <- ggplot(
  cfa_pred_df
  , aes(
    x = outputs_outcomes
    , fill=domain
    )
  ) +
  geom_histogram( 
    alpha=0.9
    , position="identity"
    ) +
  scale_fill_manual(
     values = c("#FC4E07", "#E7B800", "#00AFBB")
     , name = "Domain"
     ) + 
  geom_vline(
    data = cfa_pred_df.rating_mean
    , aes(
      xintercept = outputs_outcomes.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("Outputs & Outcomes")



library(ggpubr)
factor_domain.plot <- ggarrange(
  so_dist
  , trinv_dist + rremove("ylab")
  , trgoals_dist + rremove("ylab")
  , novelty_dist
  , out_dist + rremove("ylab")
  , common.legend = TRUE
  , legend = "top"
  ) + 
  labs(caption = "(Pauloo, et al. 2017)")
ggsave("../SIVOCS/utku/V8_Valuation of SNSF funded research through social innovation_utku_v3/factor_domain.svg", factor_domain.plot)





