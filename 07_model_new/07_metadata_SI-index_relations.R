source("./07_model_new/06_multinomial_logit_regression.R")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(ggpubr)

summary(df_indexes$si_index.ord_weight)
df_indexes$si_index_3 <-  ifelse(
  df_indexes$si_index.ord_weight > 7, 3,
  ifelse(
    df_indexes$si_index.ord_weight > 4, 2,
    ifelse(df_indexes > 1, 1, 0)
    
    )
  )


df_meta_indexes <- as.data.frame(cbind(df_indexes, meta_df))


si_domain_0 <- df_meta_indexes %>%
  dplyr::select(si_index_3, domain) %>%
  dplyr::group_by(domain, si_0 = si_index_3 == 0) %>%
  dplyr::summarise(si_index_3 = n()) %>%
  filter(si_0 == T) %>%
  as.data.frame()



si_domain_1 <- df_meta_indexes %>%
  dplyr::select(si_index_3, domain) %>%
  dplyr::group_by(domain, si_1 = si_index_3 == 1) %>%
  dplyr::summarise(si_index_3 = n()) %>%
  filter(si_1 == T) %>%
  as.data.frame()


si_domain_2 <- df_meta_indexes %>%
  dplyr::select(si_index_3, domain) %>%
  dplyr::group_by(domain, si_2 = si_index_3 == 2) %>%
  dplyr::summarise(si_index_3 = n()) %>%
  filter(si_2 == T) %>%
  as.data.frame()

si_domain_3 <- df_meta_indexes %>%
  dplyr::select(si_index_3, domain) %>%
  dplyr::group_by(domain, si_3 = si_index_3 == 3) %>%
  dplyr::summarise(si_index_3 = n()) %>%
  filter(si_3 == T) %>%
  as.data.frame()



df_meta_indexes %>% 
  select(si_index.ord_weight, domain) %>%
  group_by(si_index.ord_weight) %>%
  summarise(n())

as.data.frame(table(df_meta_indexes$si_index_3, df_meta_indexes$domain ))



data <- df_meta_indexes
si_domain.df <-data.frame(si_index = data$si_index.ord_weight,
                         domain =  data$domain)
str(si_domain.df)
## si_domain.df$familiarWithSI <- as.numeric(b1domain.df$familiarWithSI)
si_domain.df %>% 
  gather(key="domain", value="si_index") %>%
  ggplot( aes(x=si_index, y=domain, fill=domain),  show.legend = FALSE) +
    geom_boxplot() +
  ylab(" ")+
  theme_light() + 
  theme(axis.text.y=element_blank()) + 
  xlab("SI-Index") + scale_fill_manual(values=c("#FC4E07", "#E7B800", "#00AFBB"), name="Domain") + 
  scale_x_continuous(breaks = seq(0,10,2))  


df_meta_indexes$self_assessment <- feat_df.num_o$contribToSI.rate.

lm_scatter(
  df_meta_indexes
  , x = df_meta_indexes$si_index.ord_weight
  , y = df_meta_indexes$self_assessment
  , laby="SI-Rating (Self Assessment)"
  , labx = "SI-Index"
  ) 

si_domain_freq <- rbind(
si_domain_0[, c(1,3)]
, si_domain_1[, c(1,3)]
, si_domain_2[, c(1,3)]
, si_domain_3[, c(1,3)]

)

si_domain_freq$si_index_group <- rep(0:3, each = 3)

ggplot(si_domain_freq, aes(x = si_index_group, y = si_index_3, fill = domain)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#FC4E07", "#E7B800", "#00AFBB"), name="Domain")






