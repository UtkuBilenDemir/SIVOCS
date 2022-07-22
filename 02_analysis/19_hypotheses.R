source("./02_analysis/02_static_responses.R")

# ---------- B1
# The familiarity with the concept of SI depends on the field of research. The one one is familiar with the concept of SI, the higher is the propensity to contribute to SI.
# --- B1 depends on DOMAIN

shapiro.test(data$familiarWithSI.response.)


b1domain.df <-data.frame(familiarWithSI = data$familiarWithSI.response.,
                         domain =  data$domain)

b1domain.df$familiarWithSI <- as.numeric(b1domain.df$familiarWithSI)
b1domain.df %>% 
  gather(key="domain", value="familiarWithSI") %>%
  ggplot( aes(x=familiarWithSI, y=domain, fill=domain),  show.legend = FALSE) +
    geom_boxplot() +
  ylab(" ")+
  theme_light() + 
  theme(axis.text.y=element_blank())+ scale_fill_discrete(name = "Domain") + xlab("Familiarity with SI") + scale_fill_manual(values=c("#FC4E07", "#E7B800", "#00AFBB"), name="Domain") + 
  scale_x_continuous(breaks = seq(0,10,2))  


