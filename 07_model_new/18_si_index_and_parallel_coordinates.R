source("./07_model_new/17_create_an_si_index.R")

library("dplyr")
library("ggplot2")
library("alluvial")
library("ggforce")
library("ggalluvial")
library("ggparallel")
library(GGally)

# -> split factors into intervalls in fitted df
den <- cfa_pred_df[, 1:5]
split3 <- function(var) {
  var <- ifelse(var <= 3, 0,
           ifelse(var < 7 , 1, 2))
  return(var)
}

# -> Create a basis for the SI-Indec by calculating the rowSums of the factors
den <- as.data.frame(sapply(den, split3))
den$sum <- as.vector(rowSums(den))

# -> Introduce the raw index and place it with the other factors
cfa_pred_df$den_ind <- factor(split3(den$sum))
colnames(cfa_pred_df)
cfa_pred_df  <- cfa_pred_df[, c(1:5, 13 , 6:12)]

class(den)

# Parallel coordinates didn't work well
# -> Try alluvial:
#-------------------------------------------------------------------------------

# -> Define graph colors (darker colors for SI-Index)
A_col <- "darkorchid1"
A_col2 <- "darkorchid3"
B_col <- "darkorange1"
B_col2 <- "darkorange3"
C_col <- "skyblue1"
C_col2 <- "skyblue3"
alpha <- 0.7 # transparency value

si_index <- den$solution_orientation + 
  den$an_transdisciplinary_inv*0.5 + 
  den$an_transdisciplinary_goals*0.5 + 
  den$novelty +
  den$outputs_outcomes

# If there are more than 1 zero in a line, si_index is set to be a 0
for (i in seq_len(nrow(den))) {
  k = 2
  for (j in seq_len(ncol(den))) {
    if (k == 0) {
     si_index[i] = 0 
     print(i)
    } else if (den[i, j] == 0) {
      k = k - 1
    }
  }
}

den <- as.data.frame(cbind(den, si_index))
den$si_index <- scales::rescale(den$si_index, c(1,10))
den$si_index <- split3(den$si_index) 

den$solution_orientation <- factor(den$solution_orientation)
den$an_transdisciplinary_inv <- factor(den$an_transdisciplinary_inv)
den$an_transdisciplinary_goals <- factor(den$an_transdisciplinary_goals)
den$novelty <- factor(den$novelty)
den$outputs_outcomes <- factor(den$outputs_outcomes)
den$si_index <- factor(den$si_index)

levels(den$solution_orientation) <- c("0 - 3\n(32%)", "4 - 6\n(57%)", "7 - 10\n(11%)")
levels(den$an_transdisciplinary_inv) <- c("0 - 3\n(48%)", "4 - 6\n(46%)", "7 - 10\n(6%)")
levels(den$an_transdisciplinary_goals) <- c("0 - 3\n(35%)", "4 - 6\n(56%)", "7 - 10\n(9%)")
levels(den$novelty) <- c("0 - 3\n(35%)", "4 - 6\n(58%)", "7 - 10\n(7%)")
levels(den$outputs_outcomes) <- c("0 - 3\n(31%)", "4 - 6\n(59%)", "7 - 10\n(10%)")
levels(den$si_index) <- c("0 - 3\n(39%)", "4 - 6\n(51%)", "7 - 10\n(10%)")
table(den$si_index)/361
  
den_plot <- ggparallel(list(
  "solution_orientation"
  , "an_transdisciplinary_inv"
  , "an_transdisciplinary_goals"
  , "novelty"
  ,"outputs_outcomes"
  , "si_index"
  ), data = den
  , width = 0.3
  , alpha = alpha
  , order = 0
  , label.size = 7,
  , text.angle=0
  ) +
  scale_fill_manual(values  = c(A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col, A_col2, B_col2, C_col2, A_col, B_col, C_col)) +
  scale_color_manual(values  = c(A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col, A_col2, B_col2, C_col2, A_col, B_col, C_col)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold", angle = 45, hjust=1)
  ) + scale_x_discrete(labels= c("Sol.-Orientation", "Transdisc. Involvement", "Transdisc. Goals", "Novelty", "Outcomes", "SI-Index"))

ggsave(
  den_plot
  , device="svg"
  , filename = "../SIVOCS/utku/V8_Valuation of SNSF funded research through social innovation_utku_v3/si_alluvial.svg"
  , dpi = 900
  , width = 16
  , height = 10
  )


# -> Create a domain distribution plot
table(cfa_pred_df$si_index)
cfa_pred_df$si_index <- scales::rescale(si_index, c(0,10))
cfa_pred_df.rating_mean$si_index.rating.mean <- ddply(
  cfa_pred_df
  , "domain"
  , summarise
  , si_index.rating.mean = mean(si_index))$si_index.rating.mean

si_dist <- ggplot(
  cfa_pred_df
  , aes(
    x = si_index
    , fill=domain
    )
  ) +
  geom_density( 
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
      xintercept = si_index.rating.mean)
      ,  colour = c("#FC4E07", "#E7B800", "#00AFBB")
    , linetype="dashed", size=1)+
  theme_light() +
  theme(legend.position="top") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  xlab("SI-Index")
si_dist

sum(7 <=cfa_pred_df$si_index)

cor(cfa_pred_df$si_index, feat_df.num_o$contribToSI.rate.)
## anova(cfa_pred_df$si_index, feat_df.num_o$contribToSI.rate.)

cfa_pred_df.rating_mean





mean(cfa_pred_df$si_index - feat_df.num$contribToSI.rate., na.rm = T)
sum(!is.na(feat_df.num$contribToSI.rate.))

class(cfa_pred_df$si_index)

index_contrib_df <- as.data.frame(
  cbind(
  si_index = cfa_pred_df$si_index,
  contribToSI.rate. = feat_df.num$contribToSI.rate.
  #, domain = cfa_pred_df$domain
  )
)

likert_recode
summary(feat_df.num$contribToSI.rate.)

index_contrib_df$contribToSI.rate. <- likert_recode(index_contrib_df$contribToSI.rate.)
col_seq <- c("#f4a582",
             "#92c5de",
             "#2166ac")

index_contrib_cor <- cor(index_contrib_df$si_index, feat_df.num$contribToSI.rate., method = "spearman", use = "pairwise.complete.obs")
index_contrib_df %>% 
  filter(!is.na(contribToSI.rate.)) %>%
  gather(key="contribToSI.rate.", value="si_index") %>%
  ggplot( aes(x=si_index, y=contribToSI.rate., fill = contribToSI.rate.)) +
  geom_boxplot() +
  ylab(" ") +
  scale_x_continuous(breaks = seq(0,10,2)) +
  theme_light() + 
  theme(axis.text.y=element_blank()) +
  xlab("SI-Index") +
  scale_fill_manual(values=col_seq, name="Contribution to SI (self-assessment)")+
  guides(fill = guide_legend(reverse = TRUE)) + 
  annotate("text", x = 1, y = 3.5, label = paste0("Cor. (Spearman's rho): ", round(index_contrib_cor, 2)))










shapiro.test(as.numeric(cfa_pred_df$si_index - feat_df.num$contribToSI.rate.))
t.test( feat_df.num$contribToSI.rate., cfa_pred_df$si_index,paired = TRUE, alternative = "two.sided")

mean( feat_df.num_o$contribToSI.rate.)

kruskal.test(cfa_pred_df$si_index ~ feat_df.num$contribToSI.rate.)
wilcox.test(cfa_pred_df$si_index[!is.na(feat_df.num$contribToSI.rate.)], as.numeric(feat_df.num$contribToSI.rate.)[!is.na(feat_df.num$contribToSI.rate.)],
                                   mu = 0,
                                   alt = "two.sided",
                                   conf.int = T,
                                   conf.level = 0.95,
                                   paired = F,
                                   correct = T,
                                   p.adjust="bonferroni" )


