source("./07_model_new/17_create_an_si_index.R")

 # install.packages("GGally")
library(GGally)

# -> greated intervalls in fitted df
den <- cfa_pred_df[, 1:5]
split3 <- function(var) {
  var <- ifelse(var <= 3, 0,
           ifelse(var <= 6, 1, 2))
  return(var)
}

split4 <- function(var) {
  var <- ifelse(var < 3, 0,
           ifelse(var < 7, 1, 2))
  return(var)
}

den <- as.data.frame(sapply(den, split3))
den$sum <- as.vector(rowSums(den))

cfa_pred_df$den_ind <- factor(split3(den$sum))
cfa_pred_df  <- cfa_pred_df[, c(1:5, 8 , 6:7, 9:12)]

## ggparcoord(data = cfa_pred_df,
##            columns = 1:6,
##            groupColumn = "den_ind",
##            showPoints = TRUE,
##            scale = "globalminmax"
##            
##            ) +
##   scale_color_brewer(palette = "Set2") +
##   theme_light()


class(den)

# Parallel coordinates didn't work well
# -> Try alluvial:
#-------------------------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("alluvial")
## devtools::install_github('thomasp85/ggforce')
library("ggforce")
## devtools::install_github("corybrunson/ggalluvial")
library("ggalluvial")
library("ggparallel")

A_col <- "darkorchid1"
A_col2 <- "darkorchid3"
# A_col <- "#F87474"
# A_col <- "#d73027"
# A_col2 <- "#a50026"
B_col <- "darkorange1"
B_col2 <- "darkorange3"
# B_col <- "#FFB562"
# B_col <- "#fdae61"
# B_col2 <- "#f46d43"
C_col <- "skyblue1"
C_col2 <- "skyblue3"
# C_col <- "#3AB0FF"
# C_col <- "#4575b4"
# C_col2 <- "#313695"
alpha <- 0.7 # transparency value
fct_levels <- c("A","C","B")

dat_raw <- data.frame(Tested  = sample(c("A","B","C"),100,
                                       replace = TRUE,prob=c(0.2,0.6,0.25)),
                      Modeled = sample(c("A","B","C"),100,
                                       replace = TRUE,prob=c(0.56,0.22,0.85)),
                      stringsAsFactors = FALSE)
dat <- dat_raw %>%
  group_by(Tested,Modeled) %>%
  dplyr::summarise(freq = n()) %>%
  ungroup()
dat_raw2 <- dat_raw %>%
  mutate_at(vars(Modeled, Tested), 
            funs(factor(., levels = fct_levels)))

dat_raw2$test2 <- dat_raw2$Tested
levels(dat_raw2$test2) <- c("A 25%", "C 35%", "B 60%")

ggparallel(list("Modeled", "Tested", "test2"), data = dat_raw2, 
           alpha = alpha
           , order = 0
           , label.size = 6,
           , text.angle=0) +
  scale_fill_manual(values  = c(A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col)) +
  scale_color_manual(values  = c(A_col, B_col, C_col, A_col, B_col, C_col, A_col, B_col, C_col)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 18, face = "bold")
  )


si_index <- den$solution_orientation + 
  den$an_transdisciplinary_inv*0.5 + 
  den$an_transdisciplinary_goals*0.5 + 
  den$novelty +
  den$outputs_outcomes

den <- den[, 1:5]
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
levels(den$solution_orientation) <- c("0 - 2\n(32%)", "3 - 6\n(57%)", "7 - 10\n(11%)")
levels(den$an_transdisciplinary_inv) <- c("0 - 2\n(48%)", "3 - 6\n(46%)", "7 - 10\n(6%)")
levels(den$an_transdisciplinary_goals) <- c("0 - 2\n(35%)", "3 - 6\n(56%)", "7 - 10\n(9%)")
levels(den$novelty) <- c("0 - 2\n(35%)", "3 - 6\n(58%)", "7 - 10\n(7%)")
levels(den$outputs_outcomes) <- c("0 - 2\n(31%)", "3 - 6\n(59%)", "7 - 10\n(10%)")
levels(den$si_index) <- c("0 - 2\n(39%)", "3 - 6\n(51%)", "7 - 10\n(10%)")
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

ggsave(den_plot, device="svg", filename = "../SIVOCS/utku/V8_Valuation of SNSF funded research through social innovation_utku_v3/si_alluvial.svg")


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
anova(cfa_pred_df$si_index, feat_df.num_o$contribToSI.rate.)


