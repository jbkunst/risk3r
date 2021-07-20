library(risk3r)
library(tidyverse)
library(patchwork)

theme_set(kunstomverse::theme_knst_calibri())

# modelo ------------------------------------------------------------------
data("credit_woe")

dtrain <- head(credit_woe, 20000)
dtest  <- tail(credit_woe, 20000)

m <- glm(bad ~ ., family = binomial, data = dtrain)
m <- featsel_stepforward(m, scale = 5, trace = 1)

# ggs ---------------------------------------------------------------------
p1 <- gg_model_coef(m, color = "darkred")
p1

p2 <- gg_model_importance(m)
p2

p3 <- gg_model_corr(m)
p3

p4 <- gg_model_vif(m)
p4

p5 <- gg_model_roc(m, newdata = dtest, size = 2) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, direction = -1, option = "B")
p5

p6 <- gg_model_ks(m, newdata = dtest, size = 2) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, direction = -1, option = "B")
p6

# solo train
p6 <- p6 +
  ggforce::facet_wrap_paginate(ggplot2::vars(.data$sample), nrow = 1, ncol = 1, page = 1) +
  theme(strip.background = element_blank(), strip.text.x = element_blank())
p6


# compose -----------------------------------------------------------------
(p1 | p2 | p3) /
  (p4 | p5 | p6)

p1 <- p1 + labs(caption = NULL)

p2 <- p2 + scale_x_discrete(breaks = "", name = NULL, limits = rev) + coord_flip()

p3 <- p3 +
  scale_fill_viridis_c(
    name = "Correlación",
    limits = c(-1, 1),
    label = scales::percent,
    # begin = 0.2,
    # end   = 0.9,
    na.value = "gray90",
    breaks = seq(-1, 1, length.out = 5),
    option = "B"
  ) +
  geom_text(aes(label = round(cor * 100)), color = "white", size = 2.5) +
  scale_x_discrete(breaks = "", name = NULL) +
  scale_y_discrete(breaks = "", name = NULL) +
  theme(legend.position = "right")

p4 <- p4 + scale_x_discrete(limits = rev) + coord_flip()

(p1 | p2 | p3) /
  (p4 | p5 | p6)


# variables ---------------------------------------------------------------
data(woes)

gg_woes(woes) %>%
  reduce(`+`)



