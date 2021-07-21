library(scorecard)
library(risk3r)

data("germancredit")

dt    <- var_filter(germancredit, "creditability")
bins  <- woebin(germancredit, "creditability")
dtwoe <- woebin_ply(dt, bins)
model <- glm(creditability ~ ., family = binomial(), data = dtwoe)


model_metrics(model)
model_metrics(model, newdata = head(dtwoe, 200))

# feature selection stepwise forward --------------------------------------
model_fsstep <- featsel_stepforward(model, trace = FALSE)
broom::tidy(model_fsstep)

model_metrics(model_fsstep)
model_metrics(model_fsstep, newdata = head(dtwoe, 200))


# feature selection glmnet ------------------------------------------------
model_fsglmnet <- featsel_glmnet(model)

broom::tidy(model_fsglmnet)

model_metrics(model_fsglmnet)
model_metrics(model_fsglmnet, newdata = head(dtwoe, 200))


# partial models ----------------------------------------------------------
dfmetrics <- model_partials(model_fsglmnet, newdata = head(dtwoe, 200))
dfmetrics

plot(dfmetrics)


library(magrittr)

dfmetrics %>%
  dplyr::select(-gini, -iv) %>%
  plot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
