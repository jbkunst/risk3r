library(scorecard)
library(risk3r)

data("germancredit")

dt    <- var_filter(germancredit, "creditability")
bins  <- woebin(germancredit, "creditability", no_cores = 1)
dtwoe <- woebin_ply(dt, bins)
model <- glm(creditability ~ ., family = binomial(), data = dtwoe)


model_metrics(model)
model_metrics(model, newdata = head(dtwoe, 200))



# feature selection -------------------------------------------------------
model_fsstep <- featsel_stepforward(model, trace = FALSE)

broom::tidy(model_fsstep)

model_metrics(model_fsstep)
model_metrics(model_fsstep, newdata = head(dtwoe, 200)



model_fsglmnet <- featsel_glmnet(model)

broom::tidy(model_fsglmnet)

model_metrics(model_fsglmnet)
model_metrics(model_fsglmnet, newdata = head(dtwoe, 200))


dfmetrics <- model_partials(model_fsglmnet, newdata = head(dtwoe, 200))
dfmetrics


risk3r:::plot.model_partials(dfmetrics) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))




