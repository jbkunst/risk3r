plot_glm <- function(fit) {

  plot(fit, label = TRUE)

  fit$lambda == cvfit$lambda.min

  dm <- as.matrix(fit$beta)

  dm <- as_tibble(dm) %>%
    mutate(variable = rownames(dm)) %>%
    gather(L1_Norm, coefficient, -variable) %>%
    mutate(
      L1_Norm = str_remove_all(L1_Norm, "s"),
      L1_Norm = as.numeric(L1_Norm)
    )

  dmlast <- dm %>%
    filter(L1_Norm == max(L1_Norm))

  ggplot(dm) +
    geom_line(aes(L1_Norm, coefficient, group = variable, color = variable)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = dmlast$coefficient, labels = dmlast$variable)
      )

}
