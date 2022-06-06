# library(tidyverse)
#
# data <- mtcars
#
# filters <- list(
#   `am igual a 1`   = quo(am == 1),
#   `carb = 1 `      = quo(carb == 1),
#   `hp < 90`        = quo(hp < 90)
# )
#
# summary_fun <- function(data) {
#
#   summarise(data, n = dplyr::n(), cyl_mean = mean(cyl))
#
# }
#
#
# apply_filters <- function(data, filters, summary_fun, verbose = TRUE){
#
#   daux <- data
#
#   filters <- append(list("total" = quo(TRUE)), filters)
#   filters <- append(filters, list("final" = quo(TRUE)))
#
#   filters_names <- names(filters)
#
#   res <- map_df(seq_along(filters_names), function(i = 1){
#
#     # message(i)
#
#
#     fnm <- filters_names[i]
#
#     if(verbose) message(fnm)
#
#     # txt <- names(f)
#
#     daux <<- filter(daux, !!filters[[i]])
#
#     summary_fun(daux) |>
#       mutate(filter = fnm, .before = 1)
#
#   })
#
#   list(data = daux, summary = res)
#
# }
#
# apply_filters(data, filters, summary_fun)
#
