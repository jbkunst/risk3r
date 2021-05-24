# # check no monotonic  -----------------------------------------------------
# data(germancredit, package = "scorecard")
#
# bins <- woebin2(germancredit, y = "creditability", method = "tree")
#
# bin_summary <- woebin_summary(bins)
# bin_summary
#
# dplyr::glimpse(bin_summary)
#
# dplyr::filter(bin_summary, !factor, !monotone)
#
# vars <- c("credit.amount", "age.in.years", "present.residence.since")
#
# bins <- bins[vars]
#
# scorecard::woebin_plot(bins)
#
# bin_summary <- woebin_summary(bins)
# bin_summary
#
#
# #  monotonic cuts ---------------------------------------------------------
# library(mob)
#
# bin_result <- mob::batch_bin(
#   as.numeric(germancredit$creditability == "bad"),
#   germancredit[, vars],
#   method = 3
#   )
#
# bin_summary
#
# bin_result$bin_sum
#
# bin_result$bin_out$credit.amount
# bin_result$bin_out$age.in.years
# bin_result$bin_out$present.residence.since
#
#
# brks <- bin_result$bin_out %>%
#   map(pluck, "cut")
#
# bins2 <- woebin2(
#   germancredit,
#   y = "creditability",
#   x = vars,
#   breaks_list = brks
#   )
#
# woebin_summary(bins)
# woebin_summary(bins2)
#
# scorecard::woebin_plot(bins2)
