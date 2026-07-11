# Keep `woebin_summary()` aligned with the monotonic binning helpers.
#
# This definition is loaded after `helpers-scorecard.R` and replaces the former
# local strict-sign check. Flat steps are valid, while missing and special-value
# bins are excluded through `woebin_monotonic_bin_is_monotone()`.
woebin_summary <- function(bins, sort = TRUE) {
  is_bin_factor_character <- function(bin) {
    all(stringr::str_detect(bin, "[a-zA-Z]"))
  }

  dbiv <- dplyr::bind_rows(bins)
  dbiv <- dplyr::as_tibble(dbiv)
  dbiv <- dplyr::group_by(dbiv, .data$variable)

  dbiv <- dplyr::summarize(
    dbiv,
    n_categories = dplyr::n(),
    iv = unique(.data$total_iv),
    hhi = hhi(.data$count_distr),
    count_distr_max = max(.data$count_distr),
    count_distr_min = min(.data$count_distr),
    has_missing = any(.data$bin == "missing"),
    has_special_values = any(.data$is_special_values),
    factor = is_bin_factor_character(.data$bin),
    distribution = inline_hist_aux(.data$count),
    breaks = list(.data$breaks)
  )

  dks <- purrr::map_dbl(bins, bin_ks)
  dks <- tibble::tibble(variable = names(dks), ks = as.vector(dks))
  dbiv <- dplyr::left_join(dbiv, dks, by = "variable")

  dmn <- purrr::map_lgl(bins, woebin_monotonic_bin_is_monotone)
  dmn <- tibble::tibble(variable = names(dmn), monotone = as.vector(dmn))
  dbiv <- dplyr::left_join(dbiv, dmn, by = "variable")

  dbiv <- dplyr::mutate(
    dbiv,
    iv_lbl = iv_label(.data$iv),
    hhi_lbl = hhi_label(.data$hhi)
  )

  dbiv <- dplyr::relocate(dbiv, .data$distribution, .after = dplyr::last_col())
  dbiv <- dplyr::relocate(dbiv, .data$ks, .after = .data$iv)
  dbiv <- dplyr::relocate(dbiv, .data$monotone, .before = .data$factor)

  if (sort) dbiv <- dplyr::arrange(dbiv, dplyr::desc(.data$iv))

  dbiv
}
