#' Apply consecutive filter expressions to a data frame and return results
#'
#' @param data A data frame.
#' @param filters A list of named quoted expressions.
#' @param summary_fun A summary function. Recive a data frame and return a data
#'   frame.
#' @param verbose A logical value
#'
#' @examples
#'
#' require(rlang) # summarise, n_distinct, n
#' require(dplyr) # quo
#'
#' data <- mtcars
#'
#' filters <- list(
#'   `am equals to 1`  = quo(am == 1),
#'   `carb = 1!!`      = quo(carb == 1),
#'   `hp less than 90` = quo(hp < 90)
#' )
#'
#' summary_fun <- function(data) {
#'   summarise(
#'     data,
#'     n = dplyr::n(),
#'     cyl_mean = mean(cyl),
#'     n_unique_cyl = n_distinct(cyl)
#'     )
#' }
#'
#' results <- apply_filters(data, filters, summary_fun)
#'
#' results
#'
#' @export
apply_filters <- function(data,
                          filters,
                          summary_fun = function(data) { dplyr::summarise(data) },
                          verbose = TRUE){

  daux <- data

  filters <- append(list("initial" = rlang::quo(TRUE)), filters)
  filters <- append(filters, list("final" = rlang::quo(TRUE)))

  filters_names <- names(filters)

  res <- purrr::map_df(seq_along(filters_names), function(i = 3){

    fnm  <- filters_names[i]
    fltr <- filters[[i]]

    as.character(fltr)
    as_label(fltr)

    if(verbose) {
      t1 <- stringr::str_c("Step ", i, " `", fnm, "` ==> ", sep = "")
      t2 <- stringr::str_c(rlang::as_label(fltr), collapse = " ")
      cli::cli_alert_info(stringr::str_c(t1, t2))
    }

    daux <<- dplyr::filter(daux, !!fltr)

    summary_fun(daux) |>
      mutate(filter = fnm, rows = nrow(daux), .before = 1)

  })

  res <- res |>
    mutate(rows_removed = c(NA, -diff(.data[["rows"]])), .after = .data[["rows"]])

  list(data = daux, summary = res)

}


