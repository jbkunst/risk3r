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
#' require(rlang) # quo
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
#' apply_filters(data, filters, summary_fun)
#'
#' @export
apply_filters <- function(data, filters, summary_fun, verbose = TRUE){

  daux <- data

  filters <- append(list("total" = rlang::quo(TRUE)), filters)
  filters <- append(filters, list("final" = rlang::quo(TRUE)))

  filters_names <- names(filters)

  res <- purrr::map_df(seq_along(filters_names), function(i = 1){

    fnm <- filters_names[i]

    if(verbose) {
      cli::cli_alert_info(
        stringr::str_c(fnm, stringr::str_c(filters[[i]], collapse = " "))
        )
    }

    # if(verbose) message(fnm)

    # txt <- names(f)

    daux <<- dplyr::filter(daux, !!filters[[i]])

    summary_fun(daux) |>
      mutate(filter = fnm, .before = 1)

  })

  list(data = daux, summary = res)

}


