#' Function to calculate the vector psi given 2 vector of counts
#'
#' @param old A vector of original count distribution
#' @param new A vector with the new count distribution
#' @return A vector with psi index (value) a label and the table for with counts, percents, woe.
#' @examples
#'
#' o <- sample(letters[1:5], size = 9000, prob = 1:5, replace = TRUE)
#' n <- sample(letters[1:5], size = 1000, prob = 1:5 + 4, replace = TRUE)
#'
#' table(o)
#' table(n)
#'
#' psi_vector(
#'   table(o),
#'   table(n)
#' )
#'
#' p <- psi_vector(
#'   table(o),
#'   table(n)
#' )
#'
#' p
#'
#' sum(p)
#'
#' psi_label(sum(p))
#'
#' @export
psi_vector <- function(old, new) {

  new <- new/sum(new)

  old <- old/sum(old)

  out <- (new - old) * log( new / old )

  out

}

#' Function to calculate the vector table given 2 vector of counts
#'
#' @param old A vector of original distribution
#' @param new A vector with the new distribution
#' @return A table with psi index (value) a label and the table for with counts, percents, woe.
#' @examples
#'
#' o <- factor(sample(letters[1:5], size = 9000, prob = 1:5, replace = TRUE))
#' n <- factor(sample(letters[1:5], size = 1000, prob = 1:5 + 4, replace = TRUE))
#'
#' psi_table(o, n)
#'
#' @importFrom dplyr full_join mutate
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
psi_table <- function(old, new) {

  # old <- o; new <- n

  d <- dplyr::full_join(
    dplyr::count(tibble::tibble("category" = old), .data$category, name = "count_old"),
    dplyr::count(tibble::tibble("category" = new), .data$category, name = "count_new"),
    by = "category"
  )

  d <- dplyr::mutate(
    d,
    percent_old = .data$count_old/sum(.data$count_old),
    percent_new = .data$count_new/sum(.data$count_new),
    psi = psi_vector(.data$count_old, .data$count_new)
    )

  d

}


#'
#' Get labels for PSIs
#'
#' @param x A numeric vector
#'
#' @examples
#'
#' psi_label(c(0.2, 2))
#'
#' @export
psi_label <- function(x) {
  cut(
    x,
    c(0, 0.1, 0.25, Inf),
    include.lowest = TRUE,
    # c("Cambio insignificante", "Algún cambio menor", "Gran cambio en población")
    labels = c("insignificant change", "some minor change", "major shift in population")
  )
}
