#' Function to calculate the vector psi given 2 vector of counts
#'
#' @param old A vector of original distribution
#' @param new A vector with the new distribution
#' @return A list with psi index (value) a label and the table for with counts, percents, woe.
#' @examples
#'
#' o <- sample(letters[1:5], size = 2000, prob = 1:5, replace = TRUE)
#' n <- sample(letters[1:5], size = 1000, prob = 1:5 + 4, replace = TRUE)
#'
#' table(o)
#' table(n)
#'
#' psi(
#'   table(o),
#'   table(n)
#' )
#'
#' p <- psi(
#'   table(o),
#'   table(n)
#' )
#'
#' psi_label(sum(p))
#'
#' @export
psi <- function(new, old) {

  new <- new/sum(new)
  old <- old/sum(old)

  out <- (new - old) * log( new / old )

  out

}
