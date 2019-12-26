#'
#' Get rules from a ctree object
#'
#' @param x a ctree object
#'
#' @examples
#'
#' irisct <- partykit::ctree(Species ~ .,data = iris)
#'
#' ct_rules(irisct)
#'
#' @importFrom utils getFromNamespace
#' @export
ct_rules <- function(x){

  lrp <- utils::getFromNamespace(".list.rules.party", "partykit")

  rules <- lrp(x)

  out <- data.frame(
    node = as.numeric(names(rules)),
    rule = as.vector(rules),
    stringsAsFactors = FALSE
  )

  out

}
