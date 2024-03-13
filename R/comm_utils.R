#' Warning
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranstats_warn_msg('sucks')
cranstats_warn_msg <- function(x) {
  warning(sprintf("cranstats: %s = ", x), call. = F)
}

#' Message
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranstats_msg('sucks')
cranstats_warn_msg <- function(x) {
  message(sprintf("cranstats: %s = ", x), call. = F)
}

#' Stop
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranstats_stop('sucks')
cranstats_stop <- function(x) {
  message(sprintf("cranstats: %s = ", x), call. = F)
}
