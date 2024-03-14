#' Warning
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranquerry_warn_msg('sucks')
cranquerry_warn_msg <- function(x) {
  warning(sprintf("cranquerry: %s", x), call. = F)
}

#' Message
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranquerry_msg('sucks')
cranquerry_msg <- function(x) {
  message(sprintf("cranquerry: %s", x), call. = F)
}

#' Stop
#' @family comms
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' cranquerry_stop('sucks')
cranquerry_stop <- function(x) {
  message(sprintf("cranquerry: %s", x), call. = F)
}

#' @rdname mc
#' @family comms
#'
#' @return
#' @export
#'
#' @examples
#' multicore_msg()
multicore_msg <- function() {
  message(sprintf("cranquerry: Downloaded using Multithreading on %s", .Platform$OS.type), call. = F)
}

#' @rdname mc
#' @family comms
#'
#' @return
#' @export
#'
#' @examples
#' multicore_future_msg()
multicore_future_msg <- function() {
  message(sprintf("cranquerry: Downloaded multithreadedly using the doFuture pkg on %s", .Platform$OS.type), call. = F)
}

#' sc
#' @family comms
#'
#' @return
#' @export
#'
#' @examples
#' singlecore_msg()
singlecore_msg <- function() {
  message(sprintf("cranquerry: Downloaded sequentially on %s; using multi.core is advisable", .Platform$OS.type), call. = F)
}


