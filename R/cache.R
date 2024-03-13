.pkg_globals_cache <- function() {
  .cache <- environment()
  list(
    get = function(y) .cache[[y]],
    set = function(y, v) .cache[[y]] <- v
  )
}
