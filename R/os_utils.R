#' List R Dev platforms
#'
#' @author Tingwei Adeck
#'
#' @import XML
#' @import janitor
#'
#' @return A data frame showing os platforms and the number of packages by platform.
#'
#' @export
#'
#' @examples
#' dbos = get_platform_stats()

get_platform_stats = function(){

  URL <- "http://cran.r-project.org/web/checks/check_summary.html#summary_by_package"
  relos = readHTMLTable(URL)
  relos = as.data.frame(relos)
  relos = janitor::clean_names(relos)

  returns_list <- list("OS_flavor"=as.vector(relos$null_flavor),
                       "ok"=as.vector(relos$null_ok),
                       "error" = as.vector(relos$null_error),
                       "total"=as.vector(relos$null_total))

  return(as.data.frame(returns_list))
}
