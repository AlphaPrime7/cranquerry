#' Data Available Date
#'
#' @author Tingwei Adeck
#'
#' @import packageRank
#'
#' @return
#'
#' @export
#'
#' @family dates
#'
#' @examples
#' date = last_avail_date()

last_avail_date = function(){

  #str_extract(logInfo(show.available = TRUE)$`Available log/result`, pattern='\\d+-\\d+-\\d+')
  #gsub(".*[(]|[)].*", "", logInfo(show.available = TRUE)$`Available log/result`)
  if(packageRank::logInfo(Sys.timezone(), upload.time = "17:00")$`Today's results on 'cranlogs'?` == 'Yes.'){
    logdate = stringr::str_extract_all(logInfo(show.available = TRUE)$`Available log/result`,
                                       pattern='\\d+-\\d+-\\d+')

    logdates = c()
    for (i in logdate) {
      logdates = c(logdates, as.Date(i))
    }
    logdate = min(logdates)
    return(as.Date(logdate, origin = "1970-01-01"))

  } else if(packageRank::logInfo(Sys.timezone(), upload.time = "17:00")$`Today's results on 'cranlogs'?` == 'No.'){
    logdate = stringr::str_extract_all(logInfo(show.available = TRUE)$`Available log/result`,
                                       pattern='\\d+-\\d+-\\d+')

    logdates = c()
    for (i in logdate) {
      logdates = c(logdates, as.Date(i))
    }
    logdate = min(logdates)
    return(as.Date(logdate, origin = "1970-01-01"))

  } else {
    return(Sys.Date()-2)
  }

}

#' Initial Release Date
#'
#' @author Tingwei Adeck
#'
#' @return
#'
#' @export
#'
#' @family dates
#'
#' @examples
#' ird = get_initial_release_date('normfluodbf')

get_initial_release_date = function(packages){
  min_date = Sys.Date() - 1

  for (pkg in packages)
  {
    pkg_data = httr::GET(paste0("http://crandb.r-pkg.org/", pkg, "/all"))
    pkg_data <- jsonlite::fromJSON(rawToChar(pkg_data$content))

    initial_release = pkg_data$timeline[[1]]
    min_date = min(min_date, as.Date(initial_release))
  }

  min_date

}



