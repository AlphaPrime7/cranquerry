#' Get R packages
#'
#' @author Tingwei Adeck
#'
#' @import tools
#'
#' @return A data frame of R packages from the CRAN server
#'
#' @export
#'
#' @note This is the simplest approach to getting R packages from the CRAN site.
#' This function
#'
#' @family cranstats
#'
#' @examples
#' db = getpkgsv1()

getpkgsv1 = function(){
  db <- tools::CRAN_package_db()[, c("Package", "Description")]
  return(db)
}

#' Get R packages
#'
#' @author Tingwei Adeck
#'
#' @return A vector of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#' db = getpkgsv2()

getpkgsv2 = function() {

  #contrib.url(getOption("repos")["CRAN"], "source") - contains special words like repos
  description <- sprintf("%s/web/packages/packages.rds", getOption("repos", "https://cran.rstudio.com/")["CRAN"])

  con <- if(substring(description, 1L, 7L) == "file://"){
    file(description, "rb")
  } else {
    url(description, "rb")
  }
  on.exit(close(con))
  db <- readRDS(gzcon(con, level = 6, allowNonCompressed = TRUE))
  rownames(db) <- NULL

  db = db[, c("Package", "Title")]
  db = as.data.frame(db)
  return(as.vector(db))

}

#' Get R Packages
#'
#' @author Tingwei Adeck
#'
#' @import stringr
#' @import rvest
#'
#' @return A vector of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#'
#' db = getpkgsv3()

getpkgsv3 = function(){

  url = 'https://cloud.r-project.org/web/packages/available_packages_by_name.html'

  webpage = read_html(url)
  head(html_attr(html_nodes(webpage, "a"), "href"),26) #turns out with some formatting the packages can be extracted
  pkg_links = as.data.frame(html_attr(html_nodes(webpage, "a"), "href")) #action tag links
  pkg_links = pkg_links[27:nrow(pkg_links),] %>% str_extract('(?<=packages/).*(?=/index.html)') #.* represents the extract
  pkg_links = as.data.frame(pkg_links)

  return(as.vector(pkg_links$pkg_links))
}

#' Get R Packages
#'
#' @author Tingwei Adeck
#'
#' @return A vector of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#'
#' db = getpkgsv4()

getpkgsv4 = function(){

  url = 'https://cloud.r-project.org/web/packages/available_packages_by_name.html'

  dirty_pkgs = paste(readLines(url), collapse="\n")
  dirty_pkgs = str_match_all(dirty_pkgs, "<a href=\"(.*?)\"")
  dirty_pkgs = as.data.frame(dirty_pkgs)
  pkg_links = dirty_pkgs[2]

  pkg_links = pkg_links[27:nrow(pkg_links),] %>% str_extract('(?<=packages/).*(?=/index.html)') #.* represents the extract
  pkg_links = as.data.frame(pkg_links)

  return(as.vector(pkg_links$pkg_links))
}

#' Get R Packages
#'
#' @author Tingwei Adeck
#'
#' @return A vector of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#' db = getpkgsv5()

getpkgsv5 = function(){

  url = 'https://cloud.r-project.org/web/packages/available_packages_by_name.html'

  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'tr td') #nodes or css selectors are table rows and table data cell

  pkgs <- html_nodes(webpage,'td:nth-child(1)') %>% html_text(trim=TRUE)
  pkgs = as.data.frame(pkgs)
  pkgs <- pkgs[!apply(is.na(pkgs) | pkgs == "", 1, all),]

  desc <- html_nodes(webpage,'td:nth-child(2)') %>% html_text(trim=TRUE)
  desc <- desc[lengths(desc) > 0 & desc != ""]

  if(length(pkgs) == length(desc)){
    df <- data.frame(pkgs, desc, row.names=NULL)
    colnames(df) <- c("PackageName", "Description")
  }
  as.vector(df$PackageName)

}

#' Get R Packages
#'
#' @author Tingwei Adeck
#'
#' @return A vector of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#' db = getpkgsv6()

getpkgsv6 <- function(){

  url = "http://cran.r-project.org/web/packages/packages.rds"

  mytemp <- tempfile();
  download.file(url, mytemp);
  mydata <- as.data.frame(readRDS(mytemp), row.names=NA);
  mydata$Published <- as.Date(mydata[["Published"]]);

  mydata <- mydata[order(mydata$Published),c("Package", "Version", "Published")]
  return(as.vector(mydata$Package))
}

#' List R Dev platforms
#'
#' @author Tingwei Adeck
#'
#' @import httr
#' @import jsonlite
#'
#' @return A data frame of packages on CRAN.
#'
#' @export
#'
#' @family cranstats
#'
#' @examples
#' db = getpkgsv7()

getpkgsv7 = function(){

  pkgs = httr::GET("http://crandb.r-pkg.org/-/desc", content_type("application/octet-stream"))
  pkgs = names(jsonlite::fromJSON(rawToChar(pkgs$content)))
  #pkgs = names(httr::content(pkgs))
  return(pkgs)
}


#' Download GZIP files
#'
#' @author Tingwei Adeck
#'
#' @import utils
#' @import data.table
#'
#' @return Check pwd for downloaded GZIP files.
#'
#' @export
#'
#' @examples
#' download_all_data("normfluodbf", range_start = 194)
#' download_all_data("normfluodbf", range_start = 194, multi.core = F)

download_all_data = function(package, range_start = 0, multi.core = TRUE){

  if (.Platform$OS.type == "windows") cores <- 1L else cores <- detectCores()
  #if (.Platform$OS.type == "unix") cores <- detectCores() else cores <- 1L

  data = data.table::fread('http://cran-logs.rstudio.com/', fill = TRUE)
  start <- get_initial_release_date(package) + range_start
  today <- last_avail_date()

  all_days <- seq(start, today, by = 'day')
  year <- as.POSIXlt(all_days)$year + 1900

  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')
  #urls <- .GlobalEnv$urls #more correct as below
  if(!("urls" %in% ls(environment())) ) assign("urls", urls, envir = environment())

  if(multi.core){
    cl = parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(), varlist = c("urls"))
    filenames <- parallel::parLapply(cl, urls,
                                     function(x){stringr::str_match(x,'\\d+-\\d+-\\d+')})
    parallel::stopCluster(cl)

  } else {
    file_names = parallel::mclapply(urls, function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')}, mc.cores = cores )
  }

  for(i in 1:length(urls)){
    #curl::curl_fetch_multi(urls)
    n = paste0(package,"_") #package should be all, package is just a reference point for the timeline
    utils::download.file(
      url = urls[i],
      mode = "w", #wb
      method = "wget", #auto
      destfile = paste0(n, file_names[i]))
  }

}

#' Package download logs
#'
#' @author Tingwei Adeck
#'
#'
#' @return Downloads log per package provided for a specified date from the present.
#'
#' @export
#'
#' @examples
#' pkg_logs(c("normfluodbf","tidyDenovix"), 5)$tidyDenovix

pkg_logs = function(package, days_from_today){

  if (!curl::has_internet()) stop("Check internet connection.", call. = FALSE)

  date_range = c(Sys.Date() - days_from_today, Sys.Date())
  date_range

  log_list = c()
  for (i in date_range) {
    pd = packageLog(c(package), date = i)
    log_list = c(log_list, pd)
  }
  log_list

}



