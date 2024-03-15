#' Downloading Package Names
#' @author ap7
#' @name getpkgs
NULL
#> NULL

#' @rdname getpkgs
#' @order 1
#' @import tools
#' @return a data frame of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv1()
#' }
getpkgsv1 = function(){
  db <- tools::CRAN_package_db()[, c("Package", "Description")]
  return(db)
}

#' @rdname getpkgs
#' @order 2
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv2()
#' }
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

#' @rdname getpkgs
#' @order 3
#' @import stringr
#' @import rvest
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv3()
#' }

getpkgsv3 = function(){

  url = .globals$get("version_3_4")

  webpage = read_html(url)
  head(html_attr(html_nodes(webpage, "a"), "href"),26) #turns out with some formatting the packages can be extracted
  pkg_links = as.data.frame(html_attr(html_nodes(webpage, "a"), "href")) #action tag links
  pkg_links = pkg_links[27:nrow(pkg_links),] %>% str_extract('(?<=packages/).*(?=/index.html)') #.* represents the extract
  pkg_links = as.data.frame(pkg_links)

  return(as.vector(pkg_links$pkg_links))
}

#' @rdname getpkgs
#' @order 4
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @seealso \code{\link[cranstats]{getpkgsv3}}\cr
#' @examples
#' \dontrun{
#' db = getpkgsv4()
#' }

getpkgsv4 = function(){

  url = .globals$get("version_3_4")

  dirty_pkgs = paste(readLines(url), collapse="\n")
  dirty_pkgs = str_match_all(dirty_pkgs, "<a href=\"(.*?)\"")
  dirty_pkgs = as.data.frame(dirty_pkgs)
  pkg_links = dirty_pkgs[2]

  pkg_links = pkg_links[27:nrow(pkg_links),] %>% str_extract('(?<=packages/).*(?=/index.html)') #.* represents the extract
  pkg_links = as.data.frame(pkg_links)

  return(as.vector(pkg_links$pkg_links))
}

#' @rdname getpkgs
#' @order 5
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv5()
#' }

getpkgsv5 = function(){

  url = .globals$get("version_5")

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

#' @rdname getpkgs
#' @order 6
#' @import utils
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv6()
#' }

getpkgsv6 <- function(){

  url = .globals$get("version_6")

  mytemp <- tempfile();
  utils::download.file(url, mytemp);
  mydata <- as.data.frame(readRDS(mytemp), row.names=NA);
  mydata$Published <- as.Date(mydata[["Published"]]);

  mydata <- mydata[order(mydata$Published),c("Package", "Version", "Published")]
  return(as.vector(mydata$Package))
}

#' @rdname getpkgs
#' @order 7
#' @import httr
#' @import jsonlite
#' @return A vector of cran pkgs
#' @family cranpkgs
#' @export
#' @examples
#' \dontrun{
#' db = getpkgsv7()
#' }
getpkgsv7 = function(){

  pkgs = httr::GET(.globals$get("version_7"), content_type("application/octet-stream"))
  pkgs = names(jsonlite::fromJSON(rawToChar(pkgs$content)))
  #pkgs = names(httr::content(pkgs))
  return(pkgs)
}
