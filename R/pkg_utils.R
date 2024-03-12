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
#' @import parallel
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
    filenames <- parallel::parLapply(
      cl,
      urls,
      function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')})
    parallel::stopCluster(cl)

  } else {
    file_names = parallel::mclapply(
      urls,
      function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')},
      mc.cores = cores)
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


#MORE FUNCTIONS
download_all_data = function(package, range_start = 0, multi.core = TRUE){
  library(parallel)
  library(packageRank,lib.loc = '/usr/local/lib/R/site-library')
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
    filenames <- parallel::parLapply(
      cl,
      urls,
      function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')})
    parallel::stopCluster(cl)

  } else {
    file_names = parallel::mclapply(
      urls,
      function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')},
      mc.cores = cores)
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
download_all_data("normfluodbf", range_start = 195,multi.core = F)

pkg_data = function(package, days_after_release = 0, multi.core = TRUE){
  library(parallel)

  if (.Platform$OS.type == "windows") cores <- 1L else cores <- detectCores()
  #if (.Platform$OS.type == "unix") cores <- detectCores() else cores <- 1L

  #data = data.table::fread('http://cran-logs.rstudio.com/', fill = TRUE)
  start <- get_initial_release_date(package) + days_after_release
  date_avail = last_avail_date()
  today_min_two = Sys.Date() - 2

  all_days <- seq(start, date_avail, by = 'day')
  #missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), TRUE))
  year <- as.POSIXlt(all_days)$year + 1900

  base_url <- "http://cran-logs.rstudio.com/"
  urls <- paste0(base_url, year, '/', all_days, '.csv.gz')

  if(!("urls" %in% ls(environment())) ) assign("urls", urls, envir = environment())
  if(!("all_days" %in% ls(environment())) ) assign("all_days", urls, envir = environment())
  #urls <- .GlobalEnv$urls #more correct as below

  if(multi.core){
    cl = parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(), varlist = c("urls"))
    downloads <- parallel::parLapply(
      cl,
      urls,
      function(url){
        file_names = stringr::str_match(url,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')
        anum = as.character(stringr::str_match(file_names,'\\d{4}'))
        url = paste0(base_url, anum, "/", file_names)
        n = paste0(package,"_")
        dfile = paste0(n, file_names)

        utils::download.file(
          url = url,
          mode = "w", #wb
          method = "wget", #auto
          destfile = paste0(n, file_names))
      })

    parallel::stopCluster(cl)

  } else {
    file_names = parallel::mclapply(
      urls,
      function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')},
      mc.cores = cores)

    for(i in 1:length(urls)){
      #curl::curl_fetch_multi(urls)
      n = paste0(package,"_") #package should be all, package is just a reference point for the timeline
      utils::download.file(
        url = urls[i],
        mode = "w", #wb
        method = "wget", #auto
        destfile = paste0(n, file_names[i]))

      downloads <- read.csv(file_names[i])
      file.remove(file_names[i])
    }

    return(NULL)
  }

}

pkg_data("tidyDenovix",days_after_release = 50, multi.core = T)

.pkg_globals_store <- function() {
  .store <- environment()
  list(
    get = function(y) .store[[y]],
    set = function(y, v) .store[[y]] <- v
  )
}

get_gzips = function(pathstring){
  if(length(list.files(path = pathstring, pattern = "\\.gz$", full.names = F)) > 0) {
    files_list <- list.files(path = pathstring, pattern = "\\.gz$", full.names = F)
    return(files_list)
  } else{
    normfluodbf_warn_msg("No .gz files in dir")
  }
}
get_gzips(getwd())

read_gzips = function(package, zpath){

  gz_files = get_gzips(zpath) #assuming functions here are used solely for this purpose
  gz_dfs <- lapply(gz_files, readr::read_csv)

  n <- paste0(package, "_gz")
  for(i in 1:length(gz_dfs)){
    n <- n
    assign(paste0(n, i), as.data.frame(gz_dfs[i]), envir = parent.frame())
    .globals$set(paste0(n, i),as.data.frame(gz_dfs[i]))
  }
}
read_gzips('tidyDenovix', getwd())

condense_gzs = function(path, method = c(1,2)){
  library(readr, lib.loc = '/usr/local/lib/R/site-library')
  #library(tidyverse, lib.loc = '/usr/local/lib/R/site-library')
  library(tidyr, lib.loc = '/usr/local/lib/R/site-library')
  library(data.table, lib.loc = '/usr/local/lib/R/site-library')
  library(readr, lib.loc = '/usr/local/lib/R/site-library')
  library(tidytable)
  if (1 %in% method){
    df <-
      #@import readr
      list.files(path = path, pattern = "*.gz") %>%
      map_df(~read_csv(.))
    return(df)
  } else {
    df <-
      #@import data.table & tidytable
      list.files(path = getwd() , pattern = "*.gz") %>%
      map_df(~fread(.))
    return(df)
  }
}

filter_pkg = function(comb_df, pkg_vect, .use = 'dplyr'){

  if(is.null(.use) || .use == 'base'){
    message('Limited to a single package')
    pkg_df = comb_df[package == package]
    return(pkg_df)
  } else {
    message('Multiple pkgs can be filtered')
    pkg_df = comb_df %>%
      filter(package %in% pkg_vect)
    return(pkg_df)
  }

}

test = filter_pkg(cdf,c('tidyDenovix','normfluodbf','tidyAML'))

bind_gz_dfs = function(pkg, zpath, files = NULL, ...){
  #read_gzips must be ran before this procedure can be ran
  gzs = get_gzips(zpath)

  #act_env = as.vector(environment())
  #gzfiles = stringr::str_match(act_env,'\\w+_\\w{2}\\d+') #failed
  if(is.null(files)) files = gzs else files = files

  ls = c()
  for(i in 1:length(files)){
    files = sprintf("%s%s", pkg, paste0("_gz",i))
    ls = c(ls, files)
  }

  read_gzips(package=pkg,zpath=zpath)

  pkg_df <- tibble()
  for(i in 1:length(ls)){
    df = as.data.frame(.globals$get(ls[i])) %>%
      dplyr::filter(package == pkg)
    pkg_df <- bind_rows(pkg_df, df)
  }

  remove_gzs(zpath)
  return(pkg_df)

}
bind_gz_dfs("tidyDenovix",getwd())

remove_gzs = function(zpath){
  gzs = get_gzips(zpath)
  for(i in gzs){
    file.remove(i)
  }
}
remove_gzs(getwd())


#IMPROVEMENTS
.download_all_data = function(package, zpath, days_after_release = 0, multi.core = TRUE, memoise = T){
  library(parallel)
  library(packageRank,lib.loc = '/usr/local/lib/R/site-library')

  #data = data.table::fread('http://cran-logs.rstudio.com/', fill = TRUE)
  start <- get_initial_release_date(package) + days_after_release
  today <- last_avail_date()
  all_days <- seq(as.Date(start), today, by = 'day')

  #memoise setup
  dir_files = as.Date(tools::file_path_sans_ext(as.Date(get_gzips(zpath)),TRUE))
  not_in_wd = as.Date(setdiff(all_days, dir_files), origin = "1970-01-01")

  #---------------
  if(memoise){
    if(length(not_in_wd) == 0){
      stop('All files already downloaded')
    } else {
      all_days = not_in_wd
    }
    all_days

  } else {
    all_days = as.Date(all_days, origin = "1970-01-01")
  }
  #---------------

  year <- as.POSIXlt(all_days)$year + 1900
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

  if(!("all_days" %in% ls(environment())) ) assign("all_days", urls, envir = environment())
  if(!("urls" %in% ls(environment())) ) assign("urls", urls, envir = environment())

  if(length(not_in_wd) == 0 ){
    message('check folder for files already there')

  } else {
    if(multi.core){
      cl = parallel::makeCluster(cores)
      parallel::clusterExport(cl = cl, envir = environment(), varlist = c("urls"))
      filenames <- parallel::parLapply(
        cl,
        urls,
        function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')})
      parallel::stopCluster(cl)

    } else {
      file_names = parallel::mclapply(
        urls,
        function(x){stringr::str_match(x,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')},
        mc.cores = cores)
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

}

mdownload_all_data <- memoise::memoise(.download_all_data)
download_all_data <- .download_all_data
download_all_data("normfluodbf", zpath = getwd(), days_after_release = 195,multi.core = F)

.memoise = function(zpath, file_names){
  dir_files = as.Date(tools::file_path_sans_ext(as.Date(get_gzips(zpath)),TRUE))
  not_in_wd = as.Date(setdiff(file_names, dir_files), origin = "1970-01-01")
  if(length(not_in_wd) == 0 || length(dir_files) < length(file_names) ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
.memoise(getwd(), dates)

download_gzs = function(packages = NULL, days_after_release = 0, multi.core = TRUE, use.future = F){
  library(parallel)
  library(packageRank,lib.loc = '/usr/local/lib/R/site-library')
  library(doFuture, quietly = TRUE)

  #manage cores or threads
  if (.Platform$OS.type == "unix") cores <- detectCores() else cores <- detectCores()

  #date_utils
  start <- get_initial_release_date(packages) + days_after_release
  date_avail = last_avail_date()
  today_min_two = Sys.Date() - 2 #two days from current is safe

  #days
  all_days <- seq(start, date_avail, by = 'day')
  year <- as.POSIXlt(all_days)$year + 1900

  #construct url
  base_url <- "http://cran-logs.rstudio.com/"
  urls <- paste0(base_url, year, '/', all_days, '.csv.gz')

  #file_names = all_days #future pkg
  file_names = lapply(
    urls,
    function(url){stringr::str_match(url,'\\d+-\\d+-\\d+')} )

  if(!("urls" %in% ls(environment())) ) assign("urls", urls, envir = environment())
  if(!("all_days" %in% ls(environment())) ) assign("all_days", urls, envir = environment())

  if(multi.core && .Platform$OS.type == "windows" && use.future == F){
    cl = parallel::makeCluster(cores)
    parallel::clusterExport(cl = cl, envir = environment(), varlist = c("urls", "all_days"))
    downloads <- parallel::parLapply(
      cl,
      urls,
      function(url){
        file_names = stringr::str_match(url,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')
        anum = as.character(stringr::str_match(file_names,'\\d{4}'))
        url = paste0(base_url, anum, "/", file_names)
        dfile = file_names #here for the remove_gzs function

        if (RCurl::url.exists(url)){
          utils::download.file(
            url = url,
            mode = "w", #wb
            method = "wget", #auto
            destfile = file_names)
        } else {
          stop(sprintf("%s does not exist",url), call. = FALSE)
        }

      })
    parallel::stopCluster(cl)
    dmsg = sprintf("Downloaded using Multithreading on %s", .Platform$OS.type)
    return(dmsg)

  } else if(multi.core && .Platform$OS.type == "unix" && use.future == F){
    file_names = parallel::mclapply(
      urls,
      function(url){
        file_names = stringr::str_match(url,'\\d+-\\d+-\\d+\\.\\w+\\.\\w+')
        anum = as.character(stringr::str_match(file_names,'\\d{4}'))
        url = paste0(base_url, anum, "/", file_names)
        dfile = file_names #here for the remove_gzs function

        if (RCurl::url.exists(url)){
          utils::download.file(
            url = url,
            mode = "w", #wb
            method = "wget", #auto
            destfile = file_names)
        } else {
          stop(sprintf("%s does not exist",url), call. = FALSE)
        }
      },
      mc.cores = cores)
    dmsg = sprintf("Downloaded using Multithreading on %s", .Platform$OS.type)
    return(dmsg)

  } else if(multi.core && use.future){
    plan(multisession, workers = parallel::detectCores())
    foreach::foreach(i = 1:length(urls), .combine = c,
                     .options.future = list(seed = TRUE)) %dofuture% {
                       n = (".csv.gz")
                       utils::download.file(
                         url = urls[i],
                         mode = "w", #wb
                         method = "wget", #auto
                         destfile = paste0(file_names[i],n) )
                     }
    dmsg = sprintf("Downloaded multithreadedly using the future pkg on %s", .Platform$OS.type)
    return(dmsg)

  }  else if(!multi.core){
    for(i in 1:length(urls)){
      #curl::curl_fetch_multi(urls)
      n = (".csv.gz")
      utils::download.file(
        url = urls[i],
        mode = "w", #wb
        method = "wget", #auto
        destfile = paste0(file_names[i],n) )
    }
    dmsg = sprintf("Downloaded sequentially on %s; using multi.core is advisable", .Platform$OS.type)
    return(dmsg)

  }

}

download_gzs("tidyDenovix",days_after_release = 51, multi.core = T, use.future = F)

#special store
.pkg_globals_store <- function() {
  .store <- environment()
  list(
    get = function(y) .store[[y]],
    set = function(y, v) .store[[y]] <- v
  )
}

#get gzips
get_gzips = function(pathstring, fn = F){

  if(fn){
    if(length(list.files(path = pathstring, pattern = "\\.gz$", full.names = T)) > 0) {
      files_list <- list.files(path = pathstring, pattern = "\\.gz$", full.names = T)
      return(files_list)
    } else{
      normfluodbf_warn_msg("No .gz files in dir")
    }

  } else {
    if(length(list.files(path = pathstring, pattern = "\\.gz$", full.names = F)) > 0) {
      files_list <- list.files(path = pathstring, pattern = "\\.gz$", full.names = F)
      return(files_list)
    } else{
      normfluodbf_warn_msg("No .gz files in dir")
    }

  }

}
get_gzips(getwd(),fn=T)

#read gzips
read_gzips = function(zpath, prefix = "head"){

  prefix = prefix

  gz_files = get_gzips(zpath) #assuming functions here are used solely for this purpose
  gz_dfs <- lapply(gz_files, readr::read_csv)

  n <- paste0(prefix, "_gz")
  for(i in 1:length(gz_dfs)){
    n <- n
    .globals$set(paste0(n, i),as.data.frame(gz_dfs[i]))
    assign(paste0(n, i), as.data.frame(gz_dfs[i]), envir = parent.frame())
  }
}
read_gzips(getwd())

condense_gzs = function(path, method = c(1,2)){
  library(readr, lib.loc = '/usr/local/lib/R/site-library')
  #library(tidyverse, lib.loc = '/usr/local/lib/R/site-library')
  library(tidyr, lib.loc = '/usr/local/lib/R/site-library')
  library(data.table, lib.loc = '/usr/local/lib/R/site-library')
  library(readr, lib.loc = '/usr/local/lib/R/site-library')
  library(tidytable)
  if (1 %in% method){
    df <-
      #@import readr
      list.files(path = path, pattern = "*.gz") %>%
      map_df(~read_csv(.))
    return(df)
  } else {
    df <-
      #@import data.table & tidytable
      list.files(path = getwd() , pattern = "*.gz") %>%
      map_df(~fread(.))
    return(df)
  }
}
cdf = condense_gzs(getwd())

filter_pkg = function(comb_df, pkg_vect, multiple = T){

  if(is.null(multiple) || !multiple || length(pkg_vect) == 1){
    message('Limited to a single package')
    pkg_df = comb_df[package == pkg_vect]
    return(pkg_df)
  } else {
    pkg_df = comb_df %>%
      filter(package %in% pkg_vect)
    message(sprintf('Multiple packages filtered using %s', "dplyr"))
    return(pkg_df)
  }

}

beta = filter_pkg(cdf,c('tidyDenovix','normfluodbf','tidyAML'))
beta = filter_pkg(cdf,c('tidyAML'))

bind_gz_dfs = function(pkgs, zpath, prefix = "head", files = NULL, ...){

  gzs = get_gzips(zpath)

  #act_env = as.vector(environment())
  #gzfiles = stringr::str_match(act_env,'\\w+_\\w{2}\\d+') #fail
  if(is.null(files)) files = gzs else files = files

  ls = c()
  for(i in 1:length(files)){
    files = sprintf("%s%s", prefix, paste0("_gz",i))
    ls = c(ls, files)
  }

  read_gzips(zpath=zpath, prefix = prefix)
  pkg_df <- tibble()
  for(i in 1:length(ls)){
    df = as.data.frame(.globals$get(ls[i])) %>%
      dplyr::filter(package %in% pkgs)
    pkg_df <- bind_rows(pkg_df, df)
  }

  #cleanup
  remove_gzs(zpath)
  return(pkg_df)

}
beta = bind_gz_dfs("tidyDenovix",getwd())

remove_gzs = function(zpath){
  gzs = get_gzips(zpath)
  for(i in gzs){
    file.remove(i)
  }
}
#utility function
remove_gzs(getwd())


