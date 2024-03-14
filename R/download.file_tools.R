#' Download CRAN logs
#'
#' @import utils
#' @import data.table
#' @import parallel
#' @import packageRank
#' @importFrom memoise memoise
#'
#' @param package
#' @param zpath
#' @param days_after_release
#' @param memoise
#' @param ...
#'
#' @return
#' @export
#'
#' @note Base template for multithread version.
#' @keywords memoise
#' @examples
#' \dontrun{
#' mdownload_logs('tidyDenovix', zpath = getwd(), days_after_release = 48, memoise = T)
#' mdownload_logs(zpath = getwd(), days_after_release = 0, "2024-03-07") #fails
#' }
mdownload_logs = function(package, zpath, days_after_release = 0, memoise = T, ...){

  #--------(limit dates)
  alt_start = as.character(...)
  today <- last_avail_date()
  if(is.null(package)){
    start = as.Date(alt_start) + days_after_release
  } else {
    start <- get_initial_release_date(package) + days_after_release
  }
  if( start > Sys.Date()-2 || start > today) warning(sprintf('%s > latest logs:rectify days after or date', start), call. = F)

  #------(sequence dates)
  all_days <- seq(as.Date(start), today, by = 'day')
  #all_days <- seq(start_date, by = "day", length.out = days_after_release)

  #------(memoise setup)
  if(gzs_present(zpath)){
    dir_files = as.Date(tools::file_path_sans_ext(as.Date(get_gzips(zpath)),TRUE))
  } else {
    dir_files = c()
  }
  not_in_wd = as.Date(setdiff(all_days, dir_files), origin = "1970-01-01")

  #---------(memoise execution)
  if(memoise){
    all_days = not_in_wd
  } else {
    all_days = as.Date(all_days, origin = "1970-01-01")
  }

  #---------(urls)
  year <- as.POSIXlt(all_days)$year + 1900
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

  #---------(file names)
  file_names <- lapply(
    urls,
    function(x){stringr::str_match(x,'\\d+-\\d+-\\d+')})

  #---------(main with memoise)
  if(length(not_in_wd) == 0 || is.null(not_in_wd)){
    cranstats_warn_msg('No files to download: All files in directory')

  } else {
    for(i in 1:length(urls)){
      if (RCurl::url.exists(urls[i]) ){
        utils::download.file(
          url = urls[i],
          mode = "w", #wb
          method = "wget", #auto
          destfile = paste0(zpath,"/",file_names[i], ".csv.gz"))
      }
      else {
        stop(sprintf("%s does not exist",i), call. = FALSE)
      }
    }
  }
}

download_logs <- memoise::memoise(mdownload_logs) #more to learn here


#optimized -more work to do here
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

