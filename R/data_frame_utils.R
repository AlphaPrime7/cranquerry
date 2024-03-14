#' Title
#'
#' @param zpath
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
#' read_gzips(getwd())
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

#' Title
#'
#' @import readr
#' @import tidyr
#' @import data.table
#' @import tidytable
#' @param path
#' @param method
#'
#' @return
#' @export
#'
#' @examples
#' cdf = condense_gzs(getwd())
condense_gzs = function(path, method = c(1,2)){
  if (1 %in% method){
    df <-
      list.files(path = path, pattern = "*.gz") %>%
      map_df(~read_csv(.))
    return(df)
  } else {
    df <-
      list.files(path = getwd() , pattern = "*.gz") %>%
      map_df(~fread(.))
    return(df)
  }
}

#' Title
#' @import dplyr
#' @param pkgs
#' @param zpath
#' @param prefix
#' @param files
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' beta = bind_gz_dfs("tidyDenovix",getwd())
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
  pkg_df <- tibble::tibble()
  for(i in 1:length(ls)){
    df = as.data.frame(.globals$get(ls[i])) %>%
      dplyr::filter(package %in% pkgs)
    pkg_df <- bind_rows(pkg_df, df)
  }

  #cleanup
  remove_gzs(zpath)
  return(pkg_df)

}

#' Title
#'
#' @param comb_df
#' @param pkg_vect
#' @param multiple
#'
#' @return
#' @export
#'
#' @examples
#' beta = filter_pkg(cdf,c('tidyDenovix','normfluodbf','tidyAML'))
#' beta = filter_pkg(cdf,c('tidyAML'))
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

