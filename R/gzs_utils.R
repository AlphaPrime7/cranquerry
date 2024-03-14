#' Title
#'
#' @param zpath
#'
#' @return
#' @export
#'
#' @examples
#' gzs_present(getwd())
gzs_present = function(zpath){

  if (length(list.files(path = zpath, pattern = "\\.gz$", full.names = T)) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Title
#'
#' @param pathstring
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
#' get_gzips(getwd(),fn=T)
get_gzips = function(pathstring, fn = F){

  if(fn){
    if(length(list.files(path = pathstring, pattern = "\\.gz$", full.names = T)) > 0) {
      files_list <- list.files(path = pathstring, pattern = "\\.gz$", full.names = T)
      return(files_list)
    } else{
      cranstats_warn_msg("No .gz files in dir")
      ev = c()
      return(ev)
    }

  } else {
    if(length(list.files(path = pathstring, pattern = "\\.gz$", full.names = F)) > 0) {
      files_list <- list.files(path = pathstring, pattern = "\\.gz$", full.names = F)
      return(files_list)
    } else{
      ev = c()
      cranstats_warn_msg("No .gz files in dir")
      return(ev)
    }

  }

}

#' Title
#'
#' @param zpath
#'
#' @return
#' @export
#'
#' @examples
#' remove_gzs(getwd())
remove_gzs = function(zpath){
  gzs = get_gzips(zpath)
  for(i in gzs){
    file.remove(i)
  }
}

#' Title
#'
#' @param zpath
#' @param file_names
#'
#' @return
#' @export
#'
#' @examples
#' .memoise(getwd(), dates)
.memoise = function(zpath, file_names){
  dir_files = as.Date(tools::file_path_sans_ext(as.Date(get_gzips(zpath)),TRUE))
  not_in_wd = as.Date(setdiff(file_names, dir_files), origin = "1970-01-01")
  if(length(not_in_wd) == 0 || length(dir_files) < length(file_names) ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}




