#' PWD
#' @return
#' @export
#' @examples
#' get_wd()
get_wd = function(){
  if(.Platform$OS.type == "unix")
    system2("pwd", stdout = TRUE)
  if(.Platform$OS.type == "windows")
      shell("cd", intern=T, mustWork = T)
}

#' HOME
#' @return
#' @export
#' @note useful for getting the user account
#' @examples
#' get_home()
get_home = function(){
  if(.Platform$OS.type == "unix")
    home = system2("echo", c("$HOME"), stdout = TRUE)
  return(home)
  if(.Platform$OS.type == "windows")
    home = shell("echo %homepath%", intern=T, mustWork = T)
  return(normalizePath(home))
}

#' SETWD
#' @return
#' @export
#' @note A little easier than setwd.
#' @examples
#' set_wd("Documents","Active","cranStats")
set_wd = function(parent=NULL, child1=NULL, ...){
  child2 = as.character(...)
  if(.Platform$OS.type == "unix")
    setwd(paste0(get_home(),"/",parent,"/",child1, "/", child2))

  if(.Platform$OS.type == "windows")
    setwd(paste0(get_home(),"\\",parent,"\\",child1, "\\",child2))
}

#' RHOME
#' @return
#' @export
#' @examples
#' r_home()
r_home = function(){
  return(R.home())
}

#' EDIT HOME RPROFILE
#' @return
#' @export
#' @examples
#' edit_home_rprofile()
edit_home_rprofile = function(){
  file.edit(file.path("~", ".Rprofile"))
  help("Rprofile")
}

#' EDIT HOME Renviron
#' @return
#' @export
#' @examples
#' edit_renviron()
edit_renviron = function(){
  file.edit(file.path("~", ".Renviron"))
  help("Renviron")
}
