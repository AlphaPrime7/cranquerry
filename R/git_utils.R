git_commit = function(local_user, msg){

  #-----(check git-basic)
  if(system2("git", c('version')) != 0 || system('git --version') != 0 || system2("git", c('version')) == 127 )
    cranquerry_warn_msg("git is not installed") else cranquerry_msg('git is installed:proceed')

  #-----(usr options)
  usr_options = c("goob", "i dont know", "f*** the world", system2("whoami",
                                                stdout = TRUE,
                                                stderr = TRUE))

  #-----(menu items)
  lusr = utils::menu(c("goob", "i dont know",system2("whoami",
                                                     stdout = TRUE,
                                                     stderr = TRUE)), title="Confirm your system account?")
  lusr = as.numeric(lusr)

  #-----(main)
  if( local_user != usr_options[lusr] || local_user != usr_options[lusr] || local_user != Sys.info()["user"])
    stop(sprintf("check your username; expecting %s", Sys.info()["user"]))



    system2("git", args = c("commit", "-a", "-m", msg))


}

#' Title
#' @rdname git
#' @family git
#'
#' @import R.utils
#' @param log_path
#'
#' @return
#' @export
#'
#' @examples
#' get_noncommits_log(getwd())
get_noncommits_log = function(log_path){
  #----(log path)
  log_path = file.path(log_path,"log")

  #----(log dir)
  if(isTRUE(dir.exists(log_path))){
    #-----(make log file)
    git_ncs = file.path(log_path,"git_noncommits.txt")
    writeLines("", git_ncs) #file.create(git_ncs)

    #-----(open connection)
    con <- file(git_ncs, "a")

    #-----(get infidels or noncommits)
    infidels = system("git status --short", intern = T)

    #-----(main??)
    if(countLines(git_ncs)[1] > 1){
      cat(infidels, file = con, sep = "\n", append = T)
      cat( as.character(Sys.time()) , file = con, sep = "\n", append = T)
    } else {
      cat(infidels, file = con, sep = "\n")
      cat(as.character(Sys.time()) , file = con, sep = "\n", append = T)
    }

    close(con)

  } else {
    log_dir = dir.create(log_path)

    git_ncs = file.path(log_path,"git_noncommits.txt")
    writeLines("", git_ncs) #file.create(git_ncs)

    #-----(open connection)
    con <- file(git_ncs, "a")

    #-----(get infidels or noncommits)
    infidels = system("git status --short", intern = T)

    #-----(main??)
    if(countLines(git_ncs)[1] > 1){
      cat(infidels, file = con, sep = "\n", append = T)
      cat( as.character(Sys.time()) , file = con, sep = "\n", append = T)
    } else {
      cat(infidels, file = con, sep = "\n")
      cat(as.character(Sys.time()) , file = con, sep = "\n", append = T)
    }

    close(con)

  }

  print(infidels)
  return(infidels)
  message(sprintf("Check %s for a log of infidels :)", git_ncs ))

}


#' Stage
#' @rdname git
#' @family git
#'
#' @return
#' @export
#'
#' @note
#' With a fun twist, progress bar gimmick added.
#' @examples
#' git_stage
git_stage = function(){
  pb = tkProgressBar(title = "Commit progress bar", label = "Commit progress bar",
                     min = 0, max = 1, initial = 0, width = 300)
  Sys.sleep(0.5)
  inf_index = system("git status --short", intern = T)
  infidels_ls = system2("git", c('status', '--short'))

  for(i in seq(inf_index)) {
    j = (i/length(inf_index)) * 100
    Sys.sleep(0.1)
    system2("git", c("add", "."))
    info <- sprintf("%d%% done", round(j))
    setTkProgressBar(pb, i, sprintf("commits (%s)", info), info)
  }
  Sys.sleep(1)
  close(pb)


}


library(askpass)
pass = askpass('Enter the password')

system2("sudo", args = c("-S", "su"))

system2("git", args = c("commit", "-a", "-m", msg))
