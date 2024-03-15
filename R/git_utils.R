#' Commit
#' @param local_user
#' @param msg
#' @return
#' @export
#' @examples
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

  #-----(user check)
  if( local_user != usr_options[lusr] || local_user != usr_options[lusr] || local_user != Sys.info()["user"])
    stop(sprintf("check your username; expecting %s", Sys.info()["user"]))

  #-----(commit command)
  system2("git", args = c("commit", "-a", "-m", msg))

}

#' Non commits
#' @rdname git
#' @family git
#' @import R.utils
#' @param log_path
#' @return
#' @export
#' @examples
#' noncommits_log(getwd())
noncommits_log = function(log_path){
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
#' @import tcltk
#' @return
#' @export
#' @note
#' With a fun twist, tcltk progress bar gimmick added. Need to fix geometry.
#' @examples
#' git_stage()
git_stage = function(){
  pb = tcltk::tkProgressBar(title = "Commit progress bar", label = "Commit progress bar",
                     min = 0, max = 1, initial = 0, width = 300)
  Sys.sleep(0.5)
  inf_index = system("git status --short", intern = T)
  infidels_ls = system2("git", c('status', '--short'))
  nc_log = noncommits_log()

  for(i in seq(inf_index)) {
    j = (i/length(inf_index)) * 100
    Sys.sleep(0.1)
    system2("git", c("add", "."))
    info <- sprintf("%d%% done", round(j))
    tcltk::setTkProgressBar(pb, i, sprintf("commits (%s)", info), info)
  }
  Sys.sleep(1)
  close(pb)

}

#' Init
#' @return
#' @export
#' @examples
#' git_init()
git_init = function(){
    system2("git", c("init"))
}

#' Remote
#' @return
#' @export
#' @examples
#' git_remote()
git_remote = function(url, remote_name=NULL){
  if(is.null(remote_name))
    system2("git", c("remote", "add", "origin", url)) else
      system2("git", c("remote", "add", remote_name, url))
}

#' Push
#' @return
#' @export
#' @examples
#' git_push()
git_push = function(url = NULL, remote_name = NULL, force = F){
  if(is.null(url) && is.null(remote_name)){
    if(force) system2("git", c("push", "origin", "master", "-f")) else
      system2("git", c("push", "origin", "master"))
  } else if(!is.null(url) || is.null(remote_name)){
    if(force) system2("git", c("push", url, "-f")) else
      system2("git", c("push", url))
  } else if(!is.null(url) && !is.null(remote_name)){
    if(force) system2("git", c("push", paste0("--set-", remote_name) , url, "-f")) else
      system2("git", c("push", is.null(remote_name), url))

  }

}

#' Pull
#' @return
#' @export
#' @examples
#' git_pull()
git_pull = function(){
  system2("git", c("pull", "origin", "master"))
}

#' Clone
#' @return
#' @export
#' @examples
#' git_clone()
git_clone = function(url){
  system2("git", c("clone", url))
}

#' Locale
#' @return
#' @export
#' @examples
#' git_locale()
git_locale = function(){
  system2("whereis", c("git"))
}

#' Un_init or Remove
#' @return
#' @export
#' @examples
#' git_remove()
git_remove = function(){
  system2("rm", c("rf", ".git"))
}

#' Reset
#' @return
#' @export
#' @examples
#' git_reset(number_of_commits_back)
git_reset = function(number_of_commits_back){
  number_of_commits_back = as.numeric(number_of_commits_back)
  system2("git", c("reset", "--hard", paste0("HEAD~",number_of_commits_back)) )
}

#' Delete
#' @return
#' @export
#' @examples
#' git_delete()
git_delete = function(){
  system2("git", c("push", "origin", "master", "--delete"))
}

#' Revert
#' @return
#' @export
#' @examples
#' git_revert(number_of_commits_back)
git_revert = function(number_of_commits_back, msg, push = F){
  number_of_commits_back = as.numeric(number_of_commits_back)
  system2("git", c("revert", "n", paste0("HEAD",number_of_commits_back,"..HEAD")))
  system2("git", c("commit", "-m", msg))
  if(push) system2("git", c("push", "origin", "master")) else
    message("No need to push")
}

#' New branch
#' @return
#' @export
#' @examples
#' git_new_branch(branch_name)
git_new_branch = function(branch_name){
  system2("git", c("branch",branch_name))
}

#' Checkout
#' @return
#' @export
#' @examples
#' git_checkout(new_branch)
git_checkout = function(new_branch=NULL, base_branch=NULL, create_and_switch = F){
  if(is.null(base_branch)){
    if(create_and_switch){
      system2("git", c("checkout", "-b", new_branch))
    } else {
      system2("git", c("checkout", new_branch))
    }
  } else {
    if(create_and_switch){
      system2("git", c("checkout", "-b", new_branch, base_branch))
    } else {
      system2("git", c("checkout", new_branch))
    }

  }

}

#' Fetch
#' @return
#' @export
#' @examples
#' git_fetch()
git_fetch = function(){
  system2("git", c("fetch","--all"))
}

#' Stage Specific File Types
#' @return
#' @export
#' @examples
#' git_stage_spec(ext = ".sh")
git_stage_spec = function(ext){
  system2("git", c("add", paste0("git-*", ext)))
}
