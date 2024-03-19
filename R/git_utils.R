#-----(GIT LOCAL SERVER)
#' LOCALE
#' @return
#' @export
#' @examples
#' git_locale()
git_locale = function(){
  system2("whereis", c("git"))
}

#' INIT
#' @return
#' @export
#' @examples
#' git_init()
git_init = function(){

  #-----(check git-basic)
  if(system2("git", c('version')) != 0 ||
     system('git --version') != 0 ||
     system2("git", c('version')) == 127)
    cranquerry_warn_msg("git is not installed") else cranquerry_msg('git is installed:proceed')

  #-----(Execute)
  system2("git", c("init"))
}

#' NON-COMMITS
#' @rdname git
#' @family git
#' @import R.utils
#' @param log_path
#' @return
#' @export
#' @examples
#' noncommits_log(getwd())
noncommits_log = function(log_path=NULL){
  #----(log path)
  if(is.null(log_path)){
    log_path = file.path(.globals$get(getwd),"log")
  } else {
    log_path = file.path(log_path,"log")
  }

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
    if(R.utils::countLines(git_ncs)[1] > 1){
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
    if(R.utils::countLines(git_ncs)[1] > 1){
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

#' STAGE
#' @rdname git
#' @family git
#' @import tcltk
#' @return
#' @export
#' @note
#' With a fun twist, tcltk progress bar gimmick added. Need to fix geometry.
#' Issue with box geometry.
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

#' COMMIT
#' @import svDialogs
#' @param local_user
#' @param msg
#' @return
#' @export
#' @examples
git_commit = function(msg, stage_first = FALSE){

  #-----(commit command)
  if(stage_first){
    system2("git", c("add", "."))
    system2("git", args = c("commit", "-a", "-m", msg))
    svDialogs::dlg_message(message = 'all files committed', type="ok", gui = .GUI)
  } else {
    system2("git", args = c("commit", "-a", "-m", msg))
    svDialogs::dlg_message(message = 'All files committed', type="ok", gui = .GUI)
  }
}

#------(GITHUB REMOTE)

#' REMOTE SET
#' @return
#' @export
#' @examples
#' git_remote_set()
git_remote_set = function(user_name=NULL, remote_name = "origin", ssh = F){

  bn = basename(.globals$get(getwd))
  if(ssh){
    system2("git", c("remote", "set-url", remote_name , paste0("ssh://git@github.com:", user_name, "/", bn, ".git")))
    browseURL(paste0("ssh://git@github.com:", user_name, "/", bn, ".git"))
  } else {
    system2("git", c("remote", "set-url", remote_name , paste0("https://github.com/", user_name, "/", bn, ".git")))
    browseURL(paste0("https://github.com/", user_name, "/", bn, ".git"))
  }
}

#' REMOTE ADD
#' @return
#' @export
#' @examples
#' git_remote_add()
git_remote_add = function(url, remote_name=NULL){
  if(is.null(remote_name))
    system2("git", c("remote", "add", "origin", url)) else
      system2("git", c("remote", "add", remote_name, url))
}

#' REMOTE ALIASES
#' @return
#' @export
#' @note
#' NULL if no remote aliases created
#' @examples
#' git_remote_versions()
git_remote_versions = function(){
    system2("git", c("remote", "-v"))
}

#' REMOVE REMOTE
#' @return
#' @export
#' @examples
#' git_remove_remote()
git_remove_remote = function(remote_alias = "origin"){
  system2("git", c("remote", "remove", remote_alias))
}

#' PUSH
#' @import rstudioapi
#' @param url
#' @param remote_name
#' @param force
#'
#' @return
#' @export
#'
#' @examples
git_push = function(url = NULL, remote_name = NULL, force = F){

  #----(WHOAREU?)
  user <- svDialogs::dlgInput("Who are you?", Sys.info()["user"])$res

  #-----(usr options)
  usr_options = c("goob",
                  "i dont know",
                  "f*** the world",
                  system2("whoami",
                          stdout = TRUE,
                          stderr = TRUE))

  #-----(menu items)
  lusr = utils::menu(c("goob",
                       "i dont know",
                       system2("whoami",
                               stdout = TRUE,
                               stderr = TRUE)),
                     title="Choose your user account?")
  lusr = as.numeric(lusr)

  #-----(user check)
  if(Sys.info()["user"] != user || Sys.info()["user"] != usr_options[lusr] || usr_options[lusr] != rstudioapi::userIdentity())
    stop(sprintf("check your username; expecting %s", Sys.info()["user"]))

  #----(terminals list)
  tl = rstudioapi::terminalList()

  #----(check terminal status)
  bterms = c()
  for(i in tl){
    if(!rstudioapi::terminalBusy(i)){
      bterms = c(bterms, i)
    }
  }

  #-----(Execute cmd)
  if(is.null(url) && is.null(remote_name)){
    if(force) rstudioapi::terminalExecute("git push origin master -f") else
      rstudioapi::terminalExecute("git push origin master")
  } else if(!is.null(url) || is.null(remote_name)){
    if(force) rstudioapi::terminalExecute(paste("git push", url, "-f", sep = " ")) else
      rstudioapi::terminalExecute(paste("git push ", url, sep = " "))
  } else if(!is.null(url) && !is.null(remote_name)){
    mid = paste0("--set-", remote_name)
    if(force) rstudioapi::terminalExecute(paste("git push", mid, "-f", sep = " ")) else
      rstudioapi::terminalExecute(paste("git push", mid, sep = " "))

  }

}

#-----(GITHUB API CALLS)
#' API USER INFO
#' @param user_name
#' @return
#' @export
#' @note
#' curl must be install on ubuntu so "sudo apt install curl"
#' @examples
#' github_user_api_information("alphaprime7")
github_user_api_information = function(user_name){
  url = paste0("https://api.github.com/users/",user_name)
  x = system2("curl",c(url), stdout = T)
  return(x)
}

#' API PRIVATE USER INFO
#'
#' @param user_name
#' @param gh_token
#' @return github data
#' @export
#' @example
#' x = github_get_private("alphaprime7", gh_token = Sys.getenv('gh_token_linux'))
github_get_private = function(user_name, gh_token){
  #url = paste0("https://api.github.com/users/",user_name)
  url = "https://api.github.com/gists/starred"
  user_name = paste0(user_name,":")
  x = system2("curl",c("--user", paste0(user_name,gh_token), url), stdout = T)
  return(x)
}




#-----(MULTI-BRANCHES)

#' MERGE
#' @return
#' @export
#' @examples
#' git_merge()
git_merge = function(branch_to_merge = NULL){
    system2("git", c("merge", branch_to_merge))
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

#' Interactive Add
#' @return
#' @export
#' @examples
#' git_locale()
git_add = function(){
  system2("git", c("add", "i"))
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

#' Restore
#' @return
#' @export
#' @examples
#' git_restore(filename)
git_restore = function(filename){
  system2("git", c("restore",filename))
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

#' INDEX LOCK RM
#' @return
#' @export
#' @examples
#' remove_index_lock()
remove_index_lock = function(){
  system2("rm", c("--force", "./.git/index.lock"))
}

#' GIT LOG
#' @return
#' @export
#' @examples
#' git_log()
git_log = function(){
  system2("git", c("log"))
}
