#' SCREENRES
#' @rdname screen
#' @return
#' @export
#' @note
#' Only for Ubuntu. IDGARA about any other OS.
#' Ubuntu combines the width for multiple platforms into 1
#' Screen number applicable to windows.
#' @examples
#' get_screen_resolution()
get_screen_resolution = function(screen_number = NULL){
  if(.Platform$OS.type == 'unix'){

    #-----(system resolution)
    screen_res_ubuntu = system2("xdpyinfo", c("|", "awk", "'/dimensions/{print $2}'"), stdout = T)
    #screen_res_ubuntu = system2("xdpyinfo", c("|", "grep", "-oP", "'dimensions:\s+\K\S+'"), stdout = T)

    #-----(pattern)
    sr_pat = "^(.*)[x](.*)" #experiment with [x] worked
    sr = grep(sr_pat, screen_res_ubuntu, value = TRUE)
    #sr = substring(w, 1 - regexpr("x", screen_res_ubuntu))

    #-----(width)
    width.loc = "\\1"
    width = as.numeric(gsub(sr_pat, width.loc, screen_res_ubuntu))
    #width = sapply(strsplit(screen_res_ubuntu, "x"), "[", 1)
    #w = sub("x.*", "", screen_res_ubuntu)

    #-----(height)
    height.loc = "\\2"
    height = as.numeric(gsub(sr_pat, height.loc, screen_res_ubuntu))
    #height = sapply(strsplit(screen_res_ubuntu, "x"), "[", 2)
    #h = sub(".*x", "", screen_res_ubuntu)

    #-----R
    screen_res = c(width, height)
    message(sprintf('The screen resolution is %dx%d on the  %s platform', screen_res[1], screen_res[2], .Platform$OS.type))
    return(screen_res)

  } else if(.Platform$OS.type == 'windows'){

    #-----(raw resolutions)
    width_raw = shell("wmic desktopmonitor get screenwidth", intern = T)
    height_raw = shell("wmic desktopmonitor get screenheight", intern = T)

    #-----(resolution vectors)
    wv = as.numeric(c(
      width_raw[-c(1,length(width_raw))]
    ))
    wv = wv[!is.na(wv)]

    hv = as.numeric(c(
      height_raw[-c(1,length(height_raw))]
    ))
    hv = hv[!is.na(hv)]

    #----(number of screens)
    numscreens = sum(!is.na(wv))
    #numscreens = sum(!is.na(hv))

    #----(resolutions list)
    screen_res = vector(mode = 'list', length = length(wv))

    #----(main??)
    for(i in seq(wv)){
      for(j in seq(hv)){
        sr = c(wv[i], hv[i])
        screen_res[[i]] = sr
      }
    }

    #---R
    message(sprintf('The program detected %d screens on the %s platform', numscreens, .Platform$OS.type))
    if(is.null(screen_number)){
      return(screen_res)
    } else {
      return(screen_res[[screen_number]])
    }

  }

}

#' @rdname screen
#' @return
#' @export
#' @seealso [get_screen_resolution()]
#' @note
#' Windows only. A rudimentary version of @seealso [get_screen_resolution()]
#' @examples
#' screen_resolution()
screen_resolution = function(){
  if(.Platform$OS.type == 'unix'){

    #-----(system resolution)
    warning(sprintf('The function is not designed for the %s platform', .Platform$OS.type))
    return(NULL)

  } else if(.Platform$OS.type == 'windows'){

    #----(Main?? monitor)
    suppressWarnings(
      current_resolution <- system("wmic path Win32_VideoController get CurrentHorizontalResolution,CurrentVerticalResolution /format:value", intern = TRUE)  %>%
        strsplit("=") %>%
        unlist() %>%
        as.numeric()
    )
    #---R
    current_resolution <- current_resolution[!is.na(current_resolution)]
    current_resolution = as.vector(current_resolution)

    message(sprintf('The screen resolution is c(%d,%d) on the %s platform', numscreens, .Platform$OS.type))
    return(current_resolution)

  }

}


