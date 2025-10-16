
#' Print time between first and second function call
#'
#' @param clear is logical arg to clear current time marker
#' @return time passed between calls
#' @export
how.long <- function(clear = FALSE){
  if(clear & ('how.long.time' %in% ls(envir = .GlobalEnv))){
    rm('how.long.time', envir = .GlobalEnv)
    how.long.time = Sys.time()
    return(assign('how.long.time', how.long.time, envir = .GlobalEnv))
  }else{
    if('how.long.time' %in% ls(envir = .GlobalEnv)){
      print(difftime(Sys.time() , how.long.time, units = 'auto' ))
      rm('how.long.time', envir = .GlobalEnv)
    }else{
      how.long.time = Sys.time()
      return(assign('how.long.time', how.long.time, envir = .GlobalEnv))
    }
  }
}

