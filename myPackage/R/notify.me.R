
#' Print message to notifications when called
#'
#' @param message is string to display in notif. banner
#' @return notification banner
#' @export
notify.me <- function(message = "R completed") {

  command <- paste0(
    "terminal-notifier -message '", message,
    "' -title 'R Notification' -subtitle 'Completion Notice' ",
    "-sound /System/Library/Sounds/Ping.aiff"
  )

  system(command)
}

