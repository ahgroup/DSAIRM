#' @title A helper function that returns a list of existing apps/simulations
#'
#' @description This function opens returns a list of existing apps
#' @details Run this function with no arguments to list all apps.
#' @examples
#' # To see all available apps, run
#' dsairmapps()
#' @author Andreas Handel
#' @export


dsairmapps <- function()
{
    apppath = system.file("DSAIRMapps", package = "DSAIRM")
    applist = list.dirs(path = apppath, full.names = FALSE, recursive = FALSE)
    applist = applist[applist != 'MainMenu'] #remove main menu entry from list of apps
    upcasewords = grepl("^[[:upper:]]", applist) #anything starting with a lower case letter is not an app
    applist = applist[upcasewords] #remove all non-apps
    paste(applist, collapse =', ')
}
