#' @title A function that lets you run a specific DSAIRM without going through the main menu
#'
#' @description This function opens the specified DSAIRM Shiny App
#' @param appname a string (with quotation marks) indicating the name of the app to run.
#' Leave empty to get a list of all available apps.
#'
#' @details Run this function with no arguments to list all apps. Specify the name of an app (with quotation marks) to start that app
#' @examples
#' # To see all available apps, run
#' dsairmapps()
#' # To start a specific app, call its name, e.g.
#' \dontrun{dsairmapps('BasicVirus')}
#' @author Andreas Handel
#' @export


dsairmapps <- function(appname = NULL)
{
  if (is.null(appname))
  {
    apppath = system.file("shinyapps", package = "DSAIRM")
    applist = list.dirs(path = apppath, full.names = FALSE, recursive = FALSE)
    applist = applist[applist != 'MainMenu'] #remove main menu entry from list of apps
    upcasewords = grepl("^[[:upper:]]", applist) #anything starting with a lower case letter is not an app
    applist = applist[upcasewords] #remove all non-apps
    print(paste('Currently existing apps:', paste(applist, collapse =', ')))
  }
  else
  {
    appDirname <- system.file("shinyapps", appname, package = "DSAIRM")
    shiny::runApp(appDir = appDirname)
  }
}
