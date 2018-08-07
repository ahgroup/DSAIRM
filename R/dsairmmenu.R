#' @title The main menu for the DSAIRM package
#'
#' @description This function opens a Shiny app menu that will allow the user to run the different simulation apps.
#'
#' @details Run this function with no arguments to start the main menu (a Shiny app) for DSAIRM.
#' @examples
#' \dontrun{dsairmmenu()}
#' @author Andreas Handel
#' @import shiny
#' @export

dsairmmenu <- function() {
  cond <- 1
  while (cond == 1)
  {
    appname <- NULL
    appDir <- system.file("shinyapps", "MainMenu", package = "DSAIRM")
    appname = shiny::runApp(appDir = appDir)
    if (!is.null(appname) & appname != "Exit")     #run the shiny app chosen
    {
      appDirname <- system.file("shinyapps", appname, package = "DSAIRM")
      shiny::runApp(appDir = appDirname)
    }
    if (appname == "Exit") {cond = 0} #leave while loop/menu
  }
  print('*************************************************')
  print('Exiting the DSAIRM main menu.')
  print('I hope you had a fun and educational experience!')
  print('*************************************************')
}

#needed to prevent NOTE messages on CRAN checks
#most of those are from the ggplot commands in the generate_ functions,
#the last one is from the subset function in the fit functions
utils::globalVariables(c("xvals", "yvals", "varnames","IDvar","style","Condition"))


.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIRM package. Type dsairmmenu() to get started.")
}
