#' @title The main menu for the DSAIRM package
#'
#' @description This function opens a Shiny app with a menu that will allow the user to run the different simulations.
#'
#' @details Run this function with no arguments to start the main menu (a Shiny app) for DSAIRM.
#' @examples
#' \dontrun{dsairmmenu()}
#' @author Andreas Handel
#' @import shiny
#' @export

dsairmmenu <- function() {


    appDir <- system.file("DSAIRM", package = "DSAIRM") #get directory for main menu app
    shiny::runApp(appDir = appDir, launch.browser = TRUE) #run main menu app

    print('*************************************************')
    print('Exiting the DSAIRM main menu.')
    print('I hope you had a fun and educational experience!')
    print('*************************************************')
}

#needed to prevent NOTE messages on CRAN checks
#comes because the data has that name
#not sure how to prevent otherwise
utils::globalVariables(c("hayden96flu","txtime","schirm20strep"))


.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIRM package. Type dsairmmenu() to get started.")
}
