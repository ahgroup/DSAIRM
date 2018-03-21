#' @title The main menu for the DSAIRM package
#'
#' @description This function opens a Shiny App menu that will allow the user to run the different simulation apps
#'
#' @details Run this function with no arguments to start the main menu
#' @examples
#' \dontrun{dsairmmenu()}
#' @author Andreas Handel
#' @import shiny
#' @importFrom knitr knit
#' @export

dsairmmenu <- function() {

  cond <- 1

    while (cond == 1){

    appname <- NULL
    appDir <- system.file("shinyapps", "MainMenu", package = "DSAIRM")
    op = shiny::runApp(appDir = appDir)

<<<<<<< HEAD
    if (op == "Exit") {cond = 0} #leave while loop/menu

    if (op == "BasicBacteria") {appname = "BasicBacteria"}
    if (op == "BasicVirus") {appname = "BasicVirus"}
    if (op == "VirusandIR") {appname = "VirusandIR"}

    if (op == "ModelExploration") {appname = "ModelExploration"}
    if (op == "HCVmodel") {appname = "HCVmodel"}
    if (op == "InfluenzaResistance") {appname = "InfluenzaResistance"}

    if (op == "ModelVariants") {appname = "ModelVariants"}
    if (op == "USAnalysis") {appname = "USAnalysis"}
    if (op == "BasicVirusStochastic") {appname = "BasicVirusStochastic"}
=======
    if (op == "X") {cond = 0} #leave while loop/menu
    if (op == "BasicBacteria") {appname = "BasicBacteria"}
    if (op == "BasicVirus") {appname = "BasicVirus"}
    if (op == "VirusandIR") {appname = "VirusandIR"}
    if (op == "ModelExploration") {appname = "ModelExploration"}
    if (op == "USAnalysis") {appname = "USAnalysis"}
    if (op == "StochasticModel") {appname = "StochasticModel"}
>>>>>>> origin/master

    if (!is.null(appname))     #run the shiny app chosen
    {
        appDir <- system.file("shinyapps", appname, package = "DSAIRM")
        shiny::runApp(appDir = appDir)
    }

  }
  print('*************************************************')
  print('Exiting the DSAIRM main menu.')
  print('I hope you had a fun and educational experience!')
  print('*************************************************')

}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIRM package. Type dsairmmenu() to get started.")
}

