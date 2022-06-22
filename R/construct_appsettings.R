#' @title A function to parse appsettings
#'
#' @description This function parses the apptable for a specific appName preparing for generate_shinyinput()
#'
#' @param appName an app name
#' @param modeldir model directory
#' @param simdir sim directory
#' @return the information is stored in a list called 'appsettings' \cr
#' different models can have different variables \cr
#' all models need the following: \cr
#' variable appid - ID (short name) of the app \cr
#' variable apptitle - the name of the app. Used to display. \cr
#' variable docname - name of documentation file for app \cr
#' variable modelfigname - name of figure file for app \cr
#' variable simfunction - the name of the simulation function(s) \cr
#' variable mbmodelname - if there is an mbmodel available, list its name \cr
#' variable modeltype - the type of the model to be run. if multiple, i.e. containing "_and_" it is set by UI. \cr
#'  \cr
#' additional elements that can be provided: \cr
#' variable otherinputs - contains additional shiny UI elements that are not generated automatically by functions above \cr
#' for instance all non-numeric inputs need to be provided separately. \cr
#' this is provided as text \cr
#' If not needed, it is empty "" \cr
#' @details This function returns specific settings for apps.
#' @export

construct_appsettings <- function(appName, at, modeldir, simdir) {

  #each app has settings stored in apptable
  #read and assign to list called 'appsettings'
  #store in global variable
  appsettings <- as.list(at[which(at$appid == appName),])

  #a few apps have 2 simulator functions, combine here into vector
  if (nchar(appsettings$simfunction2) > 1)
  {
    appsettings$simfunction <<- c(appsettings$simfunction,appsettings$simfunction2)
  }

  #all columns are read in as characters, convert some
  appsettings$use_mbmodel = as.logical(appsettings$use_mbmodel)
  appsettings$use_doc = as.logical(appsettings$use_doc)
  appsettings$nplots = as.numeric(appsettings$nplots)

  #if an mbmodel should be used, check that it exists and load
  appsettings$mbmodel <- NULL
  if (appsettings$use_mbmodel)
  {
    appsettings$mbmodel = readRDS(paste0(modeldir,"/",appsettings$mbmodelname) )
    if (! is.list(appsettings$mbmodel))  {return("mbmodel could not be loaded in app.R")}
  }

  #if the doc of a file should be parsed for UI generation, get it here
  appsettings$filepath <- NULL
  if (appsettings$use_doc)
  {
    filepath = paste0(simdir,'/',appsettings$simfunction[1],'.R')
    if (! file.exists(filepath))  {return("file for function can't be found")}
    appsettings$filepath = filepath
  }

  return(appsettings)

}

