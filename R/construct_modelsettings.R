#' @title A function that takes shiny GUI input and generates intermediate code used
#' to run simulations and process results
#'
#' @description This function is based on run_model() but instead of running the models
#' it outputs code equivalent to DSAIRM back-end, server processes initiated from shiny GUI.
#'
#' @param app_input a list of shiny GUI input
#' @param appsettings current appsettings
#' @return modelsettings a list with model settings. Required list elements are: \cr
#' modelsettings$simfunction - name of simulation function(s) as string.  \cr
#' modelsettings$is_mbmodel - indicate of simulation function has mbmodel structure
#' modelsettings$modeltype - specify what kind of model should be run.
#' Currently one of: _ode_, _discrete_, _stochastic_, _usanalysis_, _modelexploration_, _fit_. \cr
#' For more than one model type, place _and_ between them. \cr
#' modelsettings$plottype - 'Boxplot' or 'Scatterplot' , required for US app \cr
#' Optinal list elements are: \cr
#' List elements with names and values for inputs expected by simulation function.
#' If not provided, defaults of simulator function are used.\cr
#' modelsettings$plotscale - indicate which axis should be on a log scale (x, y or both).
#' If not provided or set to '', no log scales are used. \cr
#' modelsettings$nplots -  indicate number of plots that should be produced (number of top list elements in result).
#' If not provided, a single plot is assumed.  \cr
#' modelsettings$nreps - required for stochastic models to indicate numer of repeat simulations.
#' If not provided, a single run will be done. \cr
#' @details This function runs a model for specific settings.
#' @importFrom utils head tail
#' @importFrom stats reshape
#' @export

construct_modelsettings <- function(app_input, appsettings, appNames) {

  #extract current model settings from UI input elements
  # x1=isolate(reactiveValuesToList(input)) #get all shiny inputs
  x2 = app_input[! (names(app_input) %in% appNames)] #remove inputs that are action buttons for apps
  x3 = (x2[! (names(x2) %in% c('submitBtn','Exit') ) ]) #remove further inputs
  #modelsettings = x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
  modelsettings = x3
  #remove nested list of shiny input tags
  appsettings$otherinputs <- NULL
  #add settings information from appsettings list
  modelsettings = c(appsettings, modelsettings)
  if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
  #if no random seed is set in UI, set it to 123.
  if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}



  return(modelsettings)

}

