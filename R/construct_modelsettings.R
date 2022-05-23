#' @title A function to parse modelsettings for simulation runs
#'
#' @description This function parses the shiny UI and gets input settings
#' and combines those with other settings for a full definition of the model and
#' settings that should be executed. This is a helper function.
#'
#' @param app_input a list of shiny GUI input
#' @param appsettings current appsettings
#' @param appNames character vector of app names, available in shiny server global environment upon app initialization
#' @return modelsettings a list with model settings. List elements are: \cr
#' modelsettings$apptitle - The name of the app that's being run.  \cr
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
#' @details This function returns specific settings for simulation runs.
#' @export

construct_modelsettings <- function(app_input, appsettings, appNames) {

  #extract current model settings from UI input elements
  # x1=isolate(reactiveValuesToList(input)) #get all shiny inputs
  x2 = app_input[! (names(app_input) %in% appNames)] #remove inputs that are action buttons for apps
  x3 = (x2[! (names(x2) %in% c('submitBtn','Exit', 'reset', 'detachtasks', 'destroytasks', 'DSAIRM') ) ]) #remove further inputs
  #modelsettings = x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
  modelsettings = x3
  #remove nested list of shiny input tags
  appsettings$otherinputs <- NULL
  #choose specific variables to add to modelsettings
  y1 <- appsettings[which(!names(appsettings)%in%c("appid", "docname", "modelfigname",
                                                   "underlying_function", "mbmodel_possible", "use_mbmodel",
                                                   "use_doc", "mbmodelname", "filepath"))]
  #add settings information from appsettings list
  modelsettings = c(y1, modelsettings)
  if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
  #if no random seed is set in UI, set it to 123.
  if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}


  return(modelsettings)

}

