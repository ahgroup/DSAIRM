#' @title A function that takes shiny GUI input and generates intermediate code used
#' to run simulations
#'
#' @description This function takes a modelsettings list is based on run_model() but instead of running the models
#' it outputs code equivalent to DSAIRM back-end, server processes initiated from shiny GUI.
#'
#' @param modelsettings a list with model settings. Required list elements are: \cr
#' modelsettings$simfunction - name of simulation function(s) as string.  \cr
#' modelsettings$modeltype - specify what kind of model should be run.
#' Currently one of: _ode_, _discrete_, _stochastic_, _usanalysis_, _modelexploration_, _fit_. \cr
#' For more than one model type, place _and_ between them. \cr
#' modelsettings$plottype - 'Boxplot' or 'Scatterplot' , required for US app \cr
#' Optional list elements are: \cr
#' List elements with names and values for inputs expected by simulation function.
#' If not provided, defaults of simulator function are used.\cr
#' modelsettings$plotscale - indicate which axis should be on a log scale (x, y or both).
#' If not provided or set to '', no log scales are used. \cr
#' modelsettings$nplots -  indicate number of plots that should be produced (number of top list elements in result).
#' If not provided, a single plot is assumed.  \cr
#' modelsettings$nreps - required for stochastic models to indicate numer of repeat simulations.
#' If not provided, a single run will be done. \cr
#' @return A list with two elements: the first is the modelsettings$modeltype; the second is a nested list of function calls generated according to the modelsettings.
#' @details This function provides a list of function calls necessary in constructing code for download from the shiny GUI.
#' @importFrom utils head tail
#' @importFrom stats reshape
#' @export

generate_simulationcode <- function(modelsettings) {

  #check to make sure inputs to function provide information needed for code to run
  if (is.null(modelsettings$simfunction)) { return("List element simfunction must be provided.") }
  if (is.null(modelsettings$modeltype)) { return("List element modeltype must be provided.") }

  # save model types, might be overwritten below
  modeltype_all = modelsettings$modeltype

  #if the user sets the model type in the UI, apply that choice
  #that happens for any models that have an "_and_" in their modeltype variable as defined in the apptable.tsv spreadsheet
  if (grepl('_and_',modelsettings$modeltype))
  {
    modelsettings$modeltype = modelsettings$modeltypeUI
  }

  #just to save typing
  simfunction = modelsettings$simfunction #name(s) for model function(s) to run

  #this will contain all function calls to be constructed
  fctcalls_all = NULL

  # stochastic models do not support tstart and dt as inputs, thus they are not in the UI
  # but they are needed if we run ode models at the same time
  # therefore set here if they don't exist
  if (is.null(modelsettings$tstart) ) {modelsettings$tstart = 0 }
  if (is.null(modelsettings$dt) ) {modelsettings$dt = modelsettings$tfinal/1000 }


  ##################################
  # set the current model to run
  ##################################
  # there can only be a single model for these app types
  if (grepl('_usanalysis_',modelsettings$modeltype)) {modelsettings$currentmodel <- simfunction }
  if (grepl('_fit_',modelsettings$modeltype)) {modelsettings$currentmodel <- simfunction }
  if (grepl('_modelexploration_',modelsettings$modeltype)) {modelsettings$currentmodel <- simfunction }
  # there can be more than one model for these app types, so need to figure out which


    #grepl('_modelexploration_',modelsettings$modeltype)
  #grepl('_fit_',modelsettings$modeltype))
  modelsettings$currentmodel <-  dplyr::case_when(
    grepl('_discrete_',modelsettings$modeltype) ~   simfunction[grep('_discrete',simfunction)],
    grepl('_ode_',modelsettings$modeltype) ~   simfunction[grep('_ode',simfunction)],
    grepl('_stochastic_',modelsettings$modeltype) ~ simfunction[grep('_stochastic',simfunction)],
    TRUE ~ simfunction
  )

  browser()

    # multiple runs for stochastic model
    # single runs for all others
    if (grepl('_stochastic_',modelsettings$modeltype))
    {
      nreps = ifelse(is.null(modelsettings$nreps),1,modelsettings$nreps)
      for (nn in 1:nreps)
      {
        #create function call to run model
        fctcall = generate_fctcall(modelsettings)
        fctcalls_all <- c(fctcalls_all, fctcall)
        # this means an error occurred making the call
        if (!is.call(fctcall))
        {
          #return error message generated when trying to build the function call
          return(fctcall)
        }
        modelsettings$rngseed = modelsettings$rngseed + 1 #need to update RNG seed each time to get different runs
      }
    } else {
    #create function call, then evaluate it to run model
    fctcall = generate_fctcall(modelsettings)
    fctcalls_all <- c(fctcalls_all, fctcall)
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
  }
  ##################################
  #end model fitting code block
  ##################################

  return(list(modeltype_all, fctcalls_all))

}

