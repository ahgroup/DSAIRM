#' @title A function that runs an app for specific settings and processes results for plot and text generation
#'
#' @description This function runs a model based on information
#' provided in the modelsettings list passed into it. The main work is split
#' and outsourced to different functions
#'
#' @param modelsettings a list with model settings. Required list elements are: \cr
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
#' @return A vectored list named "result" with each main list element containing the simulation results in a dataframe called dat and associated metadata required for generate_plot and generate_text functions. Most often there is only one main list entry (result[[1]]) for a single plot/text.
#' @details This function runs a model for specific settings.
#' @importFrom utils head tail
#' @importFrom stats reshape
#' @export

run_model <- function(modelsettings){

  # this function take the modelsettings information from the shiny UI and
  # turns them into a list of function calls for the underlying simulation function
  # that can then be executed
  fctcalls <- generate_fctcalls(modelsettings)

  #error handling
  #we expect a list if things worked ok, otherwise an error string is returned
  #which will be passed to caller
  if(!is.list(fctcalls)){ return(fctcalls) }

  #this executes the call(s) to the function(s) to be run
  #results are returned as nested list, the exact structure of the list depends
  #on the models/app that is executed
  resultslist <- lapply(fctcalls, function(this_fctcall){try(eval(this_fctcall))})

  #this takes results from the executed model calls,
  #as well as initial model settings, and generates output as needed
  #the result will be a list in a format needed to generate figures/text.
  result <- generate_output(modelsettings, resultslist)

  return(result)
}
