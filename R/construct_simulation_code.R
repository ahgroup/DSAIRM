#' @title A function that takes shiny GUI input and generates intermediate code used
#' to run simulations
#'
#' @description This function is based on run_model() but instead of running the models
#' it outputs code equivalent to DSAIRM back-end, server processes initiated from shiny GUI.
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

construct_simulation_code <- function(modelsettings) {



  #check to make sure inputs to function provide information needed for code to run
  if (is.null(modelsettings$simfunction)) { return("List element simfunction must be provided.") }
  if (is.null(modelsettings$modeltype)) { return("List element modeltype must be provided.") }



  #if the user sets the model type, apply that choice
  #that happens for any models that have an "_and_" in their modeltype variable as defined in the apptable.tsv spreadsheet
  if (grepl('_and_',modelsettings$modeltype))
  {
    modelsettings$modeltype = modelsettings$modeltypeUI
  }

  modeltype_all = modelsettings$modeltype
  fctcalls_all = NULL
  # datall = NULL #will hold data for all different models and replicates
  # finaltext = NULL
  simfunction = modelsettings$simfunction #name(s) for model function(s) to run









  ##################################
  #stochastic dynamical model execution
  ##################################
  if (grepl('_stochastic_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_stochastic',simfunction)] # get the stochastic function
    noutbreaks = 0
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
    }














  ##################################
  #ode dynamical model execution
  ##################################
  if (grepl('_ode_',modelsettings$modeltype)) #need to always start with ode_ in model specification
  {

    # stochastic doesn't support tstart and dt as inputs, thus they are not in the UI
    # but they are needed if we run ode models at the same time
    # therefore set here if they don't exist
    if (is.null(modelsettings$tstart) ) {modelsettings$tstart = 0 }
    if (is.null(modelsettings$dt) ) {modelsettings$dt = modelsettings$tfinal/1000 }

    modelsettings$currentmodel = simfunction[grep('_ode',simfunction)] #list of model functions, get the ode function

    #make the call to the simulator function by parsing inputs
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
  #discrete dynamical model execution
  ##################################
  if (grepl('_discrete_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_discrete',simfunction)] #list of model functions, get the ode function

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











  # ##################################
  # #take data from all simulations and turn into list structure format
  # #needed to generate plots and text
  # #this applies to simulators that run dynamical models
  # #other simulation functions need output processed differently and will overwrite some of these settings
  # #each other simulator function has its own code block below
  # ##################################
  #
  # #save all results to a list for processing plots and text
  # listlength = ifelse(is.null(modelsettings$nplots),1,modelsettings$nplots)
  # result = vector("list", listlength) #create empty list of right size for results










  ##################################
  #simulators that are not models themselves use their custom code own blocks
  #these are specified below
  #these simulators might overwrite some of the default settings above
  ##################################



  ##################################
  #Code block for US analysis
  ##################################
  if (grepl('_usanalysis_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction

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
  #end US analysis model code block
  ##################################


  ##################################
  #model fitting code block
  ##################################
  if (grepl('_fit_',modelsettings$modeltype))
  {


    modelsettings$currentmodel = simfunction

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


  ##################################
  #model exploration code block
  ##################################
  if (grepl('_modelexploration_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction

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
  #end model exploration code block
  ##################################



  return(list(modeltype_all, fctcalls_all))

}

