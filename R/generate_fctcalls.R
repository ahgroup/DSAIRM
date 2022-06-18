#' @title A helper function that produces call(s) to a simulator function for specific settings
#'
#' @description This function takes a modelsettings structure and uses that information
#' to create an unevaluated function call that runs the simulator function with the specified settings
#'
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
#' @return A list of strings containing one or multiple unevaluated function calls with the specified settings, or an error message
#' @details This function produces function call(s) for specific settings.
#' @export

generate_fctcalls <- function(modelsettings)
{

  #check to make sure inputs to function provide information needed for code to run
  if (is.null(modelsettings$simfunction)) { return("List element simfunction must be provided.") }
  if (is.null(modelsettings$modeltype)) { return("List element modeltype must be provided.") }

  # stochastic models do not support tstart and dt as inputs, thus they are not in the UI
  # but they are needed in those cases where a user wants to also run an
  # ODE model
  # therefore we define/set them here if they don't exist
  if (is.null(modelsettings$tstart) ) {modelsettings$tstart = 0 }
  if (is.null(modelsettings$dt) ) {modelsettings$dt = modelsettings$tfinal/1000 }

  # reduce typing
  modeltype = modelsettings$modeltype
  simfunction = modelsettings$simfunction #name(s) for model function(s) to run
  simfunction2 = modelsettings$simfunction2 #name(s) for model function(s) to run

  # for apps that include more than one model
  # they have an "_and_" in their modeltype variable as defined in the apptable.tsv spreadsheet
  # in that case, user chooses either one of the models or both (which is encoded by _and_)
  # we need to get the UI provided setting instead of the default one
  #if the user sets the model type in the UI, apply that choice
  if (grepl('_and_',modeltype))
  {
    # this can either be a single model, or both, in which case it contains _and_
    modeltype = modelsettings$modeltypeUI
  }

  #this will contain all function calls to be constructed
  #it's an empty data frame that contains the model,
  #a random seed and the constructed call
  fctcalls = data.frame(model = NULL, rngseed = NULL, call = NULL)

  # for most apps, a single model is run as specified in simfunction
  # the only exceptions are stochastic apps, and those when user runs both models
  if (!grepl('_stochastic_',modeltype)  && !grepl('_and_',modeltype))
  {
    fctcalls[1,] <- c(simfunction, NA, "")
  }

  ###################################
  # for stochastic models
  ###################################
  # for stochastic models, we need to run the model multiple times with different random seeds
  if (grepl('_stochastic_',modeltype) && !grepl('_and_',modeltype))
  {

    nreps = ifelse(is.null(modelsettings$nreps),1,modelsettings$nreps)
    seed = modelsettings$rngseed
    # data frame with different random seeds
    fctcalls <- c(rep(simfunction,nreps), seed+(0:(nreps-1)), "")
  }

  ###################################
  # for two models to run
  # those could 2 single-run models
  # or one could be a stochastic one
  ###################################
  # if neither is stochastic
  if (grepl('_and_',modeltype) && !grepl('_stochastic_',modeltype))
  {
    fctcalls[1,] <- c(simfunction, NA, "")
    fctcalls[2,] <- c(simfunction2, NA, "")
  }
  # if one is stochastic
  # current convention is that stochastic needs to be at beginning
  if (grepl('_and_',modeltype) && grepl('_stochastic_',modeltype))
  {
    nreps = ifelse(is.null(modelsettings$nreps),1,modelsettings$nreps)
    seed = modelsettings$rngseed
    fctcalls <- c(rep(simfunction,nreps), seed+(0:(nreps-1)), "")
    fctcalls <- cbind(fctcalls,c(simfunction2, NA, ""))
  }


  ###################################
  #start the fctcall generation
  ###################################
  #loop over all model entries in fctcalls data frame
  #generate the needed calls
  #add those calls to the call column
  for (n in 1:nrow(fctcalls))
  {

    modelsettings$rngseed = fctcalls$rngseed[n]
    #convert settings list to vector
    settingsvec = unlist(modelsettings)

    #try to get input arguments for function
    #at some point one should implement error handling for this
    modelinput = formals(fctcalls$model[n])
    ip = unlist(modelinput) #get all input arguments for function
    #pull out the inputs required by the simulator function from those
    #provided by the user/UI/shiny inputs
    currentargs = settingsvec[match(names(ip), names(settingsvec))]
    #if users do not provide all inputs but leave some cells empty
    #the matching will produce NA
    #we need users to provide all inputs, thus if NA are detected
    #we return an error message
    if (sum(is.na(currentargs)>0))
    {
      return('Please provide values for all inputs.')
    }
    #make a list, makes conversion to numeric easier
    arglist = as.list(currentargs)
    #convert arguments for function call to numeric if possible
    #preserve those that can't be converted
    numind = suppressWarnings(!is.na(as.numeric(arglist))) #find numeric values
    arglist[numind] = as.numeric(currentargs[numind])
    #add function name as first element to list
    fctlist = append(parse(text = currentmodel), arglist)
    #make fct call
    fctcalls$call[n] <- as.call(fctlist)
  }

  # return the data frame of all necessary function calls
  return(fctcalls)
}












