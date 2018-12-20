#' @title A function that runs a DSAIRM/DSAIDE app
#'
#' @description This function takes a model and model settings and runs it.
#' It runs the simulation determined by the model settings and returns simulation results.
#'
#' @param modelsettings vector of model settings. needs to match input expected by simulation function
#' @param mbmodel The name of a app/simulation function to be run with the indicated settings.
#' @return A list named "result" with the simulated dataframe and associated metadata.
#' @details This function runs a model for specific settings. It is similar to analyze_model in the modelbuilder package.
#' @author Andreas Handel
#' @export

run_model <- function(modelsettings, mbmodel) {


  #save all results to a list for processing plots and text
  listlength = 1
  #here we do all simulations in the same figure
  result = vector("list", listlength) #create empty list of right size for results

  #run simulation by executing the function call
  #the generate_fctcall creates a function call to the specified model based on the given model settings
  #depending on if mbmodel is the name to a function or a modelbuilder object, different returns are produced
  fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = mbmodel)
  set.seed(modelsettings$rngseed) #set RNG seed specified by the settings before executing function call

  browser()

  #single model execution
  if (modelsettings$nreps == 1 | modelsettings$modeltype == 'ode' | modelsettings$modeltype == 'discrete')
  {
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    result[[1]]$dat = simresult$ts
  }

  #loop over multiple runs (only leads to potential differences for stochastic model)
  if (modelsettings$nreps > 1 & modelsettings$modeltype == 'stochastic')
  {
    datall = NULL
    for (nn in 1:modelsettings$nreps)
    {
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    #data for plots and text
    #needs to be in the right format to be passed to generate_plots and generate_text
    #see documentation for those functions for details
    simresult <- simresult$ts
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
    dat$IDvar = paste(dat$varnames,nn,sep='') #make a variable for plotting same color lines for each run in ggplot2
    dat$nreps = nn
    datall = rbind(datall,dat)
    }
    result[[1]]$dat = datall
  }

  #Meta-information for each plot
  #Might not want to hard-code here, can decide later
  result[[1]]$plottype = "Lineplot"
  result[[1]]$xlab = "Time"
  result[[1]]$ylab = "Numbers"
  result[[1]]$legend = "Compartments"

  plotscale = modelsettings$plotscale

  result[[1]]$xscale = 'identity'
  result[[1]]$yscale = 'identity'
  if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
  if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}

  result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur inside generate_text

  return(result)
}
