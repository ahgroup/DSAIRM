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

  set.seed(modelsettings$rngseed) #set RNG seed specified by the settings before executing function call

  datall = NULL #will hold data for all different models and replicates

  ##################################
  #stochastic dynamical model execution
  ##################################
  if (grep('stochastic',modelsettings$modeltype))
  {
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = mbmodel)
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
  }

  ##################################
  #ode dynamical model execution
  ##################################
  if (grep('ode',modelsettings$modeltype))
  {

    currentmodel = mbmodel[grep('_ode',mbmodel)] #list of model functions, get the ode function

    currentmodel = mbmodel #default is a single name for the underlying simulation function
    #if multiple versions of functions (ode/deterministic/stochastic) are run by the app, check them here
    if (modelsettings$modeltype == 'ode' & length(mbmodel)>1) {currentmodel = mbmodel[grep('_ode',mbmodel)]}
    if (modelsettings$modeltype == 'discrete' & length(mbmodel)>1) {currentmodel = mbmodel[grep('_discrete',mbmodel)]}
    if (modelsettings$modeltype == 'stochastic' & length(mbmodel)>1) {currentmodel = mbmodel[grep('_stochastic',mbmodel)]}
    #the generate_fctcall creates a function call to the specified model based on the given model settings
    #depending on if mbmodel is the name to a function or a modelbuilder object, different returns are produced
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = currentmodel)
    #browser()
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    result[[1]]$dat = simresult$ts
    #Meta-information for each plot
    #Might not want to hard-code here, can decide later
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"
  }


  ##################################
  #runs more than one dynamical model
  ##################################
  if (modelsettings$modeltype == 'ode_and_discrete')
  {
    #run ODE model
    modelsettings$modeltype = 'ode'
    currentmodel = mbmodel[grep('_ode',mbmodel)]
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = currentmodel)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    names(simresult$ts)[-1] = paste0(names(simresult$ts)[-1],'c') #label variables as continous
    result_ode = simresult #assign result to temp variable for combining below

    #run discrete model
    modelsettings$modeltype = 'discrete'
    currentmodel = mbmodel[grep('_discrete',mbmodel)]
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = currentmodel)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    names(simresult$ts)[-1] = paste0(names(simresult$ts)[-1],'d') #label variables as discrete
    result_disc = simresult #assign result to temp variable for combining below

    dat = rbind(tidyr::gather(result_ode$ts, -time, value = "yvals", key = "varnames"), tidyr::gather(result_disc$ts, -time, value = "yvals", key = "varnames"))
    result[[1]]$dat = dat
    #Meta-information for each plot
    #Might not want to hard-code here, can decide later
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"

  }




  ##################################
  #simulators that are not models themselves use this block
  ##################################
  if (modelsettings$modeltype == 'other')
  {
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, mbmodel = mbmodel)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    result[[1]]$dat = simresult$dat

    #Meta-information for each plot
    result[[1]]$plottype = "Scatterplot"
    result[[1]]$xlab = modelsettings$samplepar
    result[[1]]$ylab = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$linesize = 3

    #the 'other' functions need to provide infomration on maketext, showtext and finaltext
    result[[1]]$maketext = simresult$maketext
    result[[1]]$showtext = simresult$showtext
    result[[1]]$finaltext = simresult$finaltext

  }




  #save all results to a list for processing plots and text
  listlength = modelsettings$nplots
  #here we do all simulations in the same figure
  result = vector("list", listlength) #create empty list of right size for results

  for (n in 1:listlength) #loop over each plot
  {


    ##################################
    #default for text display, used by most basic simulation models
    #can/will be potentially overwritten below for specific types of models
    ##################################

    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if FALSE no result processing will occur inside generate_text
    #the following are for text display for each plot
    result[[1]]$showtext = '' #text can be added here which will be passed through to generate_text and displayed for each plot
    result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot
  }


  ##################################
  #additional settings for all types of simulators
  ##################################
  plotscale = modelsettings$plotscale

  #set min and max for scales. If not provided ggplot will auto-set
  #result[[1]]$ymin = 0.1
  #result[[1]]$ymax = max(simresult)
  #result[[1]]$xmin = 1e-12
  #result[[1]]$xmax = 9
  result[[1]]$dat = datall
  #Meta-information for each plot
  #Might not want to hard-code here, can decide later
  result[[1]]$plottype = "Lineplot"
  result[[1]]$xlab = "Time"
  result[[1]]$ylab = "Numbers"
  result[[1]]$legend = "Compartments"


  result[[1]]$xscale = 'identity'
  result[[1]]$yscale = 'identity'
  if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
  if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}



  return(result)
}
