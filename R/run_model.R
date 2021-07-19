#' @title A function that runs an app for specific settings and processes results for plot and text generation
#'
#' @description This function runs a model based on information
#' provided in the modelsettings list passed into it.
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

run_model <- function(modelsettings) {

  #check if a simresult function ran ok
  #if error occurs we exit run_model function
  check_results <- function(simresult)
  {
    checkres = NULL
    if (class(simresult)!="list") #if the return from the simulator function is not a list, something went wrong
    {
      checkres <- 'Model run failed. Maybe unreasonable parameter values?'
      return(checkres)
    }
    #if simeresult is a list, check that no values in time-series are NaN or NA or Inf
    if (!is.null(simresult$ts))
    {
      if (   (sum(is.nan(unlist(simresult$ts)))>0) || (sum(is.na(unlist(simresult$ts)))>0) || (sum(is.infinite(unlist(simresult$ts)))>0) )
      {
        checkres <- 'Model run failed. Maybe unreasonable parameter values?'
        return(checkres)
      }
    }
    return(checkres)
  }


  #check to make sure inputs to function provide information needed for code to run
  if (is.null(modelsettings$simfunction)) { return("List element simfunction must be provided.") }
  if (is.null(modelsettings$modeltype)) { return("List element modeltype must be provided.") }



  #if the user sets the model type, apply that choice
  #that happens for any models that have an "_and_" in their modeltype variable as defined in the apptable.tsv spreadsheet
  if (grepl('_and_',modelsettings$modeltype))
  {
    modelsettings$modeltype = modelsettings$modeltypeUI
  }

  datall = NULL #will hold data for all different models and replicates
  finaltext = NULL
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
      #extract modelsettings inputs needed for simulator function

      #all models should be using tfinal so turn this off
      #if (is.null(modelsettings$tmax) & !is.null(modelsettings$tfinal) )
      #{
        #modelsettings$tmax = modelsettings$tfinal
      #}

      #create function call, then evaluate it to run model
      fctcall = generate_fctcall(modelsettings)
      # this means an error occurred making the call
      if (!is.call(fctcall))
      {
        #return error message generated when trying to build the function call
        return(fctcall)
      }
      #wrap in try command to catch errors
      #send result from simulator to a check function. If that function does not return null, exit run_model with error message
      simresult = try(eval(fctcall))
      checkres <- check_results(simresult)
      if (!is.null(checkres)) {return(checkres)}

      #data for plots and text
      #needs to be in the right format to be passed to generate_plots and generate_text
      #see documentation for those functions for details
      simresult <- simresult$ts
      if (grepl('_and_',modelsettings$modeltype)) #this means  model is run with another one, relabel variables to indicate stochastic
      {
        colnames(simresult) = paste0(colnames(simresult),'_sto')
      }
      colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
      #reformat data to be in the right format for plotting
      rawdat = as.data.frame(simresult)
      #using basic reshape function to reformat data
      dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL
      dat$IDvar = paste(dat$varnames,nn,sep='') #make a variable for plotting same color lines for each run in ggplot2
      dat$nreps = nn
      datall = rbind(datall,dat)
      modelsettings$rngseed = modelsettings$rngseed + 1 #need to update RNG seed each time to get different runs
      #keep track of outbreaks occurence among stochastic simulations
      S0=head(simresult[,2],1)
      Sfinal=tail(simresult[,2],1)
      if ( (S0-Sfinal)/S0>0.2 ) {noutbreaks = noutbreaks + 1}
    }
    finaltext = paste('For stochastic simulation scenarios, values shown are the mean over all simulations.', noutbreaks,' simulations produced an outbreak (susceptible/uninfected dropped by at least 20%)')
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
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
    #run model
    simresult = try(eval(fctcall))
    checkres <- check_results(simresult)
    if (!is.null(checkres)) {return(checkres)}

    simresult <- simresult$ts
    if (grepl('_and_',modelsettings$modeltype)) #this means ODE model is run with another one, relabel variables to indicate ODE
    {
      colnames(simresult) = paste0(colnames(simresult),'_ode')
    }
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    rawdat = as.data.frame(simresult)
     #using basic reshape function to reformat data
    dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL

    dat$IDvar = dat$varnames #make variables in case data is combined with stochastic runs. not used for ode.
    dat$nreps = 1
    datall = rbind(datall,dat)
  }


  ##################################
  #discrete dynamical model execution
  ##################################
  if (grepl('_discrete_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = simfunction[grep('_discrete',simfunction)] #list of model functions, get the ode function

    #create function call, then evaluate it to run model
    fctcall = generate_fctcall(modelsettings)
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
    #wrap in try command to catch errors
    #send result from simulator to a check function. If that function does not return null, exit run_model with error message
    simresult = try(eval(fctcall))

    checkres <- check_results(simresult)
    if (!is.null(checkres)) {return(checkres)}

    simresult <- simresult$ts
    if (grepl('_and_',modelsettings$modeltype)) #this means  model is run with another one, relabel variables to indicate discrete
    {
      colnames(simresult) = paste0(colnames(simresult),'_dis')
    }
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    rawdat = as.data.frame(simresult)
    #using basic reshape function to reformat data
    dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL
    dat$IDvar = dat$varnames #make variables in case data is combined with stochastic runs. not used for discrete.
    dat$nreps = 1
    datall = rbind(datall,dat)
  }

  ##################################
  #take data from all simulations and turn into list structure format
  #needed to generate plots and text
  #this applies to simulators that run dynamical models
  #other simulation functions need output processed differently and will overwrite some of these settings
  #each other simulator function has its own code block below
  ##################################

  #save all results to a list for processing plots and text
  listlength = ifelse(is.null(modelsettings$nplots),1,modelsettings$nplots)
  result = vector("list", listlength) #create empty list of right size for results

  ##################################
  #default for text display, used by most basic simulation models
  #can/will be potentially overwritten below for specific types of models
  ##################################

  result[[1]]$maketext = TRUE #indicate if we want the generate_text function to process data and generate text
  result[[1]]$showtext = NULL #text can be added here which will be passed through to generate_text and displayed for EACH plot
  result[[1]]$finaltext = paste0('Numbers are rounded to 2 significant digits. ',finaltext) #text can be added here which will be passed through to generate_text and displayed once


  ##################################
  #additional settings for all types of simulators
  ##################################

  result[[1]]$dat = datall

  #set min and max for scales. If not provided ggplot will auto-set
  #if (!is.null(datall))
  #{
   # result[[1]]$ymin = 0.1
  #  result[[1]]$ymax = max(datall$yvals) #max of all variables ignoring time
  #  result[[1]]$xmin = 1e-12
  #  result[[1]]$xmax = max(datall$xvals)
  #}

  #Meta-information for each plot
  #Might not want to hard-code here, can decide later
  result[[1]]$plottype = "Lineplot"
  result[[1]]$xlab = "Time"
  result[[1]]$ylab = "Numbers"
  result[[1]]$legend = "Compartments"

  #if plotscale is not provided, assume no log scales for x and y, i.e. set to ''
  plotscale = ifelse(is.null(modelsettings$plotscale),'',modelsettings$plotscale)
  result[[1]]$xscale = 'identity'
  result[[1]]$yscale = 'identity'
  if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
  if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}


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
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
    #wrap in try command to catch errors
    #send result from simulator to a check function. If that function does not return null, exit run_model with error message
    simresult = try(eval(fctcall))


    checkres <- check_results(simresult)
    if (!is.null(checkres)) {return(checkres)}

    #pull the indicator for non-steady state out of the dataframe, process separately
    steady = simresult$dat$steady
    simresult$dat$steady <- NULL
    simdat = simresult$dat

    #number of columns - each outcome gets a column
    result[[1]]$ncols = modelsettings$nplots

    #loop over each outer list element corresponding to a plot and fill it with another list
    #of meta-data and data needed to create each plot
    #each parameter-output pair is its own plot, therefore its own list entry
    ct=1; #some counter
    for (nn in 1:modelsettings$nplots) #for specified parameter, loop over outcomes
      {
        #data frame for each plot
        xvals = simdat[,modelsettings$samplepar] #get parameter under consideration
        xvalname = modelsettings$samplepar
        yvals = simdat[,nn] #first 3 elements are outcomes
        yvalname = colnames(simdat)[nn]
        dat = data.frame(xvals = xvals, yvals = yvals, varnames = yvalname)
        result[[ct]]$dat = dat

        #meta-data for each plot
        result[[ct]]$plottype = modelsettings$plottype
        result[[ct]]$xlab = xvalname
        result[[ct]]$ylab = yvalname
        result[[ct]]$makelegend = FALSE #no legend for these plots

        #if plotscale is not provided, assume no log scales for x and y, i.e. set to ''
        plotscale = ifelse(is.null(modelsettings$plotscale),'',modelsettings$plotscale)

        result[[ct]]$xscale = 'identity'
        result[[ct]]$yscale = 'identity'
        if (plotscale == 'x' | plotscale == 'both') { result[[ct]]$xscale = 'log10'}
        if (plotscale == 'y' | plotscale == 'both') { result[[ct]]$yscale = 'log10'}

        #the following are for text display for each plot
        result[[ct]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur inside generate_text
        result[[ct]]$finaltext = paste("System might not have reached steady state", length(steady) - sum(steady), "times")

        #set y-axis limits based on variable
        result[[ct]]$ymin = min(result[[ct]]$dat$yvals, na.rm=TRUE)
        result[[ct]]$ymax = max(result[[ct]]$dat$yvals, na.rm=TRUE)

        ct = ct + 1
    } #loop over plots
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
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
    #wrap in try command to catch errors
    #send result from simulator to a check function. If that function does not return null, exit run_model with error message
    simresult = try(eval(fctcall))

    checkres <- check_results(simresult)
    if (!is.null(checkres)) {return(checkres)}


    colnames(simresult$ts)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    #each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratification for each plot
    rawdat = as.data.frame(simresult$ts)
    #using basic reshape function to reformat data
    dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL

    dat$style = 'line'

    #next, add data that's being fit to data frame
    fitdata  = simresult$data
    fitdata$style = 'point'
    datall = rbind(dat,fitdata)

    #code variable names as factor and level them so they show up right in plot
    mylevels = unique(datall$varnames)
    datall$varnames = factor(datall$varnames, levels = mylevels)

    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = datall


    #Meta-information for each plot
    result[[1]]$plottype = "Mixedplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"

    result[[1]]$maketext = FALSE
    result[[1]]$showtext = NULL

    ####################################################
    #different choices for text display for different fit models
    #both DSAIDE and DSAIRM models
    if (grepl('flu_fit',simfunction) || grepl('basicvirus_fit',simfunction))
    {
      txt1 <- paste('Best fit values for parameters',paste(names(simresult$bestpars), collapse = '/'), ' are ', paste(format(simresult$bestpars,  digits =2, nsmall = 2), collapse = '/' ))
      txt2 <- paste('Final SSR is ', format(simresult$SSR, digits =2, nsmall = 2))
      result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")
    }
    if (grepl('confint_fit',simfunction))
    {
      txt1 <- paste('Best fit values for parameters', paste(names(simresult$bestpars), collapse = '/'), ' are ', paste(format(simresult$bestpars,  digits =2, nsmall = 2), collapse = '/' ))
      txt2 <- paste('Lower and upper bounds for parameter', paste(names(simresult$bestpars[1]), collapse = '/'), ' are ', paste(format(simresult$confint[1:2],  digits =2, nsmall = 2), collapse = '/' ))
      txt3 <- paste('Lower and upper bounds for parameter', paste(names(simresult$bestpars[2]), collapse = '/'), ' are ', paste(format(simresult$confint[3:4],  digits =2, nsmall = 2), collapse = '/' ))
      txt4 <- paste('SSR is ', format(simresult$SSR, digits =2, nsmall = 2))
      result[[1]]$finaltext = paste(txt1,txt2,txt3,txt4, sep = "<br/>")
    }
    if (grepl('noro_fit',simfunction) || grepl('fludrug_fit',simfunction) || grepl('modelcomparison_fit',simfunction))
    {
      txt1 <- paste('Best fit values for model', modelsettings$fitmodel, 'parameters',paste(names(simresult$bestpars), collapse = '/'), ' are ', paste(format(simresult$bestpars,  digits =2, nsmall = 2), collapse = '/' ))
      txt2 <- paste('SSR and AICc are ',format(simresult$SSR, digits =2, nsmall = 2),' and ',format(simresult$AICc, digits =2, nsmall = 2))
      result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")
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
    # this means an error occurred making the call
    if (!is.call(fctcall))
    {
      #return error message generated when trying to build the function call
      return(fctcall)
    }
    #wrap in try command to catch errors
    #send result from simulator to a check function. If that function does not return null, exit run_model with error message
    simresult = try(eval(fctcall))

    checkres <- check_results(simresult)
    if (!is.null(checkres)) {return(checkres)}

    steady = simresult$dat$steady

    #these 3 settings are only needed for the shiny UI presentation
    result[[1]]$maketext = FALSE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$showtext = NULL #text for each plot can be added here which will be passed through to generate_text and displayed for each plot
    result[[1]]$finaltext = paste("System might not have reached steady state", length(steady) - sum(steady), "times")

    #Meta-information for each plot
    result[[1]]$plottype = "Scatterplot"
    result[[1]]$xlab = modelsettings$samplepar
    result[[1]]$ylab = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$linesize = 3

    simresult$dat$steady <- NULL #remove before return so it won't be plotted
    result[[1]]$dat = simresult$dat
  }
  ##################################
  #end model exploration code block
  ##################################

  #return result structure to calling function (app.R)
  #results need to be in a form that they
  #can be sent to the plot and text functions to generate results
  return(result)

}

