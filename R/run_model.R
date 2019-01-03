#' @title A function that runs a DSAIRM/DSAIDE app
#'
#' @description This function takes a model and model settings and runs it.
#' It runs the simulation determined by the model settings and returns simulation results.
#'
#' @param modelsettings vector of model settings. needs to contain inputs expected by simulation function. Also needs to provide further information.
#' @param modelfunction The name of a simulation function to be run with the indicated settings.
#' @return A list named "result" with the simulated dataframe and associated metadata.
#' @details This function runs a model for specific settings. It is similar to analyze_model in the modelbuilder package.
#' @author Andreas Handel
#' @export

run_model <- function(modelsettings, modelfunction) {

  set.seed(modelsettings$rngseed) #set RNG seed specified by the settings before executing function call

  datall = NULL #will hold data for all different models and replicates
  #ct = 1 #counter to keep track of number of simulations that ran

  ##################################
  #stochastic dynamical model execution
  ##################################
  if (grepl('_stochastic_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = 'stochastic'
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = modelfunction)
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
    #ct = nn + 1 #increase counter that keeps track of number of model runs
  }

  ##################################
  #ode dynamical model execution
  ##################################
  if (grepl('_ode_',modelsettings$modeltype)) #need to always start with ode_ in model specification
  {

    modelsettings$currentmodel = 'ode'
    currentmodel = modelfunction[grep('_ode',modelfunction)] #list of model functions, get the ode function
    #the generate_fctcall creates a function call to the specified model based on the given model settings
    #depending on if modelfunction is the name to a function or a modelbuilder object, different returns are produced
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = currentmodel)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    simresult <- simresult$ts
    if (grepl('_and_',modelsettings$modeltype)) #this means ODE model is run with another one, relabel variables to indicate ODE
    {
      colnames(simresult) = paste0(colnames(simresult),'_ode')
    }
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
    dat$IDvar = dat$varnames #make variables in case data is combined with stochastic runs. not used for ode.
    dat$nreps = 1
    datall = rbind(datall,dat)
    #ct = ct + 1
  }


  ##################################
  #discrete dynamical model execution
  ##################################
  if (grepl('_discrete_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = 'discrete'
    currentmodel = modelfunction[grep('_discrete',modelfunction)] #list of model functions, get the ode function
    #the generate_fctcall creates a function call to the specified model based on the given model settings
    #depending on if modelfunction is the name to a function or a modelbuilder object, different returns are produced
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = currentmodel)
    #browser()
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object
    simresult <- simresult$ts
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
    dat$IDvar = dat$varnames #make variables in case data is combined with stochastic runs. not used for discrete.
    dat$nreps = 1
    datall = rbind(datall,dat)
    #ct = ct + 1
  }

  ##################################
  #take data from all simulations and process into list structure format
  #needed to generate plots and text
  #this applies to simulators that run dynamical models
  #other simulation functions need output processed differently and will overwrite some of these settings
  #each other simulator function has its own code block below
  ##################################

  #save all results to a list for processing plots and text
  listlength = modelsettings$nplots
  #here we do all simulations in the same figure
  result = vector("list", listlength) #create empty list of right size for results

  ##################################
  #default for text display, used by most basic simulation models
  #can/will be potentially overwritten below for specific types of models
  ##################################

  result[[1]]$maketext = TRUE #indicate if we want the generate_text function to process data and generate text
  result[[1]]$showtext = NULL #text can be added here which will be passed through to generate_text and displayed for EACH plot
  result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed once

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


  ##################################
  #simulators that are not models themselves use their own blocks
  ##################################


  if (grepl('usanalysis',modelsettings$modeltype))
  {
    modelsettings$currentmodel = 'other'
    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = modelfunction)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object

    #Meta-information for each plot
    result[[1]]$plottype = "Scatterplot"
    result[[1]]$xlab = modelsettings$samplepar
    result[[1]]$ylab = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$linesize = 3

  }


  if (grepl('_fit_',modelsettings$modeltype))
  {

    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = modelfunction)
    eval(parse(text = fctcall)) #execute function, result is returned in 'simresult' object


    colnames(simresult$timeseries)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    #each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratifications for each plot
    dat = tidyr::gather(as.data.frame(simresult$timeseries), -xvals, value = "yvals", key = "varnames")
    dat$style = 'line'

    #next, add data that's being fit to data frame
    fitdata  = simresult$data
    colnames(fitdata) = c('xvals','yvals')
    fitdata$varnames = 'Data'
    fitdata$yvals = 10^fitdata$yvals #data is in log units, for plotting transform it
    fitdata$style = 'point'
    dat = rbind(dat,fitdata)

    #code variable names as factor and level them so they show up right in plot
    mylevels = unique(dat$varnames)
    dat$varnames = factor(dat$varnames, levels = mylevels)

    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = dat

    #Meta-information for each plot
    result[[1]]$plottype = "Mixedplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"
    #set min and max for scales. If not provided ggplot will auto-set
    #result[[1]]$ymin = 0.1
    #result[[1]]$ymax = max(simresult)
    #result[[1]]$xmin = 1e-12
    #result[[1]]$xmax = 9

    #best fit results to be displayed as text
    ssr = format(simresult$SSR, digits =2, nsmall = 2)
    pfinal = format(log10(simresult$bestpars[1]), digits =2, nsmall = 2)
    bfinal = format(log10(simresult$bestpars[2]), digits =2, nsmall = 2)
    dVfinal = format(simresult$bestpars[3], digits =2, nsmall = 2)

    txt1 <- paste('Best fit values for parameters 10^p / 10^b / dV are ', pfinal, ' / ' ,bfinal,  ' / ' , dVfinal)
    txt2 <- paste('Final SSR is ',ssr)

    result[[1]]$maketext = FALSE
    result[[1]]$showtext = NULL
    result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")

  }




  if (grepl('modelexploration',modelsettings$modeltype))
  {

    fctcall <- DSAIRM::generate_fctcall(modelsettings = modelsettings, modelfunction = modelfunction)
    eval(parse(text = fctcall)) #execute function, result is returned in 'result' object

    #these 3 settings are only needed for the shiny UI presentation
    result[[1]]$maketext = FALSE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$showtext = '' #text for each plot can be added here which will be passed through to generate_text and displayed for each plot
    result[[1]]$finaltext = paste("System might not have reached steady state", sum(result$dat$nosteady), "times")

    #Meta-information for each plot
    result[[1]]$plottype = "Scatterplot"
    result[[1]]$xlab = modelsettings$samplepar
    result[[1]]$ylab = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$legend = "Outcomes"
    result[[1]]$linesize = 3

    simresult$dat$nosteady <- NULL #remove before return so it won't be plotted
    result[[1]]$dat = simresult$dat
  }




  return(result)

}

