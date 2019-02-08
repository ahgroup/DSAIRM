#' @title A function that runs a DSAIRM/DSAIDE app
#'
#' @description This function takes a model and model settings and runs it.
#' It runs the simulation determined by the model settings and returns simulation results.
#'
#' @param modelsettings a list with model settings. needs to contain list elements with names and values for all inputs expected by simulation function. Also needs to contain an element plotscale to indicate which axis should be on a log scale (x, y or both), a list element nplots to indicate number of plots that should be produced when calling the generate_plot function with the result, and a list element modeltype which specifies what kind of model should be run. Currently one of (_ode_, _discrete_, _stochastic_, _usanalysis_, _modelexploration_, _fit_ ). Stochastic models also need an nreps list entry to indicate numer of repeat simulations.
#' @param modelfunction The name of a simulation function to be run with the indicated settings.
#' @return A vectored list named "result" with each main list element containing the simulation results in a dataframe called dat and associated metadata required for generate_plot and generate_text functions. Most often there is only one main list entry (result[[1]]) for a single plot/text.
#' @details This function runs a model for specific settings. It is similar to analyze_model in the modelbuilder package.
#' @importFrom utils head tail
#' @importFrom stats reshape
#' @export

run_model <- function(modelsettings, modelfunction) {

  datall = NULL #will hold data for all different models and replicates
  finaltext = NULL

  ##################################
  #stochastic dynamical model execution
  ##################################
  if (grepl('_stochastic_',modelsettings$modeltype))
  {
    modelsettings$currentmodel = 'stochastic'
    currentmodel = modelfunction[grep('_stochastic',modelfunction)] #list of model functions, get the ode function
    noutbreaks = 0
    for (nn in 1:modelsettings$nreps)
    {
      #extract modesettings inputs needed for simulator function
      if (is.null(modelsettings$tmax) & !is.null(modelsettings$tfinal) )
      {
        modelsettings$tmax = modelsettings$tfinal
      }
      currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))]
      simresult <- do.call(currentmodel, args = currentargs)
      #data for plots and text
      #needs to be in the right format to be passed to generate_plots and generate_text
      #see documentation for those functions for details
      simresult <- simresult$ts
      colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
      #reformat data to be in the right format for plotting
      rawdat = as.data.frame(simresult)
      #using tidyr to reshape
      #dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
      #using basic reshape function to reformat data
      dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL
      dat$IDvar = paste(dat$varnames,nn,sep='') #make a variable for plotting same color lines for each run in ggplot2
      dat$nreps = nn
      datall = rbind(datall,dat)
      modelsettings$rngseed = modelsettings$rngseed + 1 #need to update RNG seed each time to get different runs
      #keep track of outbreaks occurence among stochastic simulations
      #assuming susceptible people/target cells are in 2nd slot, i.e. first variable
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
    modelsettings$currentmodel = 'ode'
    currentmodel = modelfunction[grep('_ode',modelfunction)] #list of model functions, get the ode function
    currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    simresult <- do.call(currentmodel, args = currentargs)

    simresult <- simresult$ts
    if (grepl('_and_',modelsettings$modeltype)) #this means ODE model is run with another one, relabel variables to indicate ODE
    {
      colnames(simresult) = paste0(colnames(simresult),'_ode')
    }
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    rawdat = as.data.frame(simresult)
    #using tidyr to reshape
    #dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
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
    modelsettings$currentmodel = 'discrete'
    currentmodel = modelfunction[grep('_discrete',modelfunction)] #list of model functions, get the ode function
    currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))]
    simresult <- do.call(currentmodel, args = currentargs)
    simresult <- simresult$ts
    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    rawdat = as.data.frame(simresult)
    #using tidyr to reshape
    #dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
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
  listlength = modelsettings$nplots
  #here we do all simulations in the same figure
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
  if (!is.null(datall))
  {
    result[[1]]$ymin = 0.1
    result[[1]]$ymax = max(datall$yvals) #max of all variables ignoring time
    result[[1]]$xmin = 1e-12
    result[[1]]$xmax = max(datall$xvals)
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
    modelsettings$currentmodel = 'other'
    currentmodel = modelfunction
    currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))]
    simresult <- do.call(currentmodel, args = currentargs)

    #pull the indicator for non-steady state out of the dataframe, process separately
    steady = simresult$dat$steady
    simresult$dat$steady <- NULL

    simdat = simresult$dat

    result <- vector("list", 24) #set up a list structure with as many elements as plots
    #loop over each outer list element corresponding to a plot and fill it with another list
    #of meta-data and data needed to create each plot
    #each parameter-output pair is its own plot, therefore its own list entry
    ct=1; #some counter
    result[[ct]]$ncol = 3 #number of columns for plot, needs to be stored in 1st sub-list element
    for (n in 1:8) #first loop over each parameter
    {
      for (nn in 1:3) #for each parameter, loop over outcomes
      {
        #data frame for each plot
        xvals = simdat[,3+n] #elements 4 to end end are parameters
        xvalname = colnames(simdat)[3+n]
        yvals = simdat[,nn] #first 3 elements are outcomes
        yvalname = colnames(simdat)[nn]
        dat = data.frame(xvals = xvals, yvals = yvals, varnames = yvalname)
        result[[ct]]$dat = dat

        #meta-data for each plot
        result[[ct]]$plottype = modelsettings$plottype
        result[[ct]]$xlab = xvalname
        result[[ct]]$ylab = yvalname
        result[[ct]]$legend = NULL #set to either false or provide the label for legends

        result[[ct]]$xscale = 'identity'
        result[[ct]]$yscale = 'identity'
        if (plotscale == 'x' | plotscale == 'both') { result[[ct]]$xscale = 'log10'}
        if (plotscale == 'y' | plotscale == 'both') { result[[ct]]$yscale = 'log10'}

        #the following are for text display for each plot
        result[[ct]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
        result[[ct]]$finaltext = paste("System might not have reached steady state", length(steady) - sum(steady), "times")

        ct = ct + 1
      } #inner loop
    } #outer loop

    #if we look at uncertainty/boxplots, we don't need results stratified by parameter
    #since all the plots and printout contain repeated information, we'll just retain the first 3 ones
    if (modelsettings$plottype == "Boxplot")
    {
      result <- result[c(1:3)]
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
    modelsettings$currentmodel = 'fit'
    currentmodel = modelfunction
    currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    simresult <- do.call(currentmodel, args = currentargs)
    colnames(simresult$timeseries)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    #each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratifications for each plot
    rawdat = as.data.frame(simresult$timeseries)
    #using tidyr to reshape
    #dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
    #using basic reshape function to reformat data
    dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL

        dat$style = 'line'

    #next, add data that's being fit to data frame
    fitdata  = simresult$data
    colnames(fitdata) = c('xvals','yvals')
    fitdata$varnames = 'Data'
    fitdata$yvals = 10^fitdata$yvals #data is in log units, for plotting transform it
    fitdata$style = 'point'
    alldat = rbind(dat,fitdata)

    #code variable names as factor and level them so they show up right in plot
    mylevels = unique(alldat$varnames)
    alldat$varnames = factor(alldat$varnames, levels = mylevels)

    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = alldat

    if (!is.null(datall))
    {
      result[[1]]$ymin = 0.1
      result[[1]]$ymax = max(datall$yvals) #max of all variables ignoring time
      result[[1]]$xmin = 1e-12
      result[[1]]$xmax = max(datall$xvals)
    }

    #Meta-information for each plot
    result[[1]]$plottype = "Mixedplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"


    result[[1]]$maketext = FALSE
    result[[1]]$showtext = NULL

    ####################################################
    #different choices for slightly different fit models
    #best fit results to be displayed as text
    #this is for basic fitting routine
    if (grepl('basicmodel_fit',modelfunction))
    {
      ssr = format(simresult$SSR, digits =2, nsmall = 2)
      pfinal = format(log10(simresult$bestpars[1]), digits =2, nsmall = 2)
      bfinal = format(log10(simresult$bestpars[2]), digits =2, nsmall = 2)
      dVfinal = format(simresult$bestpars[3], digits =2, nsmall = 2)

      txt1 <- paste('Best fit values for parameters 10^p / 10^b / dV are ', pfinal, ' / ' ,bfinal,  ' / ' , dVfinal)
      txt2 <- paste('Final SSR is ',ssr)
      result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")
    }

    #best fit results to be displayed as text
    #this is for confidence interval routine
    if (grepl('confint_fit',modelfunction))
    {
      ssr = format(simresult$SSR, digits =2, nsmall = 2)
      bfinal = format(log10(simresult$bestpars[1]), digits =2, nsmall = 2)
      blowfit = format(log10(simresult$confint[1]), digits =2, nsmall = 2)
      bhighfit = format(log10(simresult$confint[2]), digits =2, nsmall = 2)
      dVfinal = format(simresult$bestpars[2], digits =2, nsmall = 2)
      dVlowfit = format(simresult$confint[3], digits =2, nsmall = 2)
      dVhighfit = format(simresult$confint[4], digits =2, nsmall = 2)

      txt1 <- paste('Best fit values for parameters 10^b and dV are ',bfinal,' and ',dVfinal)
      txt2 <- paste('Lower and upper bounds for 10^b are ',blowfit,' and ',bhighfit)
      txt3 <- paste('Lower and upper bounds for dV are ',dVlowfit,' and ',dVhighfit)
      txt4 <- paste('SSR is ',ssr)

      result[[1]]$finaltext = paste(txt1,txt2,txt3,txt4, sep = "<br/>")
    }


    #best fit results to be displayed as text
    #this is for model comparison fit  routine
    if (grepl('modelcomparison_fit',modelfunction))
    {

      #store values for each variable
      aicc = format(simresult$AICc, digits =2, nsmall = 2)
      ssr = format(simresult$SSR, digits =2, nsmall = 2)
      afinal = format(log10(simresult$bestpars[1]), digits =2, nsmall = 2)
      bfinal = format(log10(simresult$bestpars[3]), digits =2, nsmall = 2)
      r_or_dXfinal = format(simresult$bestpars[2], digits =2, nsmall = 2)

      if (modelsettings$fitmodel == 1)
      {
        txt1 <- paste('Best fit values for model 1 parameters 10^a / 10^b / r  are ',afinal,'/',bfinal,'/',r_or_dXfinal)
      }
      if (modelsettings$fitmodel == 2)
      {
        txt1 <- paste('Best fit values for model 2 parameters 10^a / 10^b / dX are ',afinal,'/',bfinal,'/',r_or_dXfinal)
      }

      txt2 <- paste('SSR and AICc are ',ssr,' and ',aicc)

      result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")
    }

  }
  ##################################
  #end model fitting code block
  ##################################


  ##################################
  #model exploration code block
  ##################################
  if (grepl('modelexploration',modelsettings$modeltype))
  {
    currentmodel = modelfunction
    currentargs = modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    simresult <- do.call(currentmodel, args = currentargs)

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

