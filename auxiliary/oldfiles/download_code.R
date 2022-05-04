#' @title A function that downloads simulator code
#'
#' @description This function is called when the
#' user clicks the "Download Code" button in
#' the simulator app. It creates an R file which
#' will run the simulation specified in the
#' app from which the button is clicked, and
#' after running the simulation, returns the
#' plot and text created by generate_plots()
#' and generate_text().
#'
#' @param modelsettings A list with model settings.
#' #' Needs to contain list elements with names and values for all inputs expected 
#' by simulation function. Required are:
#' Name of simulation function in variable modelsettings$simfunction
#' Also needs to contain an element plotscale 
#' to indicate which axis should be on a log scale (x, y or both), 
#' a list element nplots to indicate number of plots that should be produced
#' when calling the generate_plot function with the result, 
#' and a list element modeltype which specifies what kind of model should be run. 
#' Currently one of (_ode_, _discrete_, _stochastic_, _usanalysis_, _modelexploration_, _fit_ ). Stochastic models also need an nreps list entry to indicate numer of repeat simulations.
#' @param modelfunction The name of the simulator function being called. The name
#' must be one of the simulator functions in the DSAIDE package.
#' 
#' @return Creates an R script that runs the simulation specified in the app
#' and returns the text and plots created by the simulation.
#' @export

download_code <- function(modelsettings, modelfunction) {
  # Opening lines
  opening_lines <- paste("datall = NULL",
                         "finaltext = NULL",
                         "library(DSAIDE)")
  
  # Option if model is ODE
  if (grepl("_ode_", modelsettings$modeltype)) {
    currentmodel <- modelfunction[grep('_ode',modelfunction)] #list of model functions, get the ode function
    currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(modelsettings))]
    # currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    # args_in_order <- lapply(1:length(currentargs),
    #                         function(i) paste(names(currentargs[i]), "=",
    #                                           currentargs[[i]])) %>%
    #   unlist(.) %>%
    #   paste(., collapse = ", ")
    
    args_in_order <- unlist(lapply(1:length(currentargs),
                            function(i) paste(names(currentargs[i]), "=",
                                              currentargs[[i]])))
    args_in_order <- paste(args_in_order, collapse = ", ")
    
    model_lines <- paste(paste0("tfinal <- ",
                                "\"",
                                modelsettings$tfinal,
                                "\""),
                          paste0("simresult <- ",
                                currentmodel, "(", args_in_order, ")"),
                         "simresult <- simresult$ts",
                         "if (grepl('_and_',modeltype))",
                         "{",
                         "colnames(simresult) = paste0(colnames(simresult),'_ode')",
                         "}",
                         "colnames(simresult)[1] = 'xvals'",
                         "rawdat = as.data.frame(simresult)",
                         "dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = 'varnames', times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL",
                         "dat$IDvar = dat$varnames",
                         "dat$nreps = 1",
                         "datall = rbind(datall,dat)",
                         sep = "\n")
  }
  
  # Option if the model is stochastic
  else if (grepl('_stochastic_',modelsettings$modeltype)) {
    modelsettings$currentmodel <- 'stochastic'
    if (is.null(modelsettings$tmax) & !is.null(modelsettings$tfinal)) {
      modelsettings$tmax <- modelsettings$tfinal
    }
    currentmodel <- modelfunction[grep('_stochastic',modelfunction)]
    currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(modelsettings))] #extract modesettings inputs needed for simulator function
    # currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(unlist(modelsettings)))] #extract modesettings inputs needed for simulator function
    # args_in_order <- lapply(1:length(currentargs),
    #                         function(i) paste(names(currentargs[i]), "=",
    #                                           currentargs[[i]])) %>%
    #   unlist(.) %>%
    #   paste(., collapse = ", ")
    
    args_in_order <- unlist(lapply(1:length(currentargs),
                                   function(i) paste(names(currentargs[i]), "=",
                                                     currentargs[[i]])))
    args_in_order <- paste(args_in_order, collapse = ", ")
    
    noutbreaks <- 0
    
    model_lines <- paste(
      "for (nn in 1:nreps)",
      "{",
      paste0("simresult <- ",
             currentmodel, "(", args_in_order, ")"),
      "simresult <- simresult$ts",
      "colnames(simresult)[1] = 'xvals'",
      "rawdat = as.data.frame(simresult)",
      "dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = 'varnames', times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL); dat$id <- NULL",
      "dat$IDvar = paste(dat$varnames,nn,sep='')",
      "dat$nreps = nn",
      "datall = rbind(datall, dat)",
      "rngseed = rngseed + 1",
      "S0=head(simresult[,2],1)",
      "Sfinal=tail(simresult[,2],1)",
      "if ( (S0-Sfinal)/S0>0.2 ) {noutbreaks = noutbreaks + 1}",
      "}",
      "finaltext = paste('For stochastic simulation scenarios, values shown are the mean over all simulations.', noutbreaks,' simulations produced an outbreak (susceptible/uninfected dropped by at least 20%)')",
      "}",
      sep = "\n")
  }
  
  # Option if the model is uncertainty and sensitivity analysis
  else if (grepl('_usanalysis_',modelsettings$modeltype)) {
    modelsettings$currentmodel <- 'other'
    currentmodel <- modelfunction
    currentargs <- modelsettings[match(names(unlist(formals(currentmodel))), names(modelsettings))] #extract modesettings inputs needed for simulator function
    args_in_order <- unlist(lapply(1:length(currentargs),
                                   function(i) paste(names(currentargs[i]), "=",
                                                     currentargs[[i]])))
    args_in_order <- paste(args_in_order, collapse = ", ")
    
    model_lines <- paste(
      paste0("modelsettings <- ", modelsettings),
      "#short function to call/run model",
      "runsimulation <- function(modelsettings, currentmodel)",
      "{",
      "#match values provided from UI with those expected by function",
      "settingsvec = unlist(modelsettings)",
      "currentargs = settingsvec[match(names(unlist(formals(currentmodel))), names(settingsvec))]",
      "#make a list",
      "arglist = as.list(currentargs)",
      "#convert arguments for function call to numeric if possible",
      "#preserve those that can't be converted",
      "numind = suppressWarnings(!is.na(as.numeric(arglist))) #find numeric values",
      "arglist[numind] = as.numeric(currentargs[numind])",
      "#run simulation, try command catches error from running code.",
      "simresult <- try( do.call(currentmodel, args = arglist ) )",
      "return(simresult)",
      "}",
      "\n",
      paste0("simresult <- runsimulation(modelsettings, \"",
             currentmodel, "\")"),
      "# if error occurs we exit",
      "if (class(simresult)!=\"list\")", 
      "{",
      "result <- 'Model run failed. Maybe unreasonable parameter values?'",
      "return(result)",
      "}",
      "#pull the indicator for non-steady state out of the dataframe, process separately",
      "steady = simresult$dat$steady",
      "simresult$dat$steady <- NULL", 
      "simdat = simresult$dat",
      "#number of columns - each outcome gets a column",
      "result[[1]]$ncols = modelsettings$ncols",
      "#loop over each outer list element corresponding to a plot and fill it with another list",
      "#of meta-data and data needed to create each plot",
      "#each parameter-output pair is its own plot, therefore its own list entry",
      "ct=1; #some counter",
      "for (nn in 1:modelsettings$nplots) #for specified parameter, loop over outcomes",
      "{",
      "#data frame for each plot",
      "xvals = simdat[,modelsettings$samplepar] #get parameter under consideration",
      "xvalname = modelsettings$samplepar",
      "yvals = simdat[,nn] #first 3 elements are outcomes",
      "yvalname = colnames(simdat)[nn]",
      "dat = data.frame(xvals = xvals, yvals = yvals, varnames = yvalname)",
      "result[[ct]]$dat = dat",
      "#meta-data for each plot",
      "result[[ct]]$plottype = modelsettings$plottype",
      "result[[ct]]$xlab = xvalname",
      "result[[ct]]$ylab = yvalname",
      "result[[ct]]$makelegend = FALSE #no legend for these plots",
      "result[[ct]]$xscale = 'identity'",
      "result[[ct]]$yscale = 'identity'",
      "if (plotscale == 'x' | plotscale == 'both') { result[[ct]]$xscale = 'log10'}",
      "if (plotscale == 'y' | plotscale == 'both') { result[[ct]]$yscale = 'log10'}",
      "#the following are for text display for each plot",
      "result[[ct]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text",
      "result[[ct]]$finaltext = paste(\"System might not have reached steady state\", length(steady) - sum(steady), \"times\")",
      "ct = ct + 1",
      "} #loop over plots",
      sep = "\n"
    )
  }
  
  # Final plotting stuff
  closing_lines <- paste(
    "listlength = nplots",
    "result = vector('list', listlength)",
    "result[[1]]$maketext = TRUE",
    "result[[1]]$showtext = NULL",
    "result[[1]]$finaltext = paste0('Numbers are rounded to 2 significant digits. ',finaltext)",
    "result[[1]]$dat = datall",
    "if (!is.null(datall))",
    "{",
    "result[[1]]$ymin = 0.1",
    "result[[1]]$ymax = max(datall$yvals)",
    "result[[1]]$xmin = 1e-12",
    "result[[1]]$xmax = max(datall$xvals)",
    "}",
    "result[[1]]$plottype = 'Lineplot'",
    "result[[1]]$xlab = 'Time'",
    "result[[1]]$ylab = 'Numbers'",
    "result[[1]]$legend = 'Compartments'",
    "result[[1]]$xscale = 'identity'",
    "result[[1]]$yscale = 'identity'",
    "if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}",
    "if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}",
    "if (plotengine == \"ggplot\") {generate_ggplot(result)}",
    "if (plotengine == \"plotly\") {generate_plotly(result)}",
    "generate_text(result)",
    sep = "\n")
  
  # Writing to file
  output_text <- paste(opening_lines, model_lines, closing_lines, sep = "\n")
  return(output_text)
}
