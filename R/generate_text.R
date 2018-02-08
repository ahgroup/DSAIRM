#' @title A helper function that takes result from the simulators and produces text output
#'
#' @description This function generates text to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @param allres a list containing all simulation results.
#'    the length of the list indicates number of separate text blocks to make (one for each figure), each list entry contains the data is one plot and one set of text output
#'    for each list entry (e.g. allres[[i]]$type), 'type' specifies the kind of plot
#'    allres[[i]]$dat contains a dataframe in tidy/ggplot format for plotting. one column is called xvals, one column yvals, further columns are stratifiers/aesthetics, e.g. names of variables or number of run for a given variable
#' @return output a list with plot, text and warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export

generate_text <- function(input,output,allres)
{

  # Use the result returned from the simulator to compute some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderText({

    input$submitBtn

    res=isolate(allres())

    #nplots contains the number of plots to be produced.
    #for each plot, text output is produced separately
    nplots = length(res) #length of list

    alltext <- vector("list",nplots) #will hold all text outputs

    #each plot will be processed separately and text for each produced and placed in a list entry
    #using the same variable groupings as for the plots
    for (vn in 1:nplots)
    {
      #for each plot, get names of variables
      dat <- res[[vn]]$dat
      allvarnames = unique(dat$varnames)
      nvars = length(allvarnames)



      #for each plot, process each variable by looping over them
      for (nn in  1:nvars)
      {
        #data for a given variable
        currentvar = allvarnames[[nn]]
        vardat = dplyr::filter(dat, varnames == currentvar)
        #check if multiple runs are done
        #unless the data frame has a column indicating the number of runs, assume it's 1
        nreps = 1
        if ('nreps' %in% colnames(dat) ) {nreps=max(dat$nreps)}

        resmax = 0; resmin = 0; resfinal = 0;
        for (n1 in 1:nreps) #average over reps (if there are any)
        {
          #pull out each simulation/repetition
          currentsim = dplyr::filter(vardat, nreps == n1)
          nrows = nrow(currentsim) #number of entries in time-series matrix - can be different for every run

          resmax = resmax + max(currentsim$yvals)
          resmin = resmin + min(currentsim$yvals)
          resfinal = resfinal + currentsim$yvals[nrows]
        } #finish loop over reps

        #store values for each variable
        maxvals = round(resmax/nreps,2) #mean across simulations (for stochastic models)
        minvals = round(resmin/nreps,2) #mean across simulations (for stochastic models)
        numfinal = round(resfinal/nreps,2) #mean for each variable

        newtxt1 <- paste('Minimum and Maximum of ',currentvar,' during simulation: ',minvals,' and ', maxvals,sep='')
        newtxt2 <- paste('Number of ',currentvar,' at end of simulation: ',numfinal,sep='')
        if (nn == 1) {txt <- paste(newtxt1, newtxt2, sep = "<br/>")}
        if (nn > 1) {txt <- paste(txt, newtxt1, newtxt2, sep = "<br/>")}
      } #end loop over all variables
    alltext <- paste(alltext, txt, sep = "<hr>" ) #add text blocks together
    } #finishes loop over sets of variables

    finaltxt <- '<hr> <i> For stochastic simulation scenarios, values shown are the mean over all simulations. </i>'
    resulttxt <- paste(alltext, finaltxt, sep = "")
    HTML(resulttxt)
  }) #end text output

}
